/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(swish_render_graphviz,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(process)).
:- use_module(library(sgml)).
:- use_module(library(dcg/basics)).
:- use_module('../render').

:- register_renderer(graphviz, "Render data using graphviz").

/** <module> Render data using graphviz

This renderer exploits  [graphviz](http://www.graphviz.org)   to  render
graphs from Prolog data. It takes two   representations.  The first is a
straightforward term Program(String), e.g.,

  ```
  dot("digraph G {Hello->World}")
  ```

The   second   takes   a   Prolog    term     as    input.    The   [dot
language](http://www.graphviz.org/content/dot-language)  is  represented
as follows:

  ```
  Graph      := graph(Statements)
              | graph(Options, Statements)
	      | digraph(Statements)
	      | digraph(Options, Statements)
  Options    := ID | [ID] | [strict, ID]
  Statements := List of statements
  Statement  := NodeStm | EdgeStm | AttrStm | ID = ID | SubGraph
  NodeStm    := NodeID | node(NodeID, AttrList)
  NodeID     := ID | ID:Port | ID:Port:CompassPT
  CompassPT  := n | ne | e | se | s | sw | w | nw | c | _
  EdgeStm    := (NodeID|SubGraph) (EdgeOp (NodeID|SubGraph))+
  EdgeStm     | edge(NodeID|SubGraph) (EdgeOp (NodeID|SubGraph))+), AttrList)
  EdgeOp     := - | ->
  SubGraph   := subgraph(ID, Statements)
  ```
*/

:- http_handler(swish(graphviz), swish_send_graphviz, []).

:- dynamic
	dot_data/3.				% +Hash, +Data, +Time

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders data using graphviz.

term_rendering(Data, _Vars, Options) -->
	{ data_to_graphviz_string(Data, DOTString, Program)
	},
	render_dot(DOTString, Program, Options).

render_dot(_DOTString, Program, _Options) -->
	{ \+ has_graphviz_renderer(Program) }, !,
	no_graph_viz(Program).
render_dot(DOTString, Program, _Options) -->
	{ variant_sha1(DOTString, Hash),
	  get_time(Now),
	  assert(dot_data(Hash,
			  _{ program: Program,
			     dot: DOTString
			   }, Now)),
	  remove_old_data(Now),
	  http_link_to_id(swish_send_graphviz,
			  [ hash(Hash),
			    lang(svg),
			    target('_top')
			  ], HREF),
	  Attrs = []				% TBD
	}, !,
	html([ object([ data(HREF),
			type('image/svg+xml')
		      | Attrs
		      ],
		      [])
	     ]).

%%	data_to_graphviz_string(+Data, -DOTString, -Program) is semidet.

data_to_graphviz_string(Compound, String, Program) :-
	compound(Compound),
	compound_name_arguments(Compound, Program, [Data]),
	graphviz_program(Program),
	(   atomic(Data)
	->  String = Data
	;   phrase(graph(Data), Codes),
	    string_codes(String, Codes)
	).

graphviz_program(dot).
graphviz_program(neato).
graphviz_program(fdp).
graphviz_program(sfdp).
graphviz_program(twopi).
graphviz_program(circo).

%%	swish_send_graphviz(+Request)
%
%	HTTP handler to send a GraphViz graph

swish_send_graphviz(Request) :-
	http_parameters(Request,
			[ hash(Hash,
			       [ description('Hash-key to the graph-data')
			       ])
			]),
	dot_data(Hash, Data, _),
	process_create(path(Data.program), ['-Tsvg'],
		       [ stdin(pipe(ToDOT)),
			 stdout(pipe(XDotOut)),
			 process(PID)
		       ]),
	set_stream(ToDOT, encoding(utf8)),
	set_stream(XDotOut, encoding(utf8)),
	thread_create(send_to_dot(Data.dot, ToDOT), _,
		      [ detached(true) ]),
	call_cleanup(load_structure(stream(XDotOut),
				    SVGDom0,
				    [ dialect(xml) ]),
		     (	 process_wait(PID, _Status),
			 close(XDotOut)
		     )),
	rewrite_sgv_dom(SVGDom0, SVGDom),
	format('Content-type: ~w~n~n', ['image/svg+xml; charset=UTF-8']),
	xml_write(current_output, SVGDom,
		  [ layout(false)
		  ]).

rewrite_sgv_dom([element(svg, Attrs, Content)],
		[element(svg, Attrs,
			 [ element(script, ['xlink:href'=SVGPan], []),
			   element(g, [ id=viewport
				      ],
				   Content)
			 ])]) :-
	http_absolute_location(js('SVGPan.js'), SVGPan, []).
rewrite_sgv_dom(DOM, DOM).

send_to_dot(Data, Out) :-
	call_cleanup(format(Out, '~s', [Data]),
		     close(Out)), !.

%%	remove_old_data(+Now)
%
%	Remove data that are older than 15 minutes.

remove_old_data(Time) :-
	(   dot_data(Hash, _, Stamp),
	    Time > Stamp+900,
	    retract(dot_data(Hash, _, Stamp)),
	    fail
	;   true
	).

has_graphviz_renderer(Renderer) :-
	process:exe_options(ExeOptions),
	absolute_file_name(path(Renderer), _,
			   [ file_errors(fail)
			   | ExeOptions
			   ]).

no_graph_viz(Renderer) -->
	html(div([ class('no-graph-viz'),
		   style('color:red;')
		 ],
		 [ 'The server does not have the graphviz program ',
		   code(Renderer), ' installed in PATH. ',
		   'See ', a(href('http://www.graphviz.org/'),
			     'http://www.graphviz.org/'), ' for details.'
		 ])).


		 /*******************************
		 *   GENERATING A DOT PROGRAM	*
		 *******************************/

graph(graph(Statements)) -->
	graph(graph([], Statements)).
graph(digraph(Statements)) -->
	graph(digraph([], Statements)).
graph(graph(Options, Statements)) -->
	graph(graph, Options, Statements).
graph(digraph(Options, Statements)) -->
	graph(digraph, Options, Statements).

graph(Type, Options, Statements) -->
	{ must_be(list, Options) }, !,
	strict(Options, Options1), keyword(Type), ws, graph_id(Options1), "{", nl,
	statements(Statements),
	"}", nl.

strict(Options0, Options) -->
	{ selectchk(strict, Options0, Options) }, !,
	keyword(strict).
strict(Options, Options) --> [].

graph_id([ID]) --> !,
	id(ID), ws.
graph_id([]) --> [].

statements([]) --> [].
statements([H|T]) --> "  ", statement(H), ";",  nl, statements(T).

statement(graph(Attrs)) --> keyword(graph), ws, attributes(Attrs).
statement(edge(Attrs)) --> keyword(edge), ws, attributes(Attrs).
statement(node(Attrs)) --> keyword(node), ws, attributes(Attrs).
statement(node(ID, Attrs)) --> !, id(ID), ws, attributes(Attrs).
statement(edge(Edge, Attrs)) --> !, edge(Edge), ws, attributes(Attrs).
statement(A - B) --> !, edge(A - B).
statement(A -> B) --> !, edge(A - B).
statement(ID1 = ID2) --> !, id(ID1), ws, "=", ws, id(ID2).
statement(subgraph(Statements)) --> !,
	keyword(subgraph), ws, "{", nl,
	Statements, "}".
statement(subgraph(ID, Statements)) --> !,
	keyword(subgraph), ws, id(ID), ws, "{", nl,
	Statements, "}".

edge((A-B)-C) --> !, edge(A-B), " -- ", id(C).
edge(A-B)     --> id(A), " -- ", id(B).
edge((A->B)->C) --> !, edge(A-B), " -> ", id(C).
edge(A->B)      --> id(A), " -> ", id(B).

attributes([]) --> !.
attributes(List) --> "[", attribute_list(List), "]".

attribute_list([]) --> [].
attribute_list([H|T]) -->
	attribute(H),
	(   {T == []}
	->  []
	;   ",", attribute_list(T)
	).

attribute(Name=Value) -->
	id(Name),"=",value(Name, Value).
attribute(html(Value), List, Tail) :- !,
	format(codes(List,Tail), 'label=<~w>', [Value]).
attribute(NameValue)  -->
	{NameValue =.. [Name,Value]}, !,
	id(Name),"=",value(Name, Value).

value(Name, Value) -->
	{ string_attribute(Name), !,
	  atom_codes(Value, Codes)
	},
	"\"", cstring(Codes), "\"".
value(_Name, Value, List, Tail) :-
	format(codes(List,Tail), '~w', [Value]).

id(ID) --> { number(ID) }, !, number(ID).
id(ID) --> { atom_codes(ID, Codes) }, "\"", cstring(Codes), "\"".

keyword(Kwd) --> atom(Kwd).
ws --> " ".
nl --> "\n".


		 /*******************************
		 *	  DOT PRIMITIVES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is copied from ClioPatria, rdf_graphviz.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

string_attribute(label(_)).
string_attribute(url(_)).
string_attribute(href(_)).
string_attribute(id(_)).
string_attribute('URL'(_)).
string_attribute(fillcolor(_)).
string_attribute(style(_)).

%%	gv_attr(?AttrName, ?Element, ?Type) is nondet.
%
%	Name and type-declarations for GraphViz   attributes.  Types are
%	defined my must_be/2.
%
%	@see http://www.graphviz.org/doc/info/shapes.html

gv_attr(align,	      table, oneof([center,left,right])).
gv_attr(bgcolor,      table, atom).
gv_attr(border,	      table, atom).
gv_attr(cellborder,   table, atom).
gv_attr(cellpadding,  table, atom).
gv_attr(cellspacing,  table, atom).
gv_attr(color,	      table, atom).
gv_attr(fixedsize,    table, boolean).
gv_attr(height,	      table, atom).
gv_attr(href,	      table, atom).
gv_attr(port,	      table, atom).
gv_attr(target,	      table, atom).
gv_attr(title,	      table, atom).
gv_attr(tooltip,      table, atom).
gv_attr(valign,	      table, oneof([middle,bottom,top])).
gv_attr(width,	      table, atom).

gv_attr(align,	      td,    oneof([center,left,right,text])).
gv_attr(balign,	      td,    oneof([center,left,right])).
gv_attr(bgcolor,      td,    atom).
gv_attr(border,	      td,    atom).
gv_attr(cellpadding,  td,    atom).
gv_attr(cellspacing,  td,    atom).
gv_attr(color,	      td,    atom).
gv_attr(colspan,      td,    integer).
gv_attr(fixedsize,    td,    boolean).
gv_attr(height,	      td,    atom).
gv_attr(href,	      td,    atom).
gv_attr(port,	      td,    atom).
gv_attr(rowspan,      td,    integer).
gv_attr(target,	      td,    atom).
gv_attr(title,	      td,    atom).
gv_attr(tooltip,      td,    atom).
gv_attr(valign,	      td,    oneof([middle,bottom,top])).
gv_attr(width,	      td,    atom).

gv_attr(color,	      font,  atom).
gv_attr(face,	      font,  atom).
gv_attr('point-size', font,  integer).

gv_attr(align,	      br,    oneof([center,left,right])).

gv_attr(scale,	      img,   oneof([false,true,width,height,both])).
gv_attr(src,	      img,   atom).


%%	cstring(+Codes)//
%
%	Create a C-string. =dot= uses UTF-8 encoding.

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".
