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
:- use_module('../render').

:- register_renderer(graphviz, "Render data using graphviz").

/** <module> Render data using graphviz
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
	compound_name_arguments(Compound, Program, [String]),
	graphviz_program(Program).

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
