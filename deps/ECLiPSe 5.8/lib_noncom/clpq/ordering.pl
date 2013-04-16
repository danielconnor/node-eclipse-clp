%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   ordering.pl                                            %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

:- module_interface( ordering).

:- export
	ordering/1,
	arrangement/2.

:- begin_module( ordering).

:- import
	class_new/2,
	class_all/2,
	class_process_once/2
 from ordering_class.

:- use_module( library(atts)).

:- attribute succ/1, predcount/1, class/1.

:- meta_attribute( ordering,
	[
	    unify: unify_ordering/2
	    % ,print: get_atts/2
	]).

%
% We don't check for global consistency.
% The arrangement request may be on a subgraph later anyway.
%
unify_ordering( _, AttsX) :- var( AttsX).
unify_ordering( Y, AttsX) :- nonvar( AttsX),
	map_atts( AttsX, [class(Cx),succ(Sx),predcount(Px)]),
	( var(Y) ->
	    ( get_atts( Y, [class(Cy),succ(Sy),predcount(Py)]) ->
		Cx = Cy,				% intra vs. inter
		append( Sx, Sy, Sz),
		Pz is Px + Py,
		put_atts( Y, [succ(Sz),predcount(Pz)])
	    ;
		put_atts( Y, [class(Cx),succ(Sx),predcount(Px)])
	    )
	;
	    %
	    % drop node X from G
	    %
	    dec_succ( Sx)
	).

dec_succ( []).
dec_succ( [S|Ss]) :-
	( var(S), get_atts( S, predcount(P)) ->
	    P1 is P-1,
	    put_atts( S, predcount(P1)),
	    dec_succ( Ss)
	;
	    dec_succ( Ss)
	).

ordering( A < B) :-
	( get_atts( A, [class(ClassA),succ(Sa)]) ->
	    put_atts( A, succ([B|Sa]))
	;
	    class_new( ClassA, A),
	    put_atts( A, [class(ClassA),succ([B]),predcount(0)])
	),
	( get_atts( B, [class(ClassB),predcount(Pcb)]) ->
	    Pcb1 is Pcb+1,
	    put_atts( B, predcount(Pcb1))
	;
	    class_new( ClassB, B),
	    put_atts( B, [class(ClassB),succ([]),predcount(1)])
	),
	ClassA = ClassB.
ordering( A > B) :-
	ordering( B < A).
ordering( []).
ordering( [A|As]) :-
	init_var( A),				% for ordering([A])
	ordering( As, A),
	ordering( As).

ordering( [],	  _).
ordering( [B|Bs], A) :-
	ordering( A < B),
	ordering( Bs, A).

init_var( X) :-
	get_atts( X, class(_)),
	!.
init_var( X) :-
	class_new( Class, X),
	put_atts( X, [class(Class),succ([]),predcount(0)]).

%
% Arr is a permutation of Vars such that
% the ordering criteria are satisfied.
% Destructive on the graph.
%
/**
arrangement( [],   []).
arrangement( Vars, Arr) :-
	Vars = [_|_],
	length( Vars, Len),
	same_class( Vars, Class),
	class_all( Class, All),
	project_graph( All, Vars),
	topsort_init( All, Arr, []),		% O(N)
	length( Arr, Len).			% arrangement exists
**/

arrangement( Vars, Arr) :-
	arrange_class( Vars, Vars, Late, Arr, Late),
	length( Vars, Len),
	length( Arr, Len).			% arrangement exists


arrange_class( [],     _,    []) --> [].
arrange_class( [V|Vs], Vars, Late) -->
	( { get_atts( V, class(Class)) } ->
	     {
		 class_process_once( Class, All),
		 project_graph( All, Vars)
	     },
	     topsort_init( All),
	     arrange_class( Vs, Vars, Late)
	;
	     { Late = [V|Later] },
	     arrange_class( Vs, Vars, Later)
	).

%
% Drop the nodes that are not in the projection
% Currently O(N^2), easy to get O(N)
%
project_graph( Vs,     _) :- var( Vs), !.
project_graph( [V|Vs], Gv) :-
	( among( V, Gv) ->
	    project_graph( Vs, Gv)
	; get_atts( V, succ(Ss)) ->
	    dec_succ( Ss),
	    put_atts( V, [-class(_),-succ(_),-predcount(_)]),
	    project_graph( Vs, Gv)
	;
	    project_graph( Vs, Gv)
	).

among( V, [W|Ws]) :-
	( V==W ->
	    true
	;
	    among( V, Ws)
	).



same_class( [],     _).
same_class( [X|Xs], Class) :-
	( get_atts( X, class(C)) ->
	    C = Class
	;
	    class_new( Class, X),
	    put_atts( X, [class(Class),succ([]),predcount(0)])
	),
	same_class( Xs, Class).

%
% linear search for 0 predecessors: O(N)
%
topsort_init( Xs) --> {var(Xs)}, !.
topsort_init( [X|Xs]) -->
	topsort_check( X),
	topsort_init( Xs).

topsort_check( X) -->
	{ var( X), get_atts( X, [predcount(0),succ(Succ)]) },
	!,
	[ X ],
	{ put_atts( X, predcount(-1)) },
	topsort_drop( Succ).
topsort_check( _) --> [].

%
% in a complete graph there are N successors: O(N)
%
topsort_drop( []) --> [].
topsort_drop( [S|Ss]) -->
	( {
	    var( S),
	    get_atts( S, predcount(Pc)),
	    Pc1 is Pc-1,
	    put_atts( S, predcount(Pc1))
	  } ->
	       topsort_check( S),
	       topsort_drop( Ss)
	;
	       topsort_drop( Ss)
	).

:- module_interface( ordering_class).

:- begin_module( ordering_class).

:- export
	class_new/2,
	class_all/2,
	class_process_once/2.

:- use_module( library(atts)).

:- attribute all/2, processed/0.

:- meta_attribute( ordering_class, [ unify: unify_ordering_class/2 ]).

class_new( Class, X) :-
	put_atts( C, all([X|T],T)),
	Class = C.

class_all( X, All) :-
	get_atts( X, all(All,_)).

class_process_once( X, All) :-
	get_atts( X, [-processed,all(All,_)]),
	!,
	put_atts( X, processed).
class_process_once( _, _).

unify_ordering_class( _, AttsX) :- var( AttsX).
unify_ordering_class( Y, AttsX) :- nonvar( AttsX),
	map_atts( AttsX, all(Lx,Tx)),
	get_atts( Y, all(Ly,Ty)),
	Ty = Lx,
	put_atts( Y, all(Ly,Tx)).





