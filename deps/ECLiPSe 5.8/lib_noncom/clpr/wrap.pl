%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   wrap.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

:- module_interface(wrap).

:- export
	wrap/2,
	unwrap/2.

:- begin_module(wrap).

:- use_module( compat).
:- use_module( library(atts)).

:- import
	 nf/2
	from nf.

:- import
	solve_eq/1
 from clpr_intern.

:- import
	arith_normalize/2
 from arith.

:- attribute solver/1.

wrap( User, Solver) :-
	put_atts( User, solver(Solver)).

unwrap( User, Solver) :-
	get_atts( User, solver(Solver)).

:- meta_attribute( wrap,
	[
	    unify: unify_wrap/2
	    % ,print: print_wrap/2
	]).

unify_wrap( _, AttsX) :- var( AttsX).
unify_wrap( Y, AttsX) :- nonvar( AttsX),
	map_atts( AttsX, solver(SolverX)),
	( var(Y) ->
	    ( get_atts( Y, solver(SolverY)) ->
		nf( SolverX - SolverY, Nf),
		solve_eq( Nf)
	    ;
		put_atts( Y, solver(Y))
	    )
	;
	    ( number(Y),
	      arith_normalize( Y, Y) ->
		true
	    ;
		raise_exception( not_normalized(Y))
	    ),
	    nf( SolverX - Y, Nf),
	    solve_eq( Nf)
	).

print_wrap( V, solver(S)) :-
	get_atts( V, solver(S)).

