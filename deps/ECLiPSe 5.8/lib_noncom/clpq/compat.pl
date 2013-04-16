%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   compat.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*

    Purpose: Factor out some predicates/functionalities that need
	     different handling in different prolog environments

    Author: Christian Holzbaur

*/

:- pragma( expand).				% needed for goal expansion
:- pragma( nodebug).

:- module_interface( compat).

:- export trans_geler/2.
:- define_macro( geler/2, trans_geler/2, [goal]).

:- begin_module( compat).

:- export
	host_prolog/1,
	geler/2,
	print_values/3,
	reachable/2,
	dump_extern/1,
	raise_exception/1,
	ord_add_element/3.


host_prolog( eclipse).

%:- tool_body((delay)/2,P,M), (import P from M), tool( geler/2, P).
% geler( Vars, Goal) :- delay( Vars, Goal).	% easy, could even save
						% term_variables/2 call

trans_geler(geler(Vars, Goal), suspend(Goal, 2, Vars->inst)).


% -------------- Projection hook attachment for eclipse -------------------

print_values( N, Varnames, Module) :-
	loaded_solvers( [clpq_intern,clpr_intern], Solvers),
	Solvers = [_|_],
	!,
	term_variables( Varnames, Vars),
	solvers_project( Solvers, Vars),
	error( default(N), Varnames, Module),	% proceed with binding printer
	solvers_print( Solvers, Vars).
print_values( N, Varnames, Module) :-
	error( default(N), Varnames, Module).

:- set_error_handler( 155, print_values/3).

reachable( _, Vars) :-				% special solver
	delayed_goals( Gs),
	term_variables( Gs, Vars).

loaded_solvers( [],	[]).
loaded_solvers( [S|Ss], Loaded) :-
	( current_module( S) ->
	    Loaded = [S|Ls],
	    loaded_solvers( Ss, Ls)

	;
	    loaded_solvers( Ss, Loaded)
	).

solvers_project( [],	 _).
solvers_project( [S|Ss], Vs) :-
	call(project(Vs),S),
	solvers_project( Ss, Vs).

solvers_print( [],     _).
solvers_print( [S|Ss], Vs) :-
	call(print_linear_store(Vs),S),
	solvers_print( Ss, Vs).

/*
%
% Collect the trasitive closure of variables connected via
% delayed goals
%
reachable( Vars, Final) :-
	sort( Vars, Todo),
	reachable( Todo, Todo, Final).

reachable( [],	   Final, Final).
reachable( [X|Xs], Sofar, Closure3) :-
	delayed_goals( X, Gs),
	term_variables( Gs, Vs),
	sort( Vs, Vss),
	reachable_merge( Sofar, Vss, Closure1, Todo),
	reachable( Xs,	 Closure1, Closure2),	% depth first
	reachable( Todo, Closure2, Closure3).

reachable_merge( [],		Set2, Set2,  Set2).
reachable_merge( [Head1|Tail1], Set2, Union, New) :-
	reachable_merge_1( Set2, Head1, Tail1, Union, New).

reachable_merge_1( [],		  Head1, Tail1, [Head1|Tail1], []).
reachable_merge_1( [Head2|Tail2], Head1, Tail1, Union,	       New) :-
	compare( Order, Head1, Head2),
	reachable_merge( Order, Head1, Tail1, Head2, Tail2, Union, New).

reachable_merge( =, Head1, Tail1, _,	 Tail2, [Head1|Union], New) :-
	reachable_merge( Tail1, Tail2, Union, New).
reachable_merge( <, Head1, Tail1, Head2, Tail2, [Head1|Union], New) :-
	reachable_merge_2( Tail1, Head2, Tail2, Union, New).
reachable_merge( >, Head1, Tail1, Head2, Tail2, [Head2|Union], [Head2|New]) :-
	reachable_merge_1( Tail2, Head1, Tail1, Union, New).

reachable_merge_2( [], Head2, Tail2, [Head2|Tail2], [Head2|Tail2]).
reachable_merge_2( [Head1|Tail1], Head2, Tail2, Union, New) :-
	compare( Order, Head1, Head2),
	reachable_merge( Order, Head1, Tail1, Head2, Tail2, Union, New).
*/

%
% debugging
%
dump_extern( Term) :-
	term_variables( Term, Vs),
	loaded_solvers( [clpq_intern,clpr_intern], Solvers),
	solvers_project( Solvers, Vs),
	solvers_print( Solvers, Vs),
	fail.					% encapsulation
dump_extern( _).

% -------------------------------------------------------------------------

%
% Only useful for unary predicates
% First arg of the called predicate should be the attributed variable
%
% :- meta_attribute( eclipse, [print:attribute_goal/2]).

:- define_error("Exception",X), setval(exception,X).

raise_exception( Culprit) :-
	getval( exception, N),
	error( N, Culprit).

:- ensure_loaded( library(ordset)).

:- import
	ord_insert/3
 from ordset.

ord_add_element( S1, E, S2) :- ord_insert( S1, E, S2).

