%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   post.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	Post unification (BINDING actually) interface for clp(q,r)
	Works by encapsulating the solver vars through a layer of
	attributed vars from the outside binding world.
	The mediating attributed variables layer controls the import
	and export of solver variable bindings.

*/

:- pragma( expand).
:- pragma( nodebug).

:- use_module( wrap).
:- use_module( library(atts)).

:- attribute
	user/1, 				% user variable
	class/1, order/1, lin/1,
	type/1, strictness/1, nonzero/0,
	target/0, keep_indep/0, keep/0. 	% project.pl

%
% little trick to determine the module we are loaded into
%
dummy.
%
:- get_flag(dummy/0,definition_module,M),
	meta_attribute( M,
	[
	    unify: unify_linear/2
	]).

%
% post-binding handler informs the rest of the world
%
unify_linear( _, AttsX) :- var(AttsX).
unify_linear( Y, AttsX) :- nonvar(AttsX),
	unify_linear_nonzero( Y, AttsX),
	unify_linear_type( Y, AttsX),
	unify_linear_user( Y, AttsX).

unify_linear_nonzero( Y, AttsX) :-
	map_atts( AttsX, nonzero),
	!,
	( var(Y) ->
	    put_atts( Y, nonzero)
	;
	    arith_eval( Y =\= 0)
	).
unify_linear_nonzero( _, _).

%
% As this runs after the binding is made, we assume that
% the class data can be repaired after all
%
unify_linear_type( Y, AttsX) :-
	map_atts( AttsX, [class(Class),type(T),strictness(S)]),
	class:drop( Class, Y),
	restore_invariants( Y),
	unify_linear_type( T, Y, S).

%
% some invariants to maintain ...
%
restore_invariants( Y) :- nonvar(Y).
restore_invariants( Y) :- var(Y),
	get_atts( Y, [lin(Lin),type(T),class(Class)]),
	( indep( Lin, Y) ->			% aliasing produced an indep var
	    class:allvars( Class, All),
	    repair_nf_order( All),		% first repair !!!
	    repair_indep( T, Y)
	;
	    true
	).
	%%% sys_invars( Class). 		% debug

repair_nf_order( Vs) :- var( Vs), !.
repair_nf_order( [V|Vs]) :-
  ( get_atts( V, lin(Lin)),
    decompose( Lin, H, _, _),
    \+ nf_ordered( H) ->
	renormalize( Lin, New), 		% todo: specialize
	put_atts( V, lin(New))

  ;
	true
  ),
  repair_nf_order( Vs).

repair_indep( t_L(_), _).
repair_indep( t_U(_), _).
repair_indep( t_Lu(_,_), _).
repair_indep( t_lU(_,_), _).
repair_indep( t_l(L), V) :-
	intro_at( V, L, t_L(L)),		% activate
	basis( V, Basis),
	rcbl( Basis, Binds, []),		% todo: binds always [] ?
	export_binding( Binds).
repair_indep( t_u(U), V) :-
	intro_at( V, U, t_U(U)),
	basis( V, Basis),
	rcbl( Basis, Binds, []),
	export_binding( Binds).
repair_indep( t_lu(L,U), V) :-
	intro_at( V, L, t_Lu(L,U)),		% arbitrary
	basis( V, Basis),
	rcbl( Basis, Binds, []),
	export_binding( Binds).
repair_indep( t_none, V) :-
	( ub( V, Vub-Vb-_) ->			% exchange against thightest
	    basis_drop( Vub),
	    pivot( Vub, V, Vb)
	; lb( V, Vlb-Vb-_) ->
	    basis_drop( Vlb),
	    pivot( Vlb, V, Vb)
	;
	    true
	).

unify_linear_type( t_none,    _, _).
unify_linear_type( t_l(L),    Y, S) :- llb( S, L, Y).
unify_linear_type( t_u(U),    Y, S) :- lub( S, U, Y).
unify_linear_type( t_lu(L,U), Y, S) :- llb( S, L, Y), lub( S, U, Y).
unify_linear_type( t_L(L),    Y, S) :- llb( S, L, Y).
unify_linear_type( t_U(U),    Y, S) :- lub( S, U, Y).
unify_linear_type( t_Lu(L,U), Y, S) :- llb( S, L, Y), lub( S, U, Y).
unify_linear_type( t_lU(L,U), Y, S) :- llb( S, L, Y), lub( S, U, Y).

llb( S, L, V) :- S /\ 2'10 =:= 0, !, {L =< V}.
llb( _, L, V) :-		     {L  < V}.

lub( S, U, V) :- S /\ 2'01 =:= 0, !, {V =< U}.
lub( _, U, V) :-		     {V  < U}.

%
% Need to distinguish between true exported bindings and reflections
% because of clp(r) ...
%
unify_linear_user( Y, AttsX) :-
	map_atts( AttsX, user(Xu)),
	!,
	( var(Y) ->
	    ( get_atts( Y, user(Yu)) ->
		( nonvar(Xu),
		  nonvar(Yu) -> 		% just a reflection
		    print( reflection(Xu,Yu)), nl,
		    true
		;
		    Xu = Yu			% export alias
		)
	    ;
		put_atts( Y, user(Xu))
	    )
	;
	    ( var(Xu) ->
		Xu = Y				% export binding
	    ;
		true				% just a reflection
	    )
	).
unify_linear_user( _, _).

% ----------------------------------------------------------------------

unwrap_term( T, Tu) :- var( T),
	( get_atts( T, user(Tu)) ->
	    true
	;
	    Tu = T
	).
unwrap_term( T, Tu) :- nonvar( T),
	T =.. [F|Args],
	unwrap_list( Args, ArgsU),
	Tu =.. [F|ArgsU].

unwrap_list( [],     []).
unwrap_list( [X|Xs], [U|Us]) :-
	unwrap_term( X, U),
	unwrap_list( Xs, Us).

%
% todo: swap clauses?
%
solver_variable( X, Lin, Xs) :- 			% old user var
	unwrap( X, Xs),
	!,
	get_atts( Xs, lin(Lin)).
solver_variable( X, Lin, X) :-				% old slack
	get_atts( X, lin(Lin)).


% ------------------------- solver variable initializers ----------------------

%
% If we see a nonvar here, this is a fault
%
deref_var( X, Lin) :-				% new var
	solver_variable( X, Lin, _),
	!.
deref_var( X, Lin) :-
	wrap( X, Xs),				% create solver local var
	arith_eval( 0, Z),
	arith_eval( 1, One),
	Lin = [Z,Z,Xs*One],
	put_atts( Xs, [user(X),order(_),lin(Lin),
		       type(t_none),strictness(2'00)]).

%
% internal vars only, no user slot
%
var_with_def_intern( Type, Var, Lin, Strict) :-
  put_atts( Var, [order(_),lin(Lin),type(Type),strictness(Strict)]),
  decompose( Lin, Hom, _, _),
  get_or_add_class( Var, Class),
  same_class( Hom, Class).

%
% todo: avoid wrap for slacks
%
var_intern( Type, X, Xs, Strict) :-			% xref ineq.pl
  wrap( X, Xs),
  arith_eval( 0, Z),
  arith_eval( 1, One),
  Lin = [Z,Z,Xs*One],
  put_atts( Xs, [user(X),order(_),lin(Lin),type(Type),strictness(Strict)]),
  get_or_add_class( Xs, _Class).


% ----------------------------------------------------------------------

