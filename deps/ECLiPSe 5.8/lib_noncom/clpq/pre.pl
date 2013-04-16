%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   pre.pl                                                 %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	Pre unification (BINDING actually) interface for clp(q,r)
	modelled after the SICStus scheme.
*/

:- pragma( expand).
:- pragma( nodebug).

:- use_module( library(atts)).

:- attribute
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
	    unify:     unify_linear/2,
	    pre_unify: pre_unify_linear/2
	]).

pre_unify_linear( X, Y) :-
	get_atts( X, lin(_)),
	!,
	normalized( Y),
	verify_nonzero( X, Y),
	verify_lin( X, Y).
pre_unify_linear( _, _).

normalized( X) :- var(X).
normalized( X) :- nonvar(X),
	( number(X),
	  arith_normalize( X, X) ->
	    true
	;
	    raise_exception( not_normalized(X))
	).

unify_linear( _, AttsX) :- var(AttsX).
unify_linear( Y, AttsX) :- nonvar(AttsX),
	map_atts( AttsX, [class(Class),type(T),strictness(S)]),
	% ( var(Y) -> sys_invars( Class) ; true ), % debug.pl
	class:allvars( Class, All),
	late_export( All),
	unify_linear_type( T, Y, S).

late_export( Deps) :- var(Deps),!.
late_export( [Dep|Deps]) :-
	( var(Dep), get_atts( Dep, lin(Lin)) ->
	    decompose( Lin, Hom, _, Inhom),
	    late_export( Hom, Dep, Inhom),
	    late_export( Deps)
	;
	    late_export( Deps)
	).

late_export( [],       X, Inhom) :- export_binding( X, Inhom).
late_export( [Y*K|Ys], X, Inhom) :-
  ( Ys = [],
    Y \== X,
    arith_eval( K=:=1),
    arith_eval( Inhom=:=0) ->
      export_binding( X, Y)
  ;
      true
  ).

verify_nonzero( X, Y) :-
  get_atts( X, nonzero),
  !,
  ( var(Y) ->
      put_atts( Y, nonzero)
  ;
      arith_eval( Y =\= 0)
  ).
verify_nonzero( _, _).


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

verify_lin( X, Y) :-
	nf( X-Y, Lin),
	deref( Lin, Lind),
	decompose( Lind, H, _, I),
	solve( H, Lind, I, _, _),
	%
	% prepare for aliasing
	%
	get_atts( X, [class(Class),lin(LinX)]),
	( indep( LinX, X) ->
	     ( ub( X, Vub-Vb-_) ->		% exchange against thightest
		 basis_drop( Vub),
		 pivot( Vub, X, Vb)
	     ; lb( X, Vlb-Vb-_) ->
		 basis_drop( Vlb),
		 pivot( Vlb, X, Vb)
	     ; nb( X, Class, Nb) ->
		 pivot( Nb, X, t_none)
	     ;
		 true
	     )
	;
	    true
	),
	class:drop( Class, X).

nb( X, Class, Dep) :-
	class:allvars( Class, All),
	nb_loop( All, X, Dep).

nb_loop( Deps, _, _) :- var(Deps), !, fail.
nb_loop( [Dep|Deps], X, Res) :-
	( get_atts( Dep, [lin(Lin),type(t_none)]),
	  nf_coeff_of( Lin, X, _) ->
	     Res = Dep
	;
	     nb_loop( Deps, X, Res)
	).

% ----------------------------------------------------------------------

unwrap_term( T, T).				% rather easy ...

unwrap_list( L, L).				% rather easy ...

solver_variable( X, Lin, X) :-
	get_atts( X, lin(Lin)).


% ------------------------- solver variable initializers ----------------------

%
% If we see a nonvar here, this is a fault
%
deref_var( X, Lin) :-				% new var
	solver_variable( X, Lin, _),
	!.
deref_var( X, Lin) :-
	arith_eval( 0, Z),
	arith_eval( 1, One),
	Lin = [Z,Z,X*One],
	put_atts( X, [order(_),lin(Lin),
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
var_intern( Type, X, X, Strict) :-			% xref ineq.pl
  arith_eval( 0, Z),
  arith_eval( 1, One),
  Lin = [Z,Z,X*One],
  put_atts( X, [order(_),lin(Lin),type(Type),strictness(Strict)]),
  get_or_add_class( X, _Class).



