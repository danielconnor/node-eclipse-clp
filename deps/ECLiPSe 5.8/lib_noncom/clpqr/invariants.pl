%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   invariants.pl                                          %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).

:- begin_module( clpq_intern).
% :- begin_module( clpr_intern).

debug_handler_i( trace(_,_,_,G), _) :-
	nl,
	invariants( G),
	fail.

:- global
	debug_handler_i/2.

debug_handler_b( trace(_,_,_,G), _) :-
	nl,
	show_bounds( G),
	fail.

:- global
	debug_handler_b/2.


invariants( R) :-
  term_variables( R, Vs),
  % all_var_invars( Vs),
  related_linear_vars( Vs, _, Sys, _),			% redund
  all_sys_invars( Sys).

all_var_invars( []).
all_var_invars( [V|Vs]) :-
  var_invars( V),
  all_var_invars( Vs).

all_sys_invars( []).
all_sys_invars( [V|Vs]) :-
  sys_invars( V),
  all_sys_invars( Vs).

var_invars( V) :- get_atts( V, class(Class)), !,
  ( class:allvars( Class, All) ->
      true
  ;
      raise_exception( class_attribute_unbound)
  ),
  ( otl_pos( All, V, 1, Pos),
    Pos >= 1 -> true
  ; raise_exception( var_not_in_class(V,All))
  ).
var_invars( _).

otl_pos( L, _, _, -1) :- var( L), !.
otl_pos( [X|Xs], Y, N, Pos) :-
  ( X==Y ->  Pos = N
  ;
     N1 is N+1,
     otl_pos( Xs, Y, N1, Pos)
  ).

otl_length( L,	I, O) :- var(L), !, I=O.
otl_length( [], I, I).
otl_length( [_|L], I, O) :-
	I1 is I+1,
	otl_length( L, I1, O).

sys_invars( S) :-
	class:allvars( S, All),
	class:basis( S, Basis),
	otl_length( All, 0, Len),
	ordering_invar( All),
	rhs_invar( All),
	uc_invar( All, All),
	basis_invar( Basis),
	printf( "Checked %d vars OK\n", Len), flush( user).

ordering_invar( Vs) :- var( Vs), !.
ordering_invar( [V|Vs]) :-
  ( get_atts( V, lin(Lin)),
    decompose( Lin, H, _, _),
    \+ nf_ordered( H) ->			% store.pl
	raise_exception( ordering_corrupted( V-Lin))
  ;
	true
  ),
  ordering_invar( Vs).

% ----------------------------------------------------------------------

uc_invar( L, _) :- var(L), !.
uc_invar( [X|Xs], All) :-
	( get_atts( X, type(t_none)) ->
	    uc_dep( All, X)
	;
	    true
	),
	uc_invar( Xs, All).

uc_dep( Deps, _) :- var(Deps), !.
uc_dep( [D|Deps], X) :-
	( get_atts( D, [type(T),lin(Lin)]),
	  nf_coeff_of( Lin, X, _) ->
	  ( T = t_none ->
	      true
	  ;
	      raise_exception( uc(D:Lin, X))
	  )
	;
	    true
	),
	uc_dep( Deps, X).

% ----------------------------------------------------------------------

basis_invar( []).
basis_invar( [B|Bs]) :-
	( get_atts( B, [lin(Lin),type(T)]),
	  (T=t_l(_);T=t_u(_);T=t_lu(_,_)),
	  decompose( Lin, H, _, _),
	  all_active( H) ->
	    true
	;
	    raise_exception( basis(B:T:Lin))
	),
	basis_invar( Bs).

all_active( []).
all_active( [X*_|Xs]) :-
	get_atts( X, type(T)),
	(T=t_L(_);T=t_U(_);T=t_Lu(_,_);T=t_lU(_,_)),
	all_active( Xs).

% ----------------------------------------------------------------------

rhs_invar( Xs) :- var( Xs), !.
rhs_invar( [X|Xs]) :-
  (  get_atts( X, [lin(Lin),type(Type)]) ->
       ( rhs_invar_one( X, Lin, Type) ->
	   true
       ;
	   show_bound( [X])
       )
  ;
	true
  ),
  rhs_invar( Xs).

rhs_invar_one( X, Lin, Type) :-
  decompose( Lin, Hom, R, I),
  rhs_invar_eval( Hom, 0, R),
  arith_eval( R+I, Rhs),
  ( Type = t_l(L) ->	( indep( Lin, X) ->
			   true
			;
			   arith_eval( L =< Rhs)
			)
  ; Type = t_u(U) ->	( indep( Lin, X) ->
			   true
			;
			   arith_eval( Rhs =< U)
			)
  ; Type = t_lu(L,U) -> ( indep( Lin, X) ->
			   true
			;
			   arith_eval( L =< Rhs),
			   arith_eval( Rhs =< U)
			)
  ; Type = t_L(L) ->	arith_eval( L =:= R)
  ; Type = t_U(U) ->	arith_eval( U =:= R)
  ; Type = t_Lu(L,U) -> arith_eval( L =:= R),
			arith_eval( Rhs =< U)
  ; Type = t_lU(L,U) -> arith_eval( L =< Rhs),
			arith_eval( R =:= U)
  ;
	true
  ).

rhs_invar_eval( [],	  Ri,Ro) :- arith_eval( Ri =:= Ro).
rhs_invar_eval( [X*K|Xs], Ri,Ro) :-
  get_atts( X, type(Type)),
  ( Type = t_L(L)    -> arith_eval( Ri+K*L, Rii)
  ; Type = t_Lu(L,_) -> arith_eval( Ri+K*L, Rii)
  ; Type = t_U(U)    -> arith_eval( Ri+K*U, Rii)
  ; Type = t_lU(_,U) -> arith_eval( Ri+K*U, Rii)
  ;
	Rii=Ri
  ),
  rhs_invar_eval( Xs, Rii, Ro).

show_bounds( R) :-
  term_variables( R, Vs),
  related_linear_vars( Vs, _, Sys, _),			% project.pl
  all_sys_bounds( Sys).

all_sys_bounds( []).
all_sys_bounds( [S|Ss]) :-
	sys_bounds( S),
	all_sys_bounds( Ss).

sys_bounds( S) :-
	class:allvars( S, All),
	show_bound( All).

show_bound( L) :- var( L), !.
show_bound( []).
show_bound( [X|Xs]) :-
  get_atts( X, [lin(Lin),type(Type)]),
  !,
  decompose( Lin, _, R, I),
  arith_eval( R+I, Rhs),
  ( Type = t_l(L) ->	( arith_eval( L =< Rhs) ->
			    write('    ')
			;
			    write('*** ')
			)
  ; Type = t_u(U) ->	( arith_eval( Rhs =< U) ->
			    write('    ')
			;
			    write('*** ')
			)
  ; Type = t_lu(L,U) -> ( arith_eval( L =< Rhs),
			  arith_eval( Rhs =< U) ->
			    write('    ')
			;
			    write('*** ')
			)
  ; Type = t_L(L) ->	( arith_eval( L =:= R) -> write('    ') ; write('*** ') )
  ; Type = t_U(U) ->	( arith_eval( U =:= R) -> write('    ') ; write('*** ') )
  ; Type = t_Lu(L,U) -> ( arith_eval( L =:= R),
			  arith_eval( Rhs =< U) ->
			    write('    ')
			;
			    write('*** ')
			)
  ; Type = t_lU(L,U) -> ( arith_eval( L =< Rhs),
			  arith_eval( R =:= U) ->
			    write('    ')
			;
			    write('*** ')
			)
  ;
	write('    ')
  ),
  printf_current( output, X:Type:Rhs), nl,
  show_bound( Xs).
show_bound( [X|Xs]) :-
	write('    '), printf_current( output, X:'not a lin solver var'), nl,
	show_bound( Xs).

