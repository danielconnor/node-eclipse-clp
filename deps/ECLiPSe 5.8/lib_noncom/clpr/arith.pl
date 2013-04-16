%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   arith.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module_interface( arith).

:- export
	trans_arith/2,
	arith_eps/1,
	arith_eval/1,
	arith_eval/2,
	case_signum/4,
	arith_normalize/2,
	integerp/1,
	integerp/2,
	log/2,
	exp/3,
	truncate/2,
	integer/2.

:- begin_module( arith).

:- use_module( macros).

:- set_flag( prefer_rationals, on).		% for clp(r) too !!!

:- define_macro( arith_eval/1, trans_arith/2, [goal]).
:- define_macro( arith_eval/2, trans_arith/2, [goal]).
:- define_macro( case_signum/4, trans_arith/2, [goal]).

trans_arith( arith_eval(Rel), Exp) :-
	linearize( Rel, Res, Linear),
	specialize_R( Linear, Code, []),
	Res == boolean,
	l2conj( Code, Exp).
trans_arith( arith_eval(E,V), Exp) :-
	( nonground(E) ->
	    linearize( E, V, Linear),
	    specialize_R( Linear, Code, []),
	    l2conj( Code, Exp)
	;
	    K is E,
	    arith_normalize( K, Kn),
	    Exp = (V = Kn)
	).
trans_arith( case_signum(E,Lt,Z,Gt), Exp) :-
	arith_eps( Eps),
	NegEps is -Eps,
	linearize( E, Res, Linear),
	specialize_R( Linear, Code,
	[
	    Rv is Res,
	    ( Rv < NegEps -> Lt
	    ; Rv > Eps	  -> Gt
	    ;		     Z
	    )
	]),
	l2conj( Code, Exp).

arith_eps( 1e-05) :- get_flag( float_precision, single).
arith_eps( 1e-10) :- get_flag( float_precision, double).

%
% normalize numerical constants
%
/*
arith_normalize( C, N) :-			% stay in Z as long as possible
	integer( C),
	!,
	N = C.
*/
arith_normalize( C, N) :-
	R is float(C),
	R = N.					% late unification

%
% Make avail in is/2
%
exp( M, E, Res) :-
	Ee is E,
	rational(Ee),
	1 is denominator(Ee),
	!,
	Res is M^numerator(Ee).
exp( M, E, Res) :-				% for rkf45.pl in clp(q)
	Ee is E,
	Me is M,
	rational(Me),
	rational(Ee),
	!,
	Res is float(Me)^Ee.
exp( M, E, Res) :-
	Res is M^E.

log( E, R) :-
	R is ln(E).

truncate( X, Res) :-
	Res is fix(X).

integer( X, Res) :-
	Res is fix(X).

integerp( E) :-
	integer( E).
integerp( E) :-
	rational( E),
	1 =:= denominator(E).
integerp( E) :-
	real( E),
	floor(E) =:= E.

%
% todo: get rid of this
%
integerp( E, E) :-
	integer( E).
integerp( E, I) :-
	rational( E),
	1 =:= denominator(E),
	I is numerator(E).
integerp( E, I) :-
	real( E),
	floor(E) =:= E,
	I is fix(E).


% ----------------------------------------------------------------------

specialize_R( []) --> [].
specialize_R( [Op|Ops]) -->
  specialize_R( Op),
  specialize_R( Ops).
%
specialize_R( op_var(Var,Var)) --> [].
specialize_R( op_integer(R,I)) --> [], { R is float(I) }.
specialize_R( op_rat(R,X))     --> [], { R is float(X) }.
specialize_R( op_float(F,F))   --> [].
specialize_R( apply(R,Func)) -->
  ( specialize_R_fn( Func, R) ->
	[]
  ;
	[ R is Func ]
  ).

%
% An absolute eps is of course not very meaningful.
% An eps scaled by the magnitude of the operands participating
% in the comparison is too expensive to support in Prolog on the
% other hand ...
%
%
%		 -eps	0  +eps
%   ---------------[----|----]----------------
%	     < 0		  > 0
%      <-----------]	     [----------->
%	    =< 0
%      <---------------------]
%				  >= 0
%		   [--------------------->
%
%
specialize_R_fn( X  <  Y, boolean)  -->
  {
	arith_eps( Eps),
	NegEps is -Eps
  },
  ( {number(X),X=:=0} ->
      [ Y > Eps ]
  ; {number(Y),Y=:=0} ->
      [ X < NegEps ]
  ;
      [ X-Y < NegEps ]
  ).
specialize_R_fn( X  >  Y, boolean)  --> specialize_R_fn( Y  < X, boolean).
specialize_R_fn( X  =< Y, boolean)  -->
  {
    arith_eps( Eps)
  },
  [ X-Y < Eps ].
specialize_R_fn( X  >= Y, boolean)  --> specialize_R_fn( Y =< X, boolean).
specialize_R_fn( X =:= Y, boolean)  -->
  {
    arith_eps( Eps),
    NegEps is -Eps
  },
  ( {number(X),X=:=0} ->
	[ Y >= NegEps, Y =< Eps ]
  ; {number(Y),Y=:=0} ->
	[ X >= NegEps, X =< Eps ]
  ;
	[
	  Diff is X-Y,
	  Diff =< Eps,
	  Diff >= NegEps
	]
  ).
specialize_R_fn( X =\= Y, boolean)  -->
  {
    arith_eps( Eps),
    NegEps is -Eps
  },
  [
    Diff is X-Y,
    ( Diff < NegEps -> true ; Diff > Eps )
  ].

% ----------------------------------------------------------------------

linearize( Term, Res, Linear) :-
  linearize( Term, Res, Vs,[], Lin, []),
  keysort( Vs, Vss),
  ( Vss = [] ->     Linear = Lin
  ; Vss = [V|Vt] -> join_vars( Vt, V, Linear, Lin)
  ).

%
% flatten the evaluation, collect variables, shared by Q,R,...
%
linearize( X,	     R, [X-R|Vs],Vs) --> {var(X)}, !.
linearize( X,	     R, Vs,Vs) --> {integer(X)}, !,	[ op_integer(R,X) ].
linearize( X,	     R, Vs,Vs) --> {real(X)}, !,	[ op_float(R,X) ].
linearize( X,	     R, Vs,Vs) --> {rational(X)}, !,	[ op_rat(R,X) ].
linearize( rat(N,D), R, Vs,Vs) --> !,			[ op_rat(R,N,D) ].
linearize( Term,     R, V0,V1) -->
  {
    functor( Term, N, A),
    functor( Skeleton, N, A)
  },
  linearize_args( A, Term, Skeleton, V0,V1),		[ apply(R,Skeleton) ].

linearize_args( 0, _, _, Vs,Vs) --> !.
linearize_args( N, T, S, V0,V2) -->
  {
    arg( N, T, Arg),
    arg( N, S, Res),
    N1 is N-1
  },
  linearize( Arg, Res, V0,V1),
  linearize_args( N1, T, S, V1,V2).

join_vars( [],	      Y-Ry) --> [ op_var(Ry,Y) ].
join_vars( [X-Rx|Xs], Y-Ry) -->
  ( {X==Y} ->
      {Rx=Ry},
      join_vars( Xs, Y-Ry)
  ;
      [ op_var(Ry,Y) ],
      join_vars( Xs, X-Rx)
  ).

l2conj( [],	true).
l2conj( [X|Xs], Conj) :-
  ( Xs = [] ->	  Conj = X
  ; Xs = [_|_] -> Conj = (X,Xc), l2conj( Xs, Xc)
  ).
