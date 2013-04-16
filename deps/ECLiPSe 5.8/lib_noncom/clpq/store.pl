%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   store.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

/*
%
% critical impact on the backsubstitution effort
% AND precision in clp(r)
%
nf_ordering( A, B, Rel) :-
	get_atts( A, order( Oa)),
	get_atts( B, order( Ob)),
	compare( Rel, Oa, Ob).

%
% no references to attributes in this file below this point
%


decompose( [I,R|Hom], Hom, R, I).
*/

%
% All constants to canonical rep.
%
normalize_scalar( S, [N,Z]) :-
  arith_normalize( S, N),
  arith_eval( 0, Z).

renormalize( List, Lin) :-
  decompose( List, Hom, R, I),
  length( Hom, Len),
  renormalize_log( Len, Hom, [], Lin0),
  add_linear_11( [I,R], Lin0, Lin).

renormalize_log( 1, [Term|Xs], Xs, Lin) :- !,
  Term = X*_,
  renormalize_log_one( X, Term, Lin).
renormalize_log( 2, [A,B|Xs], Xs, Lin) :- !,
  A = X*_,
  B = Y*_,
  renormalize_log_one( X, A, LinA),
  renormalize_log_one( Y, B, LinB),
  add_linear_11( LinA, LinB, Lin).
renormalize_log( N, L0, L2, Lin) :-
  P is N>>1,
  Q is N-P,
  renormalize_log( P, L0, L1, Lp),
  renormalize_log( Q, L1, L2, Lq),
  add_linear_11( Lp, Lq, Lin).

%
% todo: (in fm)
% todo: in clp(r) with single precision we might meet 'eliminated' vars ...
%
renormalize_log_one( X,        Term, Res) :- var(X),
  arith_eval( 0, Z),
  ( get_atts( X, lin(_)) ->
      Res = [Z,Z,Term]
  ;
      Res = [Z,Z]
  ).
renormalize_log_one( X,        Term, Res) :- nonvar(X),
  Term = X*K,
  arith_eval( X*K, Xk),
  normalize_scalar( Xk, Res).

% ----------------------------- sparse vector stuff ---------------------------- %

:- mode add_linear_ff( +, +, +, +, -).
%
add_linear_ff( LinA, Ka, LinB, Kb, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia*Ka+Ib*Kb, Ic),
  arith_eval( Ra*Ka+Rb*Kb, Rc),
  add_linear_ffh( Ha, Ka, Hb, Kb, Hc).

:- mode add_linear_ffh( +, +, +, +, -).
%
add_linear_ffh( [],	  _,  Ys, Kb, Zs) :- mult_hom( Ys, Kb, Zs).
add_linear_ffh( [X*Kx|Xs], Ka, Ys, Kb, Zs) :-
  add_linear_ffh( Ys, X, Kx, Xs, Zs, Ka, Kb).

  :- mode add_linear_ffh( +, ?, +, +, -, +, +).
  %
  add_linear_ffh( [],	     X, Kx, Xs, Zs, Ka, _) :- mult_hom( [X*Kx|Xs], Ka, Zs).
  add_linear_ffh( [Y*Ky|Ys], X, Kx, Xs, Zs, Ka, Kb) :-
     nf_ordering( X, Y, Rel),
     add_linear_ffh_case( Rel, X, Kx, Xs, Zs, Ka, Kb, Y, Ky, Ys).

     :- mode add_linear_ffh_case( +, ?, +, +, -, +, +, ?, +, +).
     %
     add_linear_ffh_case( =, X, Kx, Xs, Zs, Ka, Kb, _, Ky, Ys) :-
	arith_eval( Kx*Ka+Ky*Kb, Kz),
	( arith_eval(Kz=:=0) ->
	    add_linear_ffh( Xs, Ka, Ys, Kb, Zs)
	;
	    Zs = [X*Kz|Ztail],
	    add_linear_ffh( Xs, Ka, Ys, Kb, Ztail)
	).
     add_linear_ffh_case( <, X, Kx, Xs, Zs, Ka, Kb, Y, Ky, Ys) :-
	Zs = [X*Kz|Ztail],
	arith_eval( Kx*Ka, Kz),
	add_linear_ffh( Xs, Y, Ky, Ys, Ztail, Kb, Ka).
     add_linear_ffh_case( >, X, Kx, Xs, Zs, Ka, Kb, Y, Ky, Ys) :-
	Zs = [Y*Kz|Ztail],
	arith_eval( Ky*Kb, Kz),
	add_linear_ffh( Ys, X, Kx, Xs, Ztail, Ka, Kb).

:- mode add_linear_f1( +, +, +, -).
%
add_linear_f1( LinA, Ka, LinB, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia*Ka+Ib, Ic),
  arith_eval( Ra*Ka+Rb, Rc),
  add_linear_f1h( Ha, Ka, Hb, Hc).

:- mode add_linear_f1h( +, +, +, -).
%
add_linear_f1h( [],	  _,  Ys, Ys).
add_linear_f1h( [X*Kx|Xs], Ka, Ys, Zs) :-
  add_linear_f1h( Ys, X, Kx, Xs, Zs, Ka).

  :- mode add_linear_f1h( +, ?, +, +, -, +).
  %
  add_linear_f1h( [],	     X, Kx, Xs, Zs, Ka) :- mult_hom( [X*Kx|Xs], Ka, Zs).
  add_linear_f1h( [Y*Ky|Ys], X, Kx, Xs, Zs, Ka) :-
     nf_ordering( X, Y, Rel),
     add_linear_f1h_case( Rel, X, Kx, Xs, Zs, Ka, Y, Ky, Ys).

     :- mode add_linear_f1h_case( +, ?, +, +, -, +, ?, +, +).
     %
     add_linear_f1h_case( =, X, Kx, Xs, Zs, Ka, _, Ky, Ys) :-
	 arith_eval( Kx*Ka+Ky, Kz),
	 ( arith_eval(Kz=:=0) ->
	     add_linear_f1h( Xs, Ka, Ys, Zs)
	 ;
	     Zs = [X*Kz|Ztail],
	     add_linear_f1h( Xs, Ka, Ys, Ztail)
	 ).
     add_linear_f1h_case( <, X, Kx, Xs, Zs, Ka, Y, Ky, Ys) :-
	 Zs = [X*Kz|Ztail],
	 arith_eval( Kx*Ka, Kz),
	 add_linear_f1h( Xs, Ka, [Y*Ky|Ys], Ztail).
     add_linear_f1h_case( >, X, Kx, Xs, Zs, Ka, Y, Ky, Ys) :-
	 Zs = [Y*Ky|Ztail],
	 add_linear_f1h( Ys, X, Kx, Xs, Ztail, Ka).

%:- mode add_linear_11( +, +, -).
%
add_linear_11( LinA, LinB, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia+Ib, Ic),
  arith_eval( Ra+Rb, Rc),
  add_linear_11h( Ha, Hb, Hc).

:- mode add_linear_11h( +, +, -).
%
add_linear_11h( [],	  Ys, Ys).
add_linear_11h( [X*Kx|Xs], Ys, Zs) :-
  add_linear_11h( Ys, X, Kx, Xs, Zs).

  :- mode add_linear_11h( +, ?, +, +, -).
  %
  add_linear_11h( [],	     X, Kx, Xs, [X*Kx|Xs]).
  add_linear_11h( [Y*Ky|Ys], X, Kx, Xs, Zs) :-
     nf_ordering( X, Y, Rel),
     add_linear_11h_case( Rel, X, Kx, Xs, Zs, Y, Ky, Ys).

     :- mode add_linear_11h_case( +, ?, +, +, -, ?, +, +).
     %
     add_linear_11h_case( =, X, Kx, Xs, Zs, _, Ky, Ys) :-
	arith_eval( Kx+Ky, Kz),
	( arith_eval(Kz=:=0) ->
	    add_linear_11h( Xs, Ys, Zs)
	;
	    Zs = [X*Kz|Ztail],
	    add_linear_11h( Xs, Ys, Ztail)
	).
     add_linear_11h_case( <, X, Kx, Xs, Zs, Y, Ky, Ys) :-
	Zs = [X*Kx|Ztail],
	add_linear_11h( Xs, Y, Ky, Ys, Ztail).
     add_linear_11h_case( >, X, Kx, Xs, Zs, Y, Ky, Ys) :-
	Zs = [Y*Ky|Ztail],
	add_linear_11h( Ys, X, Kx, Xs, Ztail).

:- mode mult_linear_factor( +, +, -).
%
mult_linear_factor( Lin, K, Mult) :-
  arith_eval( K=:=1 ),				% avoid copy
  !,
  Mult = Lin.
mult_linear_factor( Lin, K, Res) :-
  decompose( Lin, Hom, R, I),
  decompose( Res, Mult, Rk, Ik),
  arith_eval( I*K, Ik),
  arith_eval( R*K, Rk),
  mult_hom( Hom, K, Mult).

:- mode mult_hom( +, +, -).
%
mult_hom( [],	     _, []).
mult_hom( [A*Fa|As], F, [A*Fan|Afs]) :-
  arith_eval( F*Fa, Fan),
  mult_hom( As, F, Afs).

/*
%
% slightly stabilizes clp(r) numerically
%
mult_hom( [],	     _, []).
mult_hom( [X*Kx|Xs], K, Res) :-
  arith_eval( K*Kx, C),
  ( arith_eval( C=:=0) ->
     mult_hom( Xs, K, Res)
  ;
     Res = [X*C|Tail],
     mult_hom( Xs, K, Tail)
  ).
*/

%
% Replace V in H by its new definition, Vh+Vi
%
nf_substitute( V, LinV, LinX, LinX1) :-
  delete_factor( V, LinX, LinW, K),
  add_linear_f1( LinV, K, LinW, LinX1).


delete_factor( Vid, Lin, Res, Coeff) :-
  decompose( Lin, Hom, R, I),
  decompose( Res, Hdel, R, I),
  delete_factor_hom( Vid, Hom, Hdel, Coeff).

nf_ordered( []).
nf_ordered( [X*_|Xs]) :-
  nf_ordered( Xs, X).

  nf_ordered( [],	_).
  nf_ordered( [Y*_|Ys], X) :-
    nf_ordering( X, Y, <),
    nf_ordered( Ys, Y).

:- mode delete_factor_hom( ?, +, -, -).

/**/
%
% Makes no use of the nf_ordering and is faster ...
% Depends of course on the price of nf_ordering/3
%
delete_factor_hom( Vid, [Car|Cdr], RCdr, RKoeff) :-
  Car = Var*Koeff,
  compare( R, Var, Vid),
  delete_factor_hom_case( R, Car, RCdr, RKoeff, Vid, Cdr, Koeff).

  :- mode delete_factor_hom_case( +, +, -, -, ?, +, +).
  %
  delete_factor_hom_case( =, _,   RCdr, RKoeff, _,   Cdr, Koeff) :-
    RCdr = Cdr, RKoeff=Koeff.
  delete_factor_hom_case( <, Car, RCdr, RKoeff, Vid, Cdr, _) :-
    RCdr = [Car|RCdr1],
    delete_factor_hom( Vid, Cdr, RCdr1, RKoeff).
  delete_factor_hom_case( >, Car, RCdr, RKoeff, Vid, Cdr, _) :-
    RCdr = [Car|RCdr1],
    delete_factor_hom( Vid, Cdr, RCdr1, RKoeff).
/**/
/**
%
%
%
delete_factor_hom( Vid, [Car|Cdr], RCdr, RKoeff) :-
  Car = Var*Koeff,
  nf_ordering( Vid, Var, Rel),
  ( Rel= = ->
    RCdr = Cdr, RKoeff=Koeff
  ; Rel= > ->
    RCdr = [Car|RCdr1],
    delete_factor_hom( Vid, Cdr, RCdr1, RKoeff)
  ).
**/


% nf_coeff_of( Nf, X, Coeff)
% determine the coeff of variable X in Nf
% fails if X is not a member of the Nf
%
:- mode nf_coeff_of( +, ?, -).
%
nf_coeff_of( Lin, Vid, Coeff) :-
  decompose( Lin, Hom, _, _),
  nf_coeff_hom( Hom, Vid, Coeff).

:- mode nf_coeff_hom( +, ?, -).
%
nf_coeff_hom( [Var*K|Vs], Vid, Coeff) :-
  ( Vid == Var ->
	Coeff = K
  ;
	nf_coeff_hom( Vs, Vid, Coeff)
  ).

:- mode nf_rhs_x( +, ?, -, -).
%
nf_rhs_x( Lin, X, Rhs,K) :-
  decompose( Lin, Tail, R, I),
  nf_coeff_hom( Tail, X, K),
  arith_eval( R+I, Rhs).			% late because X may not occur in H

%
% solve for New = Lin1
%
:- mode isolate( ?, +, -).
%
isolate( New, Lin, Lin1) :-
  delete_factor( New, Lin, Lin0, Coeff),
  arith_eval( -1/Coeff, K),
  mult_linear_factor( Lin0, K, Lin1).


indep( Lin, X) :-
  decompose( Lin, [Y*K], _, I),
  X == Y,
  arith_eval( K=:=1),
  arith_eval( I=:=0).

nf2sum( [],	I, I).
nf2sum( [X|Xs], I, Sum) :-
  ( arith_eval(I=:=0) ->
      X = Var*K,
      ( arith_eval( K=:=1) ->
	  hom2sum( Xs, Var, Sum)
      ; arith_eval( K=:= -1) ->
	  hom2sum( Xs, -Var, Sum)
      ;
	  hom2sum( Xs, K*Var, Sum)
      )
  ;
      hom2sum( [X|Xs], I, Sum)
  ).

hom2sum( [],	     Term,  Term).
hom2sum( [Var*K|Cs], Sofar, Term) :-
  ( arith_eval( K=:=1) ->
      Next = Sofar + Var
  ; arith_eval( K=:= -1) ->
      Next = Sofar - Var
  ; arith_eval( K < 0) ->
      arith_eval( -K, Ka),
      Next = Sofar - Ka*Var
  ;
      Next = Sofar + K*Var
  ),
  hom2sum( Cs, Next, Term).


