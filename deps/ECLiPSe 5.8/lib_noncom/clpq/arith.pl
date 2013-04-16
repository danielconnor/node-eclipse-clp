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
	check_rel/1,				% todo: remove
	arith_eps/1,
	arith_eval/1,
	arith_eval/2,
	case_signum/4,
	arith_normalize/2,
	print_decimal/2,
	integerp/1,
	integerp/2,
	log/2,
	exp/3,
	truncate/2,
	integer/2.

:- begin_module( arith).

:- use_module( macros).

:- set_flag( prefer_rationals, on).

:- define_macro( arith_eval/1, trans_arith/2, [goal]).
:- define_macro( arith_eval/2, trans_arith/2, [goal]).
:- define_macro( case_signum/4, trans_arith/2, [goal]).

%trans_arith( arith_eval(Rel), check_rel(Rel)).
trans_arith( arith_eval(Rel), Rel).
trans_arith( arith_eval(E,V), Exp) :-
	( nonground(E) ->
	    % Exp = (V is E, print((V=E)=true), put(0'.), nl)
	    Exp = (V is E)
	;
	    K is E,
	    arith_normalize( K, Kn),
	    Exp = (V = Kn)
	).
trans_arith( case_signum(E,Lt,Z,Gt), Exp) :-
	Exp = (
		  S is sgn(E),
		  % print(signum(E,S)), put(0'.), nl,
		  ( S = -1 -> Lt
		  ; S =  0 -> Z
		  ; S =  1 -> Gt
		  )
	      ).

check_rel( Rel) :-				% todo: drop
	outcome( Rel, Res),
	print( Rel = Res), put(0'.), nl,
	Res = true.

outcome( Rel, true) :- call(Rel), !.
outcome( _,   fail).

arith_eps( 0).

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
	R is rational(C),
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

log( X, R) :-
	R is ln(X).

truncate( X, Res) :-
	Res is fix(X).

integer( X, Res) :-
	Res is fix(X).

integerp( E) :-
	integer( E).
integerp( E) :-
	rational( E),
	1 =:= denominator(E).

%
% todo: get rid of this
%
integerp( E, E) :-
	integer( E).
integerp( E, I) :-
	rational( E),
	1 =:= denominator(E),
	I is numerator(E).

%
% Support for a example
% Print Rat with a precision of N places after the decimal point
%
print_decimal( Rat, N) :-
	Int is integer(Rat),
	Rest is Rat-Int,
	Num is numerator(Rest),
	Den is denominator(Rest),
	write(Int), put(0'.),
	wdig( 0, N, Num, Den),
	nl.

wdig( N, M, _, _) :- N>=M, !.
wdig( _, _, 0, _) :- !. 			% finite decimal expansion
wdig( I, N, A, B) :-
  I1 is I+1,
  D  is (10*A) //  B,
  A1 is (10*A) mod B,
  ( I mod 10 =:= 0 -> put(0' ) ; true ),
  ( I mod 70 =:= 0 -> nl, write('  ') ; true),
  write( D),
  wdig( I1, N, A1, B).

%
% Collect n digits of the decimal expansion of a/b
% where a//b = 0
%
dig( 0, _, _, []) :- !.
dig( _, 0, _, []) :- !. 			% finite decimal expansion
dig( I, A, B, [D|Ds]) :-
  I1 is I-1,
  A10 is 10*A,
  D  is A10 //	B,
  A1 is A10 mod B,
  dig( I1, A1, B, Ds).
