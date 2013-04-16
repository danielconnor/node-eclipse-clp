% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: rationals.pl,v 1.3 2001/09/13 17:48:57 js10 Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% sccsid("%W%             %E%").
% sccscr("%Z%  Copyright 1993 ECRC GmbH ").
%
% IDENTIFICATION:	rationals.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		macro definition (/)/2
%
% DESCRIPTION:		Parse <int>/<int> as a rational number
%			Switch on the prefer_rationals option
%

:- module(rationals).

:- export
	tr_rat_in/2,
	tr_rat_out/2.

:- set_flag(prefer_rationals, on).


% parse N/D as a rational if N and D are integers or rationals

tr_rat_in(N/D, Rat) :-
	( integer(N) ; rational(N)),
	( integer(D) ; rational(D)),
	Rat is N/D.

:- export macro((/)/2, tr_rat_in/2, []).


% print rationals as (/)/2 structure or integer

tr_rat_out(Rat, Out) :-
	N is numerator(Rat),
	D is denominator(Rat),
	( D == 1 ->
		Out = N
	;
		Out = N/D
	).

:- export macro(type(rational), tr_rat_out/2, [write]).
