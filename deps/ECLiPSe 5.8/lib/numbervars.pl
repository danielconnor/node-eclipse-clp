% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: numbervars.pl,v 1.4 2001/09/13 17:48:57 js10 Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% sccsid("%W%             %E%").
% sccscr("%Z%  Copyright 1991 ECRC GmbH ").
%
% IDENTIFICATION:       numbervars.pl
%
% AUTHOR:               Joachim Schimpf
%
% CONTENTS:             numbervars/3
%

:- module(numbervars).

:- comment(summary, "C-Prolog style numbervars predicate").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2001/09/13 17:48:57 $").
:- comment(desc, html("
    Implements the numbervars(Term, From, To) predicate of C-Prolog.  Term
    is any term, From and To are integer numbers.  All variables in Term
    are instantiated to terms of the form
    <PRE>
	$VAR(N) 
    </PRE>
    where N is an integer number.  The first encountered variable will be
    coded by the number From, on exit To is instantiated to the next
    unused number. 
    <P>
    This predicate can thus be used to encode nonground term using a
    ground representation.  Note that metaterms can be used for the same
    purpose, but their use is both more efficient and more general,
    because the variables are not actually instantiated and so they can be
    used again as variables when needed. 
    ")).

:- export numbervars/3.
:- export syntax_option('$VAR').	% to print $VAR/1 terms as letters


numbervars('$VAR'(N), N, N1) :- !,
	N1 is N + 1.
numbervars(Term, N, Next) :-
	functor(Term, _, Arity),
	numbervars(0, Arity, Term, N, Next).

numbervars(Arity, Arity, _, N, Next) :- !, N = Next.
numbervars(I, Arity, Term, N0, N) :-
	I1 is I + 1,
        arg(I1, Term, Arg),
        numbervars(Arg, N0, N1),
        numbervars(I1, Arity, Term, N1, N).

