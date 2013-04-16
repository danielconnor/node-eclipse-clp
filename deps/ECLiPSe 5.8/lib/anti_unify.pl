% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: anti_unify.pl,v 1.3 2000/04/11 11:58:55 js10 Exp $
% ----------------------------------------------------------------------

%
% ECLIPSE PROLOG LIBRARY MODULE
%
% sccsid("%W%             %E%").
% sccscr("%Z%  Copyright 1993 ECRC GmbH ").
%
% IDENTIFICATION:	anti_unify.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		anti_unify/3
%
% DESCRIPTION:		anti_unify(Term1, Term2, General)
%			Computes the most specific generalization of two
%			terms in n*log(n) time, where n is the size of
%			the smaller term.
%

:- module(anti_unify).
:- export anti_unify/3.

:- comment(summary, "Computes the most specific generalization of two terms").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2000/04/11 11:58:55 $").

:- comment(anti_unify/3, [
    template:"anti_unify(Term1, Term2, General)",
    summary:"Computes the most specific generalization of two terms in
    N*log(N) time, where N is the size of the smaller term.",
    eg:"
    [eclipse 10]: anti_unify(a, b, X).
    X = _65
    yes.

    [eclipse 11]: anti_unify(a, a, X).
    X = a
    yes.

    [eclipse 9]: anti_unify(foo(a,b,c), foo(b,b,b), X).
    X = foo(_115, b, _98)
    yes.

    [eclipse 8]: anti_unify(foo(a,a,a), foo(b,b,b), X).
    X = foo(_98, _98, _98)
    yes.
    "]).


anti_unify(A, B, G) :-
	map(A, B, G, [], Map),
	sort(0, =<, Map, SortedMap),
	unify_duplicates(SortedMap).

:- mode map(?,?,?,+,-).

map(A, B, G, Map, NewMap) :-
	atomic(A), atomic(B),
	A = B,
	!,
	G = A,
	NewMap = Map.
map(A, B, G, Map0, Map) :-
	nonvar(A), A=[AH|AT],
	nonvar(B), B=[BH|BT],
	!,
	G = [GH|GT],
	map(AH, BH, GH, Map0, Map1),
	map(AT, BT, GT, Map1, Map).
map(A, B, G, Map, NewMap) :-
	compound(A),
	compound(B),
	functor(A, Name, Arity),
	functor(B, Name, Arity),
	!,
	functor(G, Name, Arity),
	map_arg(A, B, G, Map, NewMap, Arity).
map(A, B, G, Map, [subst(A, B, G)| Map]).

:- mode map_arg(?,?,?,+,-,+).

map_arg(_, _, _, Map, Map, 0) :-
	!.
map_arg(A, B, G, Map0, NewMap, N) :-
	arg(N, A, An),
	arg(N, B, Bn),
	arg(N, G, Gn),
	map(An, Bn, Gn, Map0, Map1),
	N1 is N-1,
	map_arg(A, B, G, Map1, NewMap, N1).

:- mode unify_duplicates(+), unify_duplicates(+,+).

unify_duplicates([]).
unify_duplicates([H|T]) :-
	unify_duplicates(H, T).

unify_duplicates(_, []).
unify_duplicates(subst(A1, B1, G1), [H|T]) :-
	H = subst(A2, B2, G2),
	( A1 == A2, B1 == B2 -> G1 = G2 ; true ),
	unify_duplicates(H, T).

