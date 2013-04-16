% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: ordset.pl,v 1.4 2000/06/05 13:02:45 js10 Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: ordset.pl,v 1.4 2000/06/05 13:02:45 js10 Exp $
%
% IDENTIFICATION:       ordset.pl
%
% AUTHOR:		Joachim Schimpf, IC-Parc, Imperial College, London
%
% DESCRIPTION:
%
%	This is a drop-in replacement for R.A.O'Keefe's ordset library.
%	The predicates have all been rewritten for optimal indexing
%	and garbage avoidance when used within ECLiPSe. I have kept
%	the original interface and comments.


%   File   : ORDSET.PL
%   Author : R.A.O'Keefe
%   Updated: 22 May 1983
%   Purpose: Ordered set manipulation utilities

%   In this module, sets are represented by ordered lists with no
%   duplicates.  Thus {c,r,a,f,t} would be [a,c,f,r,t].  The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  Some of the unordered
%   set routines, such as member/2, length/2,, select/3 can be used
%   unchanged.  The main difficulty with the ordered representation is
%   remembering to use it!


:- module(ordset).

:- comment(summary, "Ordered set manipulation utilities").
:- comment(author, "R.A.O'Keefe and Joachim Schimpf").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2000/06/05 13:02:45 $").
:- comment(desc, html("\
	This library implements sets as ordered lists.  The benefit of the
	ordered representation is that the elementary set operations can
	be done in time proportional to the Sum of the argument sizes
	rather than their Product.  Some of the unordered set routines,
	such as member/2, length/2,, select/3 can be used unchanged.
    ")).

:- export
	list_to_ord_set/2,	%  List -> Set
	ord_disjoint/2,		%  Set x Set ->
	ord_insert/3,		%  Set x Elem -> Set
	ord_intersect/2,	%  Set x Set ->
	ord_intersect/3,	%  Set x Set -> Set
	ord_seteq/2,		%  Set x Set ->
	ord_subset/2,		%  Set x Set ->
	ord_subtract/3,		%  Set x Set -> Set
	ord_symdiff/3,		%  Set x Set -> Set
	ord_union/3.		%  Set x Set -> Set

:- mode
	list_to_ord_set(+, ?),
	ord_disjoint(+, +),
	ord_insert(+, +, ?),
	ord_intersect(+, +),
	ord_intersect(+, +, ?),
	ord_seteq(+, +),
	ord_subset(+, +),
	ord_subtract(+, +, ?), 
	ord_symdiff(+, +, ?),
	ord_union(+, +, ?).


:- comment(list_to_ord_set/2, [
    amode:list_to_ord_set(+,?),
    args:["List":"A list of terms", "Set":"A set or variable"],
    summary:"Converts a list to a set",
    desc:html("\
	Succeeds when Set is the ordered representation of the set
	represented by the unordered representation List.  The only
	reason for giving it a name at all is that you may not have
	realised that sort/2 could be used this way."
    )]).

list_to_ord_set(List, Set) :-
	sort(0, <, List, Set).


:- comment(ord_disjoint/2, [
    amode:ord_disjoint(+,+),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether two sets are disjoint",
    desc:html("\
	Succeeds when the two ordered sets have no element in common.
    ")]).

ord_disjoint([], _).
ord_disjoint(S1, S2) :-
	S1=[_|_],
	ord_disjoint1(S1, S2).

:- mode	ord_disjoint1(+, +).
ord_disjoint1(_, []).
ord_disjoint1(EL1, EL2) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    fail
	; E1 @< E2 ->
	    ord_disjoint(L1, EL2)
	;
	    ord_disjoint(EL1, L2)
	).



:- comment(ord_insert/3, [
    amode:ord_insert(+,+,?),
    args:["Set1":"A set", "Element":"A term", "Set2":"A set or variable"],
    summary:"Adds an element to a set",
    desc:html("\
	Set2 is the set resulting from adding Element to Set1. It should
	give exactly the same result as merge(Set1, [Element], Set2).
    ")]).

ord_insert([], E, [E]).
ord_insert(EL1, E, L) :-
	EL1 = [E1|L1],
	( E1 @< E ->
	    L = [E1|L0],
	    ord_insert(L1, E, L0)
	; E1==E ->
	    L = EL1
	;
	    L = [E|EL1]
	).



:- comment(ord_intersect/2, [
    amode:ord_intersect(+,+),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether two sets have a non-empty intersection",
    desc:html("\
	Succeeds when the two ordered sets have at least one element
	in common.  Note that the test is == rather than = .
    ")]).

ord_intersect(EL1, EL2) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    true
	; E1 @< E2 ->
	    ord_intersect(L1, EL2)
	;
	    ord_intersect(EL1, L2)
	).



:- comment(ord_intersect/3, [
    amode:ord_intersect(+,+,?),
    args:["Set1":"A set", "Set2":"A set", "Intersection":"A set"],
    summary:"Computes the intersection of two sets",
    desc:html("\
	Succeeds when Intersection is the intersection of Set1 
	and Set2, provided that Set1 and Set2 are ordered sets.
    ")]).

ord_intersect([], _, []).
ord_intersect(S1, S2, L) :-
	S1=[_|_],
	ord_intersect1(S1, S2, L).

:- mode	ord_intersect1(+, +, ?).
ord_intersect1(_, [], []).
ord_intersect1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    L = [E1|L0],
	    ord_intersect(L1, L2, L0)
	; E1 @< E2 ->
	    ord_intersect(L1, EL2, L)
	;
	    ord_intersect(EL1, L2, L)
	).



:- comment(ord_seteq/2, [
    amode:ord_seteq(+,+),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Compares two sets",
    desc:html("\
	Succeeds when the two arguments represent the same set.  Since they
	are assumed to be ordered representations, they must be identical.
    ")]).

ord_seteq(Set1, Set2) :-
	Set1 == Set2.



:- comment(ord_subset/2, [
    amode:ord_subset(+,+),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether Set1 is a subset of Set2",
    desc:html("\
	Succeeds when every element of the ordered set Set1 appears
	in the ordered set Set2.
    ")]).

ord_subset([], _).
ord_subset(EL1, EL2) :-
	EL1 = [E1|L1],
	EL2 = [E2|L2],
	( E1==E2 ->
	    ord_subset(L1, L2)
	; E1 @> E2 ->
	    ord_subset(EL1, L2)
	;
	    fail
	).




:- comment(ord_subtract/3, [
    amode:ord_subtract(+,+,?),
    args:["Set1":"A set", "Set2":"A set", "Difference":"A set or variable"],
    summary:"Subtracts Set2 from Set1",
    desc:html("\
	Succeeds when Difference contains all and only the elements
	of Set1 which are not also in Set2.
    ")]).

ord_subtract([], _, []).
ord_subtract(S1, S2, L) :-
	S1=[_|_],
	ord_subtract1(S1, S2, L).

:- mode	ord_subtract1(+, +, ?).
ord_subtract1(L, [], L).
ord_subtract1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    ord_subtract(L1, L2, L)
	; E1 @< E2 ->
	    L = [E1|L0],
	    ord_subtract(L1, EL2, L0)
	;
	    ord_subtract(EL1, L2, L)
	).



:- comment(ord_symdiff/3, [
    amode:ord_symdiff(+,+,?),
    args:["Set1":"A set", "Set2":"A set", "Difference":"A set or variable"],
    summary:"Computes the symmetric difference of Set1 and Set2",
    desc:html("\
	Succeeds when Difference is the symmetric difference of Set1 and Set2.
    ")]).

ord_symdiff([], L, L).
ord_symdiff(S1, S2, L) :-
	S1=[_|_],
	ord_symdiff1(S1, S2, L).

:- mode	ord_symdiff1(+, +, ?).
ord_symdiff1(L, [], L).
ord_symdiff1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    ord_symdiff(L1, L2, L)
	; E1 @< E2 ->
	    L = [E1|L0],
	    ord_symdiff(L1, EL2, L0)
	;
	    L = [E2|L0],
	    ord_symdiff(EL1, L2, L0)
	).



:- comment(ord_union/3, [
    amode:ord_union(+,+,?),
    args:["Set1":"A set", "Set2":"A set", "Union":"A set or variable"],
    summary:"Computes the union of Set1 and Set2",
    desc:html("\
	Succeeds when Union is the union of Set1 and Set2.  Note that when
	something occurs in both sets, we want to retain only one copy.
    ")]).

ord_union(Set1, Set2, Union) :-
	merge(0, <, Set1, Set2, Union).

