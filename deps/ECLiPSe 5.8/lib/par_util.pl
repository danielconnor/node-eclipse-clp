% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: par_util.pl,v 1.5 2001/09/13 17:48:57 js10 Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% sccsid("%W%             %E%").
% sccscr("%Z%  Copyright 1994 ECRC GmbH ").
%
% IDENTIFICATION:	par_util.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:
%			par_member(?Element, +List)
%			par_delete(?Element, +List, ?Rest)
%			par_between(+From, +To, ?I)
%			par_maplist(+Pred, +ListIn, ?ListOut)
%
%			statistics_par
%			statistics_par_reset
%
%			Goal1 & Goal2
%
% DESCRIPTION:		Parallel versions of various predicates.
%			The semantics of par_member/2, par_maplist/3
%			and &/2 is not exactly the same as the
%			corresponding sequential version.
%

:- module(par_util).
:- pragma(nodebug).

:- comment(summary, "Parallel versions of various predicates").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2001/09/13 17:48:57 $").
:- comment(par_member/2, [template:"par_member(?Element, +List)",
    summary:"Parallel version of member/2",
    desc:html("Parallel version of member/2, i.e.  selects elements from
    the given list in parallel.  Note that it cannot work backwards
    and generate lists like member/2 can, the list must be a proper list.")]).
:- comment(par_delete/3, [template:"par_delete(?Element, ?List, ?Rest)",
    summary:"Parallel version of delete/3"]).
:- comment(par_between/3, [template:"par_between(+From, +To, ?I)",
    summary:"Generates integers between From and To in parallel",
    see_also:[fork/2, between/3, between/4]]).
:- comment(par_maplist/3, [template:"par_maplist(+Pred, +In, ?Out)",
    summary:"Parallel version of maplist/3",
    desc:html("Parallel version of maplist/3.  The semantics is not
    exactly the same as maplist/3:  It does not work backwards and it
    does not cope with aliasing between the In and the Out list, since
    it is implemented on top of findall/3.  There will only be a
    performance gain if the mapping predicate does enough computation
    to make the overhead pay off."),
    see_also:[maplist/3]]).
:- comment((&)/2, [template:"Goal1 & Goal2",
    summary:"Parallel AND operator implemented on top of OR-parallelism",
    desc:html("Parallel AND operator implemented on top of
    OR-parallelism.  This will only pay off for sufficiently
    coarse-grained computations in Goal1 and Goal2.")]).

:- export
	(&)/2,
	par_between/3,
	par_delete/3,
	par_maplist/3,
	par_member/2,
	statistics_par/0,
	statistics_par_reset/0.

:- tool(par_maplist/3).

:- export op(950, xfy, (&)).


:- import worker_statistics/2, worker_statistics_reset/1 from sepia_kernel.

% Parallel member(?, +List), it can't generate lists!

par_member(X, List) :-
	List = [_|_],
	Arr =.. [arr|List],
	functor(Arr, arr, N),
	N1 is N+1,
	fork(N, I),
	I1 is N1-I,
	arg(I1, Arr, X).


:- parallel par_delete/3.
par_delete(A, [A|C], C).
par_delete(A, [B|C], [B|D]) :-
	par_delete(A, C, D).


par_between(From, To, X) :-
	To1 is To+1,
	N is To1-From,
	N > 0,
	fork(N, I),
	X is To1-I.


:- tool((&)/2, '&_body'/3).

'&_body'(Goal1, Goal2, Module) :-
	findall(Sol, parand(Sol, Goal1, Goal2, Module), Bag),
	member(a-Goal1, Bag),
	member(b-Goal2, Bag).

:- parallel parand/4.
:- mode parand(-,+,+,+).
parand(a-Goal1, Goal1, _, Module) :- call(Goal1, Module).
parand(b-Goal2, _, Goal2, Module) :- call(Goal2, Module).


:- tool(par_maplist/3, par_maplist_body/4).

par_maplist_body(Pred, In, Out, Module) :-
	findall(Sol, map_elements(Pred, In, Sol, Module), Out0),
	sort(1, >=, Out0, Out1),
	strip_key(Out1, Out).

map_elements(Pred, In, I-Xout, Module) :-
	Pred =.. PL,
	append(PL, [Xin, Xout], NewPred),
	Call =.. NewPred,
	InArr =.. [in|In],
	functor(InArr, in, N),
	N1 is N+1,
	fork(N, I),
	I1 is N1-I,
	arg(I1, InArr, Xin),
	( call(Call, Module), true -> true ).

strip_key([], []).
strip_key([_-X|Xs], [X|Ys]) :- strip_key(Xs, Ys).


% Parallel statistics

statistics_par :-
	writeln(" Wrkr Jobs Prun Published Copy      Copied  Idling Working Copying Scheduling"),
	writeln("   ID    #    # cpts alts    #       bytes      ms      ms      ms      ms\n"),
	get_flag(workerids, Host:AliveIds+SleepIds),
	(member(Wid, AliveIds) ; member(Wid, SleepIds)),
	worker_statistics(Wid, Data),
	arg(1, Data, Jobs),
	arg(2, Data, Prunes),
	arg(4, Data, CopyFromCnt),
	arg(5, Data, CopyFromBytes),
	arg(8, Data, Publish),
	arg(9, Data, PubChpts),
	arg(10, Data, PubAlts),
	IdleMs is arg(14, Data) + arg(23, Data),
	WorkMs is arg(15, Data),
	CopyMs is arg(17, Data) + arg(18, Data) + arg(22, Data),
	SchedMs is arg(16, Data) + arg(19, Data) + arg(20, Data) + arg(21, Data),
	printf("%5d%5d%5d%5d%5d%5d%12d%8.0f%8.0f%8.0f%8.0f\n",
		[Wid, Jobs, Prunes, PubChpts, PubAlts, CopyFromCnt,
		 CopyFromBytes, IdleMs, WorkMs, CopyMs, SchedMs]),
	fail.
statistics_par.

statistics_par_reset :-
	get_flag(workerids, Host:AliveIds+SleepIds),
	(member(Wid, AliveIds) ; member(Wid, SleepIds)),
	worker_statistics_reset(Wid),
	fail.
statistics_par_reset.

