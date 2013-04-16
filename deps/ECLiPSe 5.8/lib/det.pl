% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: det.pl,v 1.3 2001/09/13 17:48:55 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 *	sccsid("%W%		%E%").
 *
 * IDENTIFICATION:	det.pl 
 *
 * DESCRIPTION: 	Determinacy Checker by David Bowen.
 *
 *			Modified for SEPIA by Joachim Schimpf and Micha Meier
 *
 *	The way to use this program is:
 *
 *	?- det(File).
 *
 *	where File is a file that you want checked.
 *	A suffix may be omitted, and a list of files may be specified
 *	instead of a single file
 *	
 *	This package includes indexing on first argument, however
 *	it does not check if the argument is instantiated on call,
 *	so it does not make much sense.
 *
 */

:- module(det).

:- export det/1.


:- import
    file_query/2
   from sepia_kernel.

det(X) :-
	var(X), !,
	error(4, det(X)).
det(user) :-
	!,
	get_prompt(input, Old, Out),
	set_prompt(input, " ", Out),
	det_file(user),
	set_prompt(input, Old, Out).
det(File) :-
	(string(File) ->			% first convert to a string
		FileS = File
	;
		atom(File) ->
		atom_string(File, FileS)
	;
		error(5, det(File))
	),
	(
		get_flag(prolog_suffix, Suffixes),
		member(Suffix, Suffixes),
		Suffix \== ".sd",
		concat_strings(FileS, Suffix, PlFile)
	;
		error(171, det(File))
	),
	exists(PlFile),
	!,
	atom_string(FileAtom, PlFile),
	open(FileAtom, read, input),
	det_file(FileAtom).
det([]) :- !.
det([File| Files]) :- !,
	det_file(File),
	det(Files).

det_file(File) :-
	printf("det checking:      %s%n", [File]),
	(	read_clauses(Pred, Clauses),
		process(Pred, Clauses),
		fail
	;
		true
	),
	close(input).


%
%	Returns a predicate in the form Name/Arity and a list of all its
%	clauses. This assumes that all the clauses for a predicate are
%	contiguous. If they are split, each clump will be treated as if
%	it were a seperate predicate.
%

read_clauses(Pred, Clauses) :-
	read_clause(Clause0),
	Clause0 \== end_of_file,
	read_clauses(Clause0, _InitPred, Pred, CList, CList, Clauses).

read_clauses(end_of_file, Pred, Pred, [], Clauses, Clauses) :- !.
read_clauses(Clause0, InitPred, Pred, [Clause0| Clauses0], CList, Clauses) :-
	pred_for_clause(Clause0, InitPred),
	!,
	read_clause(Clause1),
	read_clauses(Clause1, InitPred, Pred, Clauses0, CList, Clauses).
read_clauses(_Clause0, Pred, Pred, [], Clauses, Clauses). % non-determinate!
read_clauses(Clause0, _, Pred, _, _, Clauses) :-
	read_clauses(Clause0, _InitPred, Pred, CList, CList, Clauses).


%
%	Read a clause. Does expand_term, mainly to take care of grammar rules.
%

read_clause(Clause) :-
	read(X),
	(	directive(X, Z) ->
		file_query(Z, det),
		read_clause(Clause)
	;
		Clause = X
	).

directive((:- Directive), Directive).
directive((?- Directive), Directive).


%
%	Processes op declarations. Other directives are ignored
%	Obsoleted by file_query/2.
%

process_directive(op(P, T, O)) :- !,
	op(P, T, O).
process_directive((P, Q)) :- !,
	process_directive(P),
	process_directive(Q).
process_directive(_).


%
%	Process a predicate, one clause at a time.
%

process(Pred, Clauses) :-
	process(Clauses, 0, Pred).

process([], _, _).
process([Clause| Rest], I, Pred) :-
	J is I + 1,
	inter_clause_check(Clause, J,  Rest, Pred),
	clause_body(Clause, Body),
	intra_clause_check(Body, J, Pred),
	process(Rest, J, Pred).


%
%	Clause is deemed non-determinate if it contains a disjunction or
%	a repeat, subject to some exceptions.
%

intra_clause_check(Body, I, Pred) :-
	has_disjunction(Body),
	!,
	warn(Pred, I).
intra_clause_check(Body, I, Pred) :-
	has_repeat(Body),
	!,
	warn(Pred, I).
intra_clause_check(_Body, _I, _Pred).


%
%	Clause is deemed non-determinate unless
%		(1)   it contains a cut, or
%		(2)   its first argument is distinct in its principal functor
%		      from all following clauses, or
%		(3)   it ends with a 'fail'.
%

inter_clause_check(Clause, _I, _Clauses, _Pred) :-
	clause_body(Clause, Body),
	has_cut(Body),
	!.
inter_clause_check(Clause, _I, Clauses, _Pred) :-
	unique_index(Clause, Clauses),
	!.
inter_clause_check(Clause, _I, _Clauses, _Pred) :-
	clause_body(Clause, Body),
	ends_with_fail(Body),
	!.
inter_clause_check(_Clause, I, _Clauses, Pred) :-
	warn(Pred, I).


%
%	has_cut(Body) succeeds if Body contains a cut. A cut in one branch 
%	of an if-then-else is counted only if the other branch either
%	contains a cut also, ore else ends with fail. These conditions
%	guarantee that a cut must get executed if the if-then-else is to
%	succeed.
%

has_cut(-) :- !, fail.		% catch variables
has_cut(!).
has_cut((P , _Q)) :- has_cut(P), !.
has_cut((_P , Q)) :- has_cut(Q).
has_cut((_P->Q ; R)) :- has_cut(Q), !, cuts_or_fails(R).
has_cut((_P->Q ; R)) :- !, has_cut(R), cuts_or_fails(Q).
has_cut((P ; _Q)) :- has_cut(P), !.
has_cut((_P ; Q)) :- has_cut(Q).
has_cut((_P -> Q)) :- has_cut(Q).

cuts_or_fails(X) :- has_cut(X), !.
cuts_or_fails(X) :- ends_with_fail(X).


%
%	has_disjunction_body(Body) succeeeds if Body containts a disjunction.
%	There are four exeptions:
%		(1)   the disjunction is followed by a cut,
%		(2)   the disjunction is an if-then-else,
%		(3)   the left_hand side of the disjunction contains a cut, or
%		(4)   the left_hand side of the disjunction ends with 'fail'.
%

has_disjunction(-) :- !, fail.          % catch variables
has_disjunction((P , Q)) :- has_disjunction(P), \+ has_cut(Q), !.
has_disjunction((_P , Q)) :- has_disjunction(Q).
has_disjunction((_P -> Q ; _R)) :- has_disjunction(Q), !.
has_disjunction((_P -> _Q ; R)) :- !, has_disjunction(R).
has_disjunction((P ; _Q)) :- \+ has_cut(P), \+ ends_with_fail(P), !.
has_disjunction((P ; _Q)) :- has_disjunction(P), !.
has_disjunction((_P ; Q)) :- has_disjunction(Q).
has_disjunction((_P -> Q)) :- has_disjunction(Q).



%
%	has_repeat(Body) succeeds if Body containts a repeat, unless the
%	repeat is followed by a cut.
%

has_repeat(-) :- !, fail.          % catch variables 
has_repeat(repeat).
has_repeat((P , Q)) :- has_repeat(P), \+ has_cut(Q), !. 
has_repeat((_P , Q)) :- has_repeat(Q). 
has_repeat((_P -> Q ; _R)) :- has_repeat(Q), !.
has_repeat((_P -> _Q ; R)) :- !, has_repeat(R).
has_repeat((P ; _Q)) :- has_repeat(P), !.
has_repeat((_P ; Q)) :- has_repeat(Q).  
has_repeat((_P -> Q)) :- has_repeat(Q). 



%
%	ends_with_fail(Body) :- succeeds if the last goal in body is 'fail'.
%

ends_with_fail(-) :- !, fail.          % catch variables
ends_with_fail(fail).
ends_with_fail(false).
ends_with_fail((_P , Q)) :- ends_with_fail(Q).
ends_with_fail((P ; Q)) :- ends_with_fail(P), ends_with_fail(Q).
ends_with_fail((_P -> Q)) :- ends_with_fail(Q).


%
%	Succeeds if the key of a for Clause can not be unified with that
%	of any of the Clauses.
%

unique_index(Clause, Clauses) :-
	key(Clause, Key),
	member(X, Clauses),
	key(X, Key),
	!,
	fail.
unique_index(_Clause, _Clauses).


%
%	Returns the key of a given clause, that is,  the principal functor
%	of its first argument. If the first argument is a variable, Key is
%	a Variable.
%

key(Clause, Key) :-
	clause_head(Clause, Head),
	functor(Head, _, Arity),
	Arity >= 1,
	arg(1, Head, Key0),
	(	var(Key0) ->
		Key = Key0
	;
		functor(Key0, Name, KeyArity),
		functor(Key, Name, KeyArity)
	).


%
%	Obtaining the Head, Body and Predicate of a Clause
%

clause_head((H :-  _B), H) :- nonvar(H), !.
clause_head((H ?-  _B), H) :- nonvar(H), !.
clause_head(Fact, Fact).

clause_body((H :-  B), B) :- nonvar(H), !.
clause_body((H ?-  B), B) :- nonvar(H), !.
clause_body(_, true).

pred_for_clause(Clause, Name/Arity) :-
	clause_head(Clause, Head),
	functor(Head, Name, Arity).


%
%	Print a warning message.
%

warn(Pred, I) :-
	printf("non-determinate: %q (clause %d)%n", [Pred, I]).



