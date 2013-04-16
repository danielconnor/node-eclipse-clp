% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: scattered.pl,v 1.11 2004/11/19 14:54:48 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 *	sccsid("%W%		%E%").
 *	sccscr("%Z%  Copyright 1990 ECRC GmbH ").
 *
 * IDENTIFICATION:	scattered.pl 
 *
 * AUTHOR:		Joachim Schimpf
 * MODIFICATIONS:	Micha Meier - works even without declarations,
 *				by re-compiling the source.
 *
 * DESCRIPTION: 	Allow a source file to contain non-consecutive
 *			clauses for a procedure (to avoid error 134).
 *			This is done by declaring the predicate temporarily
 *			as dynamic, and recompiling it as a static predicate
 *			when the compiler reaches the end of the file.
 *			When no declaration is given, the handler for
 *			non-consecutive clauses tries to find the location
 *			of the first procedure block, abolish it,
 *			declare as dynamic and recompiling the first block.
 *
 * CONTENTS:		scattered +SpecList.
 *
 * USAGE:		Add to the source file directives of the form
 *
 *				:- scattered Name/Arity [, ...] .
 *
 *			for every (static) procedure that has non-consecutive
 *			clauses in the file. This declaration must precede
 *			any clauses of the procedure.
 *			Unless the clauses are compiled from a term, a pipe
 *			or other volatile source, the directives may
 *			be omitted, which makes the source more portable.
 *
 * PROBLEMS:	-	Does not work for predicates with delay clauses.
 *		-	In the current implementation, all information
 *			about the predicate's source location is lost.
 */

:- module(scattered).

:- comment(summary, "Allow a source file to contain non-consecutive clauses").
:- comment(author, "Joachim Schimpf and Micha Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2004/11/19 14:54:48 $").
:- comment(status, deprecated).
:- comment(desc, html("
    The ECLiPSe compiler does not allow the clauses for static predicates
    being non-consecutive, i.e.  interleaved with clauses for other
    predicates.  The event 134 \"procedure clauses are not consecutive\" is
    raised in such a case.  The correct way to avoid this error is to use
    the discontiguous/1 declaration (ISO-Prolog compatible).
    <P>
    This library is therefore mostly obsolete!
    <P>
    It can still be useful for very old Prolog programs, which have been
    written for an interpreted system, and expect non-consecutive predicates
    to be generally allowed (without declaration).  It is enough to load
    this library before compiling such a source file.  It redefines the
    handler for the event 134 in such a way that the procedures with
    non-consecutive clauses are recompiled in one chunk after encountering
    the end of the file. 
    <P>
    When not compiling from a file, the non-consecutive clauses have to be
    declared using the directive scattered/1.  This declaration has to
    precede any clauses of the predicate. 
    <PRE>
    :- lib(scattered).
    :- scattered p/3, q/1.
    </PRE>
    Note that this applies to predicates whose clauses are
    non-consecutive, but in a single file.  Predicates that are spread
    over multiple files still have to be declared as dynamic.
    ")).


:- comment((scattered)/1, [
    amode:scattered(++),
    args:["PredSpec":"Structure of the form Name/Arity, or a comma-separated sequence of those"],
    see_also:[discontiguous/1],
    summary:"This declaration is obsolete, use discontiguous/1 instead."]).


:- export op(1000, fy, scattered).

:- export (scattered)/1.
:- tool((scattered)/1).

:- import
	abolish_body/2,
	assert_/2,
	clause_body/3,
	compile_term_/2,
	dynamic_body/2,
	export_body/2,
	get_flag_body/4,
	global_body/2,
	local_body/2,
	mode_/2,
	read_/3,
	set_flag_body/4
   from sepia_kernel.

:- local record(scattered_pred).

%
% the scattered/1 declaration
%
:- tool((scattered)/1, scattered_body/2).

scattered_body((Pred, Preds), M) :-
	!,
	scattered_body(Pred, M),
	scattered_body(Preds, M).
scattered_body(Pred, M) :-
	get_flag_body(Pred, defined, on, M),
	get_flag_body(Pred, stability, dynamic, M),
	!,
	printf(warning_output,
		'warning: declaration ignored for dynamic predicate: %w\n%b',
		scattered(Pred)).
scattered_body(Pred, M) :-
	(get_flag_body(Pred, defined, on, M) ->
	    findall(Flag-Val, get_flag_body(Pred, Flag, Val, M), OldFlags),
	    abolish_body(Pred, M),
	    dynamic_body(Pred, M),
	    set_old_flags(Pred, OldFlags, M)
	;
	    dynamic_body(Pred, M)
	),
	recorda(scattered_pred, .(Pred, M)).

%
% handlers for compilation events
%

% We want to avoid that loading lib(scattered) has global side effects
% on all modules, but unfortunately, the handler settings are global.
% We therefore check now in every handler call whether it was called
% from a module that actually wants to allow non-consecutive predicates.
% Assume these are all modules which import a 'permissive_language'.

permissive_language(scattered).
permissive_language(cprolog).
permissive_language(quintus).
permissive_language(sicstus).


non_permissive_module(M) :- var(M).
non_permissive_module(M) :- nonvar(M),
	get_module_info(M, imports, Imports),
	\+ (member(Import, Imports), permissive_language(Import)).


start_compile_file(E, G, M) :-
	non_permissive_module(M), !,
	error(default(E), G, M).
start_compile_file(E, G, M) :-
	recorda(scattered_pred, '.'),
	error(default(E), G, M).

end_compile_file(E, G, M) :-
	non_permissive_module(M), !,
	error(default(E), G, M).
end_compile_file(E, G, M) :-
	( G = (term,_) ->
	    true
	;
	is_record(scattered_pred) ->	% may have been set on the fly
	    recompile_scattered_preds
	;
	    true
	),
	error(default(E), G, M).

abort_compile_file(E, G, M) :-
	non_permissive_module(M), !,
	error(default(E), G, M).
abort_compile_file(E, G, M) :-
	(is_record(scattered_pred) ->	% may have been set on the fly
	    recompile_scattered_preds
	;
	    true
	),
	error(default(E), G, M).

recompile_scattered_preds :-
	(erase(scattered_pred, R),
	 R = .(Pred, M) ->
	    recompile(Pred, M),
	    recompile_scattered_preds
	;
	    true
	).

% Handler for the non-consecutive event.
non_consecutive_handler(N, Pred, M) :-
    non_permissive_module(M), !,
    error(default(N), Pred, M).
non_consecutive_handler(_, Pred, M) :-
    get_flag_body(Pred, source_file, File, M),
    get_file_info(File, mode) /\ 8'400 =:= 8'400,
    get_flag_body(Pred, source_offset, Offset, M),
    !,
    findall(Flag-Val, get_flag_body(Pred, Flag, Val, M), OldFlags),
    abolish_body(Pred, M),
    dynamic_body(Pred, M),
    set_old_flags(Pred, OldFlags, M),
    open(File, read, Stream),
    seek(Stream, Offset),
    input_clauses(Stream, Pred, M),
    recorda(scattered_pred, .(Pred, M)).
non_consecutive_handler(N, Pred, M) :-
    % The source file is not available, make the default error
    error(default(N), Pred, M).

input_clauses(Stream, Proc, Module) :-
    read_(Stream, Clause, Module),
    (clause_proc(Clause, Proc) ->
	assert_(Clause, Module),
	input_clauses(Stream, Proc, Module)
    ;
	close(Stream)
    ).

clause_proc((Head :- _), F/A) :-
    !,
    functor(Head, F, A).
clause_proc(Fact, F/A) :-
    functor(Fact, F, A).

% recompile(Pred, Module)
% Collect all clauses and flags of the predicate,
% then abolish it, recompile it statically (using compile_term)
% and restore the old visibility and other flags

recompile(Pred, M) :-
	Pred = N/A,
	functor(Head, N, A),
	findall(Clause, my_clause(Head, Clause, M), Clauses),
	findall(Flag, get_pre_flag(Pred, Flag, M), PreFlags),
	findall(Flag, get_post_flag(Pred, Flag, M), PostFlags),
	abolish_body(Pred, M),
	(Clauses == [] ->
	    printf(warning_output,
		'warning: no clauses for scattered predicate %w\n%b', Pred)
	;
	    set_old_flags(Pred, PreFlags, M),
	    compile_term_(Clauses, M),
	    set_old_flags(Pred, PostFlags, M)
	).

my_clause(Head, Clause, M) :-
	clause_body(Head, Body, M),
	(Body == true -> Clause = Head ; Clause = (Head :- Body)).

set_old_flags(_Pred, [], _M).
set_old_flags(Pred, [Flag-Val|Flags], M) :-
	set_old_flag(Pred, Flag, Val, M),
	set_old_flags(Pred, Flags, M).

set_old_flag(_Pred, visibility, local, _M) :- !.
set_old_flag(Pred, visibility, global, M) :- !,
	global_body(Pred, M).
set_old_flag(Pred, visibility, exported, M) :- !,
	export_body(Pred, M).
set_old_flag(Pred, leash, X, M) :- !,
	set_flag_body(Pred, leash, X, M).
set_old_flag(Pred, skip, X, M) :- !,
	set_flag_body(Pred, skip, X, M).
set_old_flag(Pred, spy, X, M) :- !,
	set_flag_body(Pred, spy, X, M).
set_old_flag(Pred, statistics, on, M) :- !,
	set_flag_body(Pred, statistics, on, M).
set_old_flag(_Pred, mode, Mode, M) :- !,
	mode_(Mode, M).
set_old_flag(_, _, _, _).

get_pre_flag(Pred, Flag-Val, M) :-
	Flag = mode,
	get_flag_body(Pred, Flag, Val, M).

get_post_flag(Pred, Flag-Val, M) :-
	get_flag_body(Pred, Flag, Val, M),
	Flag \= mode.

%
% set up the handlers for compilation events
%
:- set_event_handler(146, start_compile_file/3).
:- set_event_handler(139, end_compile_file/3).
:- set_event_handler(147, abort_compile_file/3).
:- set_event_handler(134, non_consecutive_handler/3).
:- error(146, _).		% call it once for this file
