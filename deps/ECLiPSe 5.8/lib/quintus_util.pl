% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: quintus_util.pl,v 1.2 1999/08/07 15:50:37 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 *	sccsid("%W%		%E%").
 *	sccscr("%Z%  Copyright 1990 ECRC GmbH ").
 *
 * IDENTIFICATION:	quintus_util.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


:- module(quintus_util).

:- export q_prompt/2, redef_handler/2, end_compile_handler/2.

%
% predicate to obtain a Quintus-like prompt
%

q_prompt(_, Module) :-
    get_flag(debugging, Dbg),
    debug_mode(Dbg, Debug),
    (Debug == nodebug ->
        (Module == eclipse ->
            true
        ;
            printf(toplevel_output, "[%s]\n", Module)
        )
    ;

        put(toplevel_output, 0'[),
        (Module == eclipse ->
            true
        ;
            printf(toplevel_output, "%s ", Module)
        ),
        printf(toplevel_output, "%s]\n", Debug)
    ),
    write(toplevel_output, '| ?- '),
    flush(toplevel_output).

debug_mode(leap, debug).
debug_mode(nodebug, nodebug).
debug_mode(creep, trace).



% A flag to suppress the warnings
:- setval(pflag, 0).

redef_handler(_, (Proc, OldFile, NewFile)) :-
    (getval(pflag, 1) ->
        true
    ;
        printf("Procedure %w is being redefined in a different file\n",
            Proc),
        printf("    Previous file: %s\n    New file:      %s\n",
            [OldFile, NewFile]),
        printf("Do you want to redefine it? (y, n or p) %b", []),
        tyi(X),
        (X == 0'y ->
            writeln(yes)
        ;
        X == 0'p ->
            writeln('suppress warnings'),
            setval(pflag, 1)
        ;
            writeln(no),
            fail
        )
    ).

% At the file end reset the flag
end_compile_handler(A, B) :-
    setval(pflag, 0),
    error(default(A), B).

