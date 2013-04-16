% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: pretty_print.pl,v 1.3 2000/04/11 12:00:11 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 *	sccsid("%W%		%E%").
 *	sccscr("%Z%  Copyright 1990 ECRC GmbH ").
 *
 * IDENTIFICATION:	pretty_print.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


:- module(pretty_print).

:- comment(summary, "Pretty-printing of complex terms").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2000/04/11 12:00:11 $").
:- comment(pretty_print/3, [template:"pretty_print(+Stream, +Term, +Max)",
    summary:"Print a term on the given stream, split it if its width exceeds Max"
    ]).

:- export pretty_print/3.

% Print a term on the given stream, split it if its size exceeds Max
pretty_print(Stream, Term, Max) :-
        open(_, string, S),
        pp(S, Term, 0, Max),
        current_stream(String, _, S),
        close(S),
        write(Stream, String).

% First try to print the term using write/2, if it is too big,
% split it onto separate lines
pp(S, Term, Offset, MaxSize) :-
        at(S, Start),
        write(S, Term),
        (at(S) < Start + MaxSize ->
            true
        ;
            seek(S, Start),      % rewind the output
            NewOffset is Offset + 3,
            functor(Term, F, N),
            printf(S, "%a(\n", F),
            pp_arg(S, 1, N, Term, NewOffset, MaxSize),
            printf(S, "%*c)", [Offset, 0' ])
        ).

pp_arg(S, N, N, Term, Off, MaxSize) :-
        !,
        printf(S, "%*c", [Off, 0' ]),
        arg(N, Term, Arg),
        pp(S, Arg, Off, MaxSize),
        nl(S).
pp_arg(S, I, N, Term, Off, MaxSize) :-
        printf(S, "%*c", [Off, 0' ]),
        arg(I, Term, Arg),
        pp(S, Arg, Off, MaxSize),
        write(S, ',\n'),
        I1 is I + 1,
        pp_arg(S, I1, N, Term, Off, MaxSize).

