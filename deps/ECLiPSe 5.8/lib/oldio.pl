% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: oldio.pl,v 1.5 2001/11/16 18:48:22 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * sccsid("%W%		%E%").
 * sccscr("%Z%  Copyright 1989 ECRC GmbH ").
 */

/*
 * IDENTIFICATION:	oldio.pl
 *
 * DESCRIPTION: 	Contains built-in predicates to handle the I/O
 *			in the old BSI way, where the stream argument
 *			is the last one.
 *
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 * AUTHOR	VERSION	DATE	REASON
 */

:- module(oldio).
:- system.

:- export
	at/2,
	display/2,
	get/2,
	get0/2,
	get_char/2,
	print/2,
	put/2,
	put_char/2,
	read/2,
	readvar/3,
	rtoken/2,
	seek/2,
	tyi/2,
	tyo/2,
	write/2,
	writeclause/2,
	writeln/2,
	writeq/2.

:- import
	print_/3,
	read_/3,
	readvar/4,
	writeq_/3,
	write_/3
    from sepia_kernel.

% Tools
print_body(Term, S, M) :- print_(S, Term, M).

  readvar_body(Term, Var, S, M) :- readvar(S, Term, Var, M).

  read_body(Term, S, M) :- read_(S, Term, M).

write_body(Term, S, M) :- write_(S, Term, M).

writeq_body(Term, S, M) :- writeq_(S, Term, M).

% Others
at(P, S) :- eclipse_language:at(S, P).

display(T, S) :- eclipse_language:display(S, T).

get(X, S) :- eclipse_language:get(S, X).

get0(X, S) :- eclipse_language:get(S, X).

get_char(X, S) :- eclipse_language:get_char(S, X).

put(X, S) :- eclipse_language:put(S, X).

put_char(X, S) :- eclipse_language:put_char(S, X).

rtoken(Token, S) :- eclipse_language:read_token(S, Token, _).

seek(O, S) :- eclipse_language:seek(S, O).

tyi(X, S) :- eclipse_language:tyi(S, X).

tyo(X, S) :- eclipse_language:tyo(S, X).

writeclause(S, C) :- eclipse_language:writeclause(C, S).

writeln(T, S) :- eclipse_language:writeln(S, T).

:-  % must be after the bodies in order to inherit the system flag
    tool(print/2, print_body/3),
    tool(readvar/3, readvar_body/4),
    tool(read/2, read_body/3),
    tool(write/2, write_body/3),
    tool(writeq/2, writeq_body/3).

:- skipped
	at/2,
	display/2,
	get/2,
	get0/2,
	get_char/2,
	print/2,
	put/2,
	put_char/2,
	read/2,
	readvar/3,
	rtoken/2,
	seek/2,
	tyi/2,
	tyo/2,
	write/2,
	writeclause/2,
	writeln/2,
	writeq/2.

