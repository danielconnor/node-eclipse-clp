% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: cio.pl,v 1.5 2001/02/13 18:29:54 wh Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * sccsid("%W%		%E%").
 * sccscr("%Z%  Copyright 1989 ECRC GmbH ").
 */

/*
 * IDENTIFICATION:	cio.pl
 *
 * DESCRIPTION: 	Contains built-in predicates to handle the I/O
 *			in the C-prolog way.
 *
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 * AUTHOR	VERSION	DATE	REASON
 * Micha Meier	2.4	14.2.90	
 */

:- module(cio).

:- comment(summary, "Predicates for C-Prolog style I/O (see/tell family)").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2001/02/13 18:29:54 $").
:- comment(desc, html("
    This library provides C-Prolog compatible I/O predicates. It is included
    in the C-Prolog compatibility package, but can be used independently.
    The predicates change ECLiPSe's input or output stream, respectively.")).

:- export
	see/1,
	seeing/1,
	seen/0,
	skip/1,
	skip/2,
	tab/1,
	tab/2,
	tell/1,
	telling/1,
	told/0.

:- pragma(nodebug).


% Files opened by see/tell are characterised by having a stream
% alias that is identical to the name of the opened file.

% We actually allow a bit of a mixture between old style I/O and
% new style I/O, because that is used in some tests and seems to
% be done in Sicstus as well. E.g. see/1 and tell/1 can be called
% with any stream number or stream name, even one that has not been
% created with a see/tell. Also, seeing/1 and telling/1 return
% stream numbers if there is no corresponding see/tell file.

stream_to_cprolog_name(Stream, File) :-
	get_stream_info(Stream, name, File0),
	( File0 = user ->
	    File = File0
	; current_stream(File0), get_stream(File0, Stream) ->
	    File = File0
	;
	    File = Stream	% nothing better to return...
	).

cprolog_name_to_stream(File, Stream) :-
	current_stream(File),
	( get_stream(File, Stream), get_stream_info(Stream, name, File) ->
	    true
	;
	    Stream = File	% nothing better to return...
	).


see(user) :- !,
	set_stream(input, stdin).
see(File) :-
	( cprolog_name_to_stream(File, Stream) ->
	    true
	;
	    open(File, read, Stream),
	    set_stream(File, Stream)
	),
	set_stream(input, Stream).


seeing(File) :-
	get_stream(input, Stream),
	stream_to_cprolog_name(Stream, File).


seen :-
	seeing(File),
	close(File),
	set_stream(input, stdin).


tell(user) :- !,
	set_stream(output, stdout).
tell(File) :-
	( cprolog_name_to_stream(File, Stream) ->
	    true
	;
	    open(File, write, Stream),
	    set_stream(File, Stream)
	),
	set_stream(output, Stream).


telling(File) :-
	get_stream(output, Stream),
	stream_to_cprolog_name(Stream, File).


told :-
	telling(File),
	close(File),
	set_stream(output, stdout).


skip(S) :-
	skip(input, S).

skip(S, X) :-
	N is X,
	skip_to(S, N).

skip_to(S, N) :-
	(get(S, N) ->
		true
	;
		skip_to(S, N)
	).

tab(X) :-
	tab(output, X).

tab(S, X) :-
	Tab is X,
	put_spaces(S, Tab).

put_spaces(_, 0) :- !.
put_spaces(S, N) :-
	put(S, 32),
	N1 is N-1,
	put_spaces(S, N1).

