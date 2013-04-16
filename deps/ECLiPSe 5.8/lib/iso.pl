% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: iso.pl,v 1.30 2004/09/16 14:49:09 js10 Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: iso.pl,v 1.30 2004/09/16 14:49:09 js10 Exp $
%
% IDENTIFICATION:	iso.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directive
%
% DESCRIPTION:		ISO Prolog compatibility package (incomplete)
%			It follows standard draft ISO/IEC DIS 13211-1:1995(E)
%

:- module(iso).

:- reexport eclipse_language except

	floor/2,			% these have different behaviour
	round/2,
	get_char/1,
	get_char/2.

:- export
	op(0,   xfx, (of)),		% remove some eclipse extensions
	op(0,   xfx, (with)),
	op(0,   xfy, (do)),
	op(0,   xfx, (@)),
	op(0,   fx, (-?->)),
	macro((with)/2, (=)/2, []),
	macro((of)/2, (=)/2, []).

:- export
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(doubled_quote_is_quote),
	syntax_option(no_array_subscripts),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),
	syntax_option(limit_arg_precedence).

:- export
	chtab(0'`, string_quote),
	chtab(0'", list_quote).

:- comment(summary, `ISO Prolog compatibility library`).
:- comment(author, `Joachim Schimpf, ECRC and IC-Parc`).
:- comment(copyright, `Imperial College London and ICL`).
:- comment(date, `$Date: 2004/09/16 14:49:09 $`).
:- comment(desc, html('
    This library provides a reasonable degree of compatibility with
    the definition of Standard Prolog as defined in ISO/IEC 13211-1
    (Information Technology, Programming Languages, Prolog, Part 1: 
    General Core, 1995).  The areas where the library is not fully
    compiant are I/O and exception handling.  However it should be
    sufficient for most applications.  The library is provided in
    source form.
    <P>
    The effect of the compatibility library is local to the module where
    it is loaded. For maximal ISO-compatibility, an ISO-program should
    be contained in a  separate module starting with a directive like
    <PRE>
    :- module(myisomodule, [], iso).
    </PRE>
    In this case, Eclipse-specific language constructs will not be available.
    <P>
    If the compatibility package is loaded into a standard module, e.g. like
    <PRE>
    :- module(mymixedmdule).
    :- use_module(library(iso)).
    </PRE>
    then ISO and Eclipse language features can be used together. However,
    ambiguities must be resolved explicitly and confusion may arise from
    the different meaning of quotes in Eclipse vs ISO.
    <P>
    The recommended way is therefore the former one, ie to put code written
    in different language dialects into different modules.')).

:- export
	op(200, fy, (-)),
	op(200, fy, (\)),
	op(200, xfx, (**)),
	op(400, yfx, [(mod),(rem)]).

:- export
	(**)/3,
	assertz/1,
	at_end_of_stream/0,
	at_end_of_stream/1,
	atom_concat/3,
	atom_codes/2,
	atom_chars/2,
	catch/3,
	catch/4,
	char_conversion/2,
	close/2,
	current_char_conversion/2,
	current_input/1,
	current_output/1,
	current_prolog_flag/2,
	float_integer_part/2,
	float_fractional_part/2,
	floor/2,
	flush_output/0,
	flush_output/1,
	get_byte/1,
	get_byte/2,
	get_char/1,
	get_char/2,
	get_code/1,
	get_code/2,
	halt/1,
	initialization/1,
	iso_recover/4,
	log/2,
	multifile/1,
	multifile/2,
	number_chars/2,
	number_codes/2,
	peek_byte/1,
	peek_byte/2,
	peek_char/1,
	peek_char/2,
	peek_code/1,
	peek_code/2,
	put_byte/1,
	put_byte/2,
	put_code/1,
	put_code/2,
	rem/3,
	round/2,
	set_input/1,
	set_output/1,
	set_prolog_flag/2,
	set_stream_position/2,
	sign/2,
	stream_property/2,
	sub_atom/5,
	throw/1,
	truncate/2,
	unify_with_occurs_check/2.

:- tool(catch/3, catch/4).
:- tool(multifile/1, multifile/2).
:- tool(initialization/1, initialization/2).

:- pragma(nodebug).
:- pragma(system).

:- import block/4 from sepia_kernel.

%-----------------------------------------------------------------------
% 7.4 Directives
%-----------------------------------------------------------------------

multifile(Preds, Module) :-
	@(dynamic(Preds), Module).

initialization(Goal, Module) :-
	@(local(initialization(Goal)), Module).

%-----------------------------------------------------------------------
% 7.8 Control constructs (ok)
%-----------------------------------------------------------------------

:- local variable(ball).

catch(Goal, Catcher, Recovery, Module) :-
	block(Goal, Tag, iso:iso_recover(Tag, Catcher, Recovery, Module), Module).


    iso_recover(iso_ball_thrown, Catcher, Recovery, Module) :- !,
	getval(ball, Ball),
	( Catcher = Ball ->
	    setval(ball, _),
	    call(Recovery, Module)
	;
	    exit_block(iso_ball_thrown)
	).
    iso_recover(Tag, Catcher, Recovery, Module) :-
	( Catcher = Tag ->
	    call(Recovery, Module)
	;
	    exit_block(Tag)
	).
	

throw(Ball) :-
	atomic(Ball),
	exit_block(Ball).
throw(Ball) :-
	nonvar(Ball),
	setval(ball, Ball),
	exit_block(iso_ball_thrown).
throw(Ball) :-
	var(Ball),
	error(4, throw(Ball)).


    throw_handler(N, exit_block(iso_ball_thrown)) :-
	getval(ball, Ball),
	setval(ball, _),
	error(N, throw(Ball)).
    throw_handler(N, Goal) :-
	error(default(N), Goal).
	
:- set_error_handler(230, throw_handler/2).


%-----------------------------------------------------------------------
% 8.2 Term Unification (ok)
%-----------------------------------------------------------------------

:- set_flag(occur_check, on).
unify_with_occurs_check(X, X).			% 8.2.2
:- set_flag(occur_check, off).


%-----------------------------------------------------------------------
% 8.9 Clause creation and destruction (ok)
%-----------------------------------------------------------------------

:- import assert_/2 from sepia_kernel.
:- tool(assertz/1, assert_/2).			% 8.9.2

% don't retract all on a subsequent dynamic/1 declaration
:- set_error_handler(64, true/0).

%-----------------------------------------------------------------------
% 8.11 Stream selection and control (complete except stream properties)
%-----------------------------------------------------------------------

current_input(Stream) :-			% 8.11.1
	get_stream(input, Stream).

current_output(Stream) :-			% 8.11.2
	get_stream(output, Stream).

set_input(Stream) :-				% 8.11.3
	set_stream(input, Stream).

set_output(Stream) :-				% 8.11.4
	set_stream(output, Stream).

close(Stream, _) :-				% 8.11.6
	close(Stream).

flush_output :- flush(output).			% 8.11.7

flush_output(Stream) :- flush(Stream).

stream_property(Stream, Property) :-
	current_stream(Stream),
	stream_property1(Stream, Property).

stream_property1(Stream, mode(M)) :-		% 8.11.8
	get_stream_info(Stream, mode, M).
stream_property1(Stream, file_name(F)) :-
	get_stream_info(Stream, name, F).
stream_property1(Stream, position(P)) :-
	at(Stream, P).
stream_property1(_Stream, type(binary)).
stream_property1(Stream, end_of_stream(P)) :-
	(at_eof(Stream) -> P = at ; P = no).

at_end_of_stream :-
	at_eof(input).

at_end_of_stream(Stream) :-
	at_eof(Stream).

set_stream_position(Stream, P) :-		% 8.11.9
	seek(Stream, P).

get_byte(Code) :- get(Code).			% 8.13 and 8.14
get_byte(Stream, Code) :- get(Stream, Code).
get_char(Char) :- get_char(input, Char).
get_char(Stream, Char) :- get(Stream, Code),
	( Code = -1 -> Char = end_of_file ; char_code(Char, Code) ).
get_code(Code) :- get(Code).
get_code(Stream, Code) :- get(Stream, Code).

put_byte(Code) :- put(Code).
put_byte(Stream, Code) :- put(Stream, Code).
put_code(Code) :- put(Code).
put_code(Stream, Code) :- put(Stream, Code).

peek_byte(Stream, Byte) :- get(Stream, Next), unget(Stream), Next=Byte.
peek_byte(Byte) :- peek_byte(input, Byte).
peek_char(Stream, Byte) :- get_char(Stream, Next), unget(Stream), Next=Byte.
peek_char(Byte) :- peek_char(input, Byte).
peek_code(Stream, Byte) :- get(Stream, Next), unget(Stream), Next=Byte.
peek_code(Byte) :- peek_code(input, Byte).


%-----------------------------------------------------------------------
% 8.14 Term input/output (incomplete)
%-----------------------------------------------------------------------

char_conversion(C1, C2) :-			% 8.14.5
	writeln(warning_output,
	    'WARNING: char_conversion/2 not implemented, ignored.').

current_char_conversion(C, C).			% 8.14.6


%-----------------------------------------------------------------------
% 8.16 Constant Processing (ok)
%-----------------------------------------------------------------------

atom_concat(A, B, C) :-				% 8.16.2
	var(C), !,
	concat_atoms(A, B, C).
atom_concat(A, B, C) :-
	nonvar(C), nonvar(A), nonvar(B), !,
	concat_atoms(A, B, C).
atom_concat(A, B, C) :-
	nonvar(C),
	atom_string(C, SC),
	append_strings(SA, SB, SC),
	atom_string(A, SA),
	atom_string(B, SB).

sub_atom(Atom, Before, Length, After, SubAtom) :-	% 8.16.3
	var(SubAtom),
	atom_string(Atom, String),
	substring(String, Before, Length, After, SubString),
	atom_string(SubAtom, SubString).
sub_atom(Atom, Before, Length, After, SubAtom) :-
	nonvar(SubAtom),
	atom_string(Atom, String),
	atom_string(SubAtom, SubString),
	substring(String, Before, Length, After, SubString).

atom_chars(Atom, Chars) :-			% 8.16.4
	var(Atom),
	concat_atom(Chars, Atom).
atom_chars(Atom, Chars) :-
	nonvar(Atom),
	atom_codes(Atom, Codes),
	chars_codes(Chars, Codes).

atom_codes(Atom, List) :-			% 8.16.5
	var(Atom),
	string_list(String, List),
	atom_string(Atom, String).
atom_codes(Atom, List) :-
	nonvar(Atom),
	atom_string(Atom, String),
	string_list(String, List).

number_chars(Number, Chars) :-			% 8.16.7
	var(Number),
	concat_atom(Chars, Atom),
	chars_are_atoms(Chars, Number, Chars),
	atom_string(Atom, String0),
	valid_numstring(String0, String),
	number_string(Number, String).
number_chars(Number, Chars) :-
	nonvar(Number),
	number_string(Number, String),
	string_list(String, Codes),
	chars_codes(Chars, Codes).

number_codes(Number, Codes) :-			% 8.16.8
	var(Number),
	string_list(String1, Codes),
	valid_numstring(String1, String),
	number_string(Number, String).
number_codes(Number, Codes) :-
	nonvar(Number),
	number_string(Number, String),
	(var(Codes) ->
	    string_list(String, Codes)
	;   string_list(String1, Codes),
	    valid_numstring(String1, String)
	).

    chars_codes([], []).
    chars_codes([Char|Chars], [Code|Codes]) :-
	char_code(Char, Code),
	chars_codes(Chars, Codes).

    chars_are_atoms([], _, _).
    chars_are_atoms([Char|Chars], N, Cs) :-
	((atom(Char), atom_length(Char,1)) ->
	    chars_are_atoms(Chars, N, Cs) 
	;   error(5, number_chars(N, Cs))
        ).

    valid_numstring(String0, String) :-
	split_string(String0, `\n\r\t `, ``, Strings0),
	valid_numstring1(Strings0, String).

    valid_numstring1([``|Ss0], String) ?- !,
    % leading white spaces is ok...
	valid_numstring1(Ss0, String).
    valid_numstring1([String0], String) ?- 
    % no trailing white spaces 
	String0 = String.

%-----------------------------------------------------------------------
% 8.17 Implementation defined hooks (incomplete)
%-----------------------------------------------------------------------

set_prolog_flag(debug, Value) :- !,
	( Value == on -> set_flag(debugging, creep)
	; Value == off -> set_flag(debugging, nodebug)
	; error(6, set_prolog_flag(debug, Value))).
set_prolog_flag(double_quotes, Value) :- !,
	( Value == atom -> set_chtab(0'", atom_quote)
	; Value == codes -> set_chtab(0'", list_quote)
	; Value == chars -> error(141, set_prolog_flag(double_quotes, Value))
	; error(6, set_prolog_flag(double_quotes, Value))).
set_prolog_flag(unknown, Value) :- !,
	( Value == error -> reset_error_handler(68)
	; Value == fail -> set_error_handler(68, fail/0)
	; Value == warning -> set_error_handler(68, warn_and_fail/3)
	; error(6, set_prolog_flag(unknown, Value))).
set_prolog_flag(Flag, Value) :-
	readonly(Flag),
	!,
	error(30, set_prolog_flag(Flag, Value)).
set_prolog_flag(Flag, Value) :-			% 8.17.1
	set_flag(Flag, Value).

    warn_and_fail(_, Goal, Module) :-
    	printf(warning_output,
	    'WARNING: calling an undefined procedure %w in module %w%n',
	    [Goal,Module]),
	fail.

    readonly(bounded).
    readonly(char_conversion).
    readonly(double_quotes).
    readonly(integer_rounding_function).
    readonly(min_integer).
    readonly(max_integer).
    readonly(max_arity).

current_prolog_flag(bounded, false).
current_prolog_flag(char_conversion, off).
current_prolog_flag(debug, Value) :-
	get_flag(debugging, D),
	( D = creep -> Value = on
	; D = leap -> Value = on
	; Value = off ).
current_prolog_flag(double_quotes, Value) :-
	( get_chtab(0'", atom_quote) -> Value = atom
	; get_chtab(0'", list_quote) -> Value = codes
	; Value = unknown ).
current_prolog_flag(integer_rounding_function, toward_zero).
%current_prolog_flag(min_integer, _) :- fail.
%current_prolog_flag(max_integer, _) :- fail.
current_prolog_flag(max_arity, unbounded).
current_prolog_flag(unknown, Value) :-
	( get_error_handler(68, fail/0, _) -> Value = fail
	; get_error_handler(68, warn_and_fail/3, _) -> Value = warning
	; Value = error
	).
current_prolog_flag(Flag, Value) :- get_flag(Flag, Value).

halt(X) :- exit(X).				% 8.17.4

%-----------------------------------------------------------------------
% 9. Evaluable functors (incomplete)
% Note: the redefinitions of floor,round,mod don't work currently, the
% arithmetic transformation in the ECLiPSe compiler always uses the
% sepia_kernel definitions for the predefined arithmetic functions!
%-----------------------------------------------------------------------

**(X,Y,Z) :- Z is eval(X)^eval(Y).
sign(X,Y) :- Y is sgn(eval(X)).
truncate(X,Y) :- Y is fix(eval(X)).
log(X,Y) :- Y is ln(eval(X)).
floor(X,Y) :- X1 is X, sepia_kernel:floor(X1,Y0), Y is fix(Y0).
round(X,Y) :- X1 is X, sepia_kernel:round(X1,Y0), Y is fix(Y0).
rem(X,Y,Z) :- X1 is X, Y1 is Y, sepia_kernel:mod(X1,Y1,Z).
float_integer_part(X,Y) :- Y is float(fix(eval(X))).
float_fractional_part(X,Y) :- X1 is X, Y is X1-fix(X1).
