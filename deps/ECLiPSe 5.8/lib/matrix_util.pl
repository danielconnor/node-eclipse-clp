% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: matrix_util.pl,v 1.5 2002/04/16 15:46:03 js10 Exp $
% ----------------------------------------------------------------------

:- module(matrix_util).

:- comment(summary, "Predicates to build matrices from lists").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Imperial College London and ICL").
:- comment(date, "$Date: 2002/04/16 15:46:03 $").

:- comment(matrix/3, [
    summary:"Create a matrix as a list of rows",
    template:"matrix(+NRow, +NCols, -Rows)"
    ]).
:- comment(matrix/4, [
    summary:"Create a matrix as both a list of rows and a list of columns",
    template:"matrix(+NRows, +NCols, -Rows, -Cols)"
    ]).
:- comment(list2rows/4, [
    summary:"Create a matrix from a flat list of row-wise listed elements",
    template:"list2rows(+List, +NRows, +NCols, -Rows)"
    ]).
:- comment(list2cols/4, [
    summary:"Create a matrix from a flat list of row-wise listed elements",
    template:"list2rows(+List, +NRows, +NCols, -Cols)"
    ]).
:- comment(transpose/2, [
    summary:"Transpose a matrix (list of rows or list of columns)",
    template:"transpose(+Matrix, -Transposed)"
    ]).
:- comment(concat/2, [
    summary:"Concatenate all the rows (or columns) into a flat list",
    template:"concat(+RowsOrCols, -List)",
    see_also:[list2rows/4,list2cols/4,transpose/2]
    ]).
:- comment(read_data/3, [
    summary:"Read numbers from a file into List",
    desc:html("Read numbers from a file into List until either the list is full or
    the end of file has been reached. In the first case, not all numbers in the
    file may be read, in the second, the list may not be fully instantiated.
    The count of numbers that have actually been read is returned in Length.
    "),
    template:"read_data(+File, ?List, -Length)",
    see_also:[list2rows/4,list2cols/4,transpose/2]
    ]).

:- export
	matrix/3,	% matrix(+NRow, +NCol, -Rows)
	matrix/4,	% matrix(+NRow, +NCol, -Rows, -Cols)
	list2rows/4,	% list2rows(+List, +NRows, +NCols, -Rows)
	list2cols/4,	% list2cols(+List, +NRows, +NCols, -Cols)
	transpose/2,	% transpose(+Rows, -Cols) or transpose(+Cols, -Rows)
	concat/2,	% concat(+RowsOrCols, -List)
	read_data/3.	% read_data(+File, ?List, -Length)



matrix(NRow, NCol, Rows, Cols) :-
	matrix(NRow, NCol, Rows),
	transpose(Rows, Cols).

matrix(0, _NCol, []).
matrix(NRow, NCol, [L|LoL1]) :-
	integer(NRow), NRow > 0, 
	NRow1 is NRow-1,
	length(L, NCol),
	matrix(NRow1, NCol, LoL1).

list2matrix([], 0, _NCol, []) :- !.
list2matrix(List, NRow, NCol, [Row|Rows]) :-
	integer(NRow), NRow > 0, 
	NRow1 is NRow-1,
	first_n(NCol, List, Row, Rest),
	list2matrix(Rest, NRow1, NCol, Rows).

first_n(0, L, [], L) :- !.
first_n(N, [X|Xs], [X|Fs], Rest) :-
	N1 is N-1,
	first_n(N1, Xs, Fs, Rest).

list2rows([], 0, _, []) :- !.
list2rows(List, NRow, NCol, [Row|Rows]) :-
	plus(NRow1,1,NRow),
	first_n(NCol, List, Row, Rest),
	list2rows(Rest, NRow1, NCol, Rows).

list2cols(List, NRow, NCol, Cols) :-
	list2rows(List, NRow, NCol, Rows),
	transpose(Rows, Cols).

transpose([], []).
transpose(LoL, Cols) :-
	heads_and_tails(LoL, Col, LoL1),
	( Col == [] ->
	    Cols = []
	;
	    Cols = [Col|Cols0],
	    transpose(LoL1, Cols0)
	).

heads_and_tails([], [], []).
heads_and_tails([L|Ls], Hs, Ts) :-
	( L == [] ->
	    Hs = Hs0, Ts = Ts0
	;
	    L = [H|T], Hs = [H|Hs0], Ts = [T|Ts0]
	),
	heads_and_tails(Ls, Hs0, Ts0).

concat([], []).
concat([L|Ls], C) :-
	concat(L, C, C0),
	concat(Ls, C0).

concat([], L, L).
concat([X|Xs], [X|Ys], L) :-
	concat(Xs, Ys, L).

% read numbers until list is full or end of file.

read_data(File, L, N) :-
	open(File, read, Stream),
	read_floats(Stream, L, 0, N),
	close(Stream).

read_floats(Stream, [X|Xs], N0, N) :-
	read_token(Stream, Token, _),
	( number(Token) ->
	    !,	% if called with a variable
	    X is float(Token),
	    N1 is N0+1,
	    read_floats(Stream, Xs, N1, N)
	;
	    Token \= end_of_file,
	    get_stream_info(Stream, name, Name),
	    get_stream_info(Stream, line, Line),
	    printf(error, "Syntax error in file \"%w\", line %d: %w\n",
	    		[Name, Line, Token]),
	    close(Stream),
	    abort
	).
read_floats(_Stream, [], N, N).


