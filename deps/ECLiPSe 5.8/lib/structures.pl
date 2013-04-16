% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: structures.pl,v 1.9 2002/11/08 18:45:50 js10 Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% sccsid("@(#)structures.pl	30.8             95/03/30").
% sccscr("@(#)  Copyright 1990 ECRC GmbH ").
%
% IDENTIFICATION:	structures.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:
%	predicates:	define_struct(+Template)
%			erase_struct(+StructName)
%			portray_struct(+Stream, +Struct)
%
%	macros:		?Struct with ?Fields
%			+FieldName of +StructName
%
% DESCRIPTION:
%
%	The useful core of this library has been moved to the kernel!
%	This file is just for backward compatibility.
%

:- module(structures).
:- export
	define_struct/1,
	erase_struct/1,
	eq_struct/2,
	portray_struct/2.

:- export op(650, xfx, [eq_struct]).

:- pragma(nodebug).


:- tool(define_struct/1, define_struct/2).
define_struct(Struct, Module) :-
	printf(warning_output,
	    "WARNING: define_struct/1 is obsolete, use export struct or local struct",
	    []),
	export(struct(Struct))@Module.


erase_struct(_Struct) :-
	printf(warning_output, "WARNING: erase_struct/1 is obsolete, ignored", []).

eq_struct(_, _) :-
	printf(warning_output, "WARNING: eq_struct/2 is obsolete", []),
	abort.


:- tool(portray_struct/2, portray_struct/3).
portray_struct(Stream, Struct, Module) :-
	functor(Struct, Functor, Arity),
	functor(Def, Functor, Arity),
	current_struct(Def)@Module,
	make_list(Def, Struct, Arity, [], List),
	print(Stream, no_macro_expansion(Functor with List)).

make_list(_Struct, _Template, 0, List, List) :- !.
make_list(Struct, Template, N, List0, List) :-
	arg(N, Struct, FieldName),
	arg(N, Template, FieldValue),
	N1 is N-1,
	make_list(Struct, Template, N1, [FieldName:FieldValue|List0], List).

