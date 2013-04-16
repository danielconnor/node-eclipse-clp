%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   monash.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).				% DCG expansion

%
% Monash compatibility
%

:- op( 700, xfx, <=).
:- op( 150, fx, #).

:- use_module(library(scattered)).
:- use_module( expand).

undef_pred_handler( _, _) :- fail.

:- set_error_handler(68,undef_pred_handler/2).

:- import printf_body/3 from sepia_kernel.

printf_type_handler( _, printf(Fmt,[Culprit|List]), Module) :-
	string_list( Fmt, FmtL),
	phrase( pfmt(Culprit,Coerced), FmtL, _),
	!,
	printf_body( Fmt, [Coerced|List], Module).
printf_type_handler( N, G, M) :-
	error( default(N), G, M).

%
% This grammar is very approximative. We assume that the format string
% was correct in the first place.
%
pfmt( Cu, Co) --> [0'%], pfmt_pref, pfmt_type( Cu, Co).

pfmt_pref --> [0'-], pfmt_pref.
pfmt_pref --> [0'.], pfmt_pref.
pfmt_pref --> [D], {D>=0'0,D=<0'9}, pfmt_pref.
pfmt_pref --> [].

pfmt_type( Cu, Co) --> [0'd], { Co is fix(round(float(Cu))) }.
pfmt_type( Cu, Co) --> [0'e], { Co is float(Cu) }.
pfmt_type( Cu, Co) --> [0'f], { Co is float(Cu) }.
pfmt_type( Cu, Co) --> [0'g], { Co is float(Cu) }.

:- set_error_handler(5,printf_type_handler/3).

dump.						% cheating
dump( L) :- ordering( L).
