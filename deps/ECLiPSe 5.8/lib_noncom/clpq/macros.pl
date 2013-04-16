%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   macros.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	Macros used by more than one module
*/


:- module_interface( macros).

:- export
	trans_rat/2.

:- define_macro( type(rational), trans_rat/2, [write]).

:- begin_module( macros).

trans_rat( Rat, Exp) :-
	N is numerator(Rat),
	D is denominator(Rat),
	( D=1 ->
	    Exp = N
	;
	    Exp = N/D
	).
