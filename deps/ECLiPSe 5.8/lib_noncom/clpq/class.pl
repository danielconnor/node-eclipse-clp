%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   class.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

:- module_interface( class).

:- begin_module( class).

:- ensure_loaded( library(lists)).
:- import append/3
	from lists.
:- export new/4, 
	  allvars/2, 	  
	  basis_drop/2,
	  basis/2,
	  basis_add/3,
	  basis_pivot/3,
	  drop/2.

:- use_module( library(atts)).

:- attribute class_atts/3.

:- meta_attribute( class,
	[
	    unify: unify_class/2
	    % ,print: get_atts/2
	]).

unify_class( _, AttsX) :- var( AttsX).
unify_class( Y, AttsX) :- nonvar( AttsX),
	var(Y),
	map_atts( AttsX, class_atts(La,Lat,ABasis)),
	get_atts( Y, class_atts(Lb,Lbt,BBasis)),
	Lat = Lb,				% append
	append( ABasis, BBasis, CBasis),
	put_atts( Y, class_atts(La,Lbt,CBasis)).

:- ensure_loaded( class_common).
