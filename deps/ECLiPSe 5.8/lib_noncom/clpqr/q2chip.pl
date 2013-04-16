%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   q2chip.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	Express clp(q) interface
	in terms of chip rational solver and lib(r)
*/


:- op(700,xfx,$=).
:- op(700,xfx,$<>).
:- op(700,xfx,$>=).
:- op(700,xfx,$<=).
:- op(700,xfx,$=<).
:- op(700,xfx,$>).
:- op(700,xfx,$<).

minimize( Exp) :- rmin( Exp).
maximize( Exp) :- rmax( Exp).

{   R,Rs  } :- {R}, {Rs}.
{  L < R  } :- L $< R.
{  L > R  } :- L $> R.
{  L =< R } :- L $=< R.
{ <=(L,R) } :- L $=< R.
{  L >= R } :- L $>= R.
{ L =\= R } :- L $<> R.
{ L =:= R } :- L $= R.
{ L  =	R } :- L $= R.
