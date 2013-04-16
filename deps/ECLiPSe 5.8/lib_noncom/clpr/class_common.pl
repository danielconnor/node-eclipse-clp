%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   class_common.pl                                        %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

new( Class, All,AllT, Basis) :-
  put_atts( Su, class_atts(All,AllT,Basis)),
  Su = Class.

drop( Class, X) :-
  get_atts( Class, class_atts(Allvars,Tail,Basis)),
  delete_first( Allvars, X, NewAllvars),
  delete_first( Basis, X, NewBasis),
  put_atts( Class, class_atts(NewAllvars,Tail,NewBasis)).

allvars( Class, All) :- get_atts( Class, class_atts(All,_,_)).

basis( Class, Basis) :- get_atts( Class, class_atts(_,_,Basis)).

basis_add( Class, X, NewBasis) :-
  NewBasis = [X|Basis],
  get_atts( Class, class_atts(All,AllT,Basis)),
  put_atts( Class, class_atts(All,AllT,NewBasis)).

basis_drop( Class, X) :-
  get_atts( Class, class_atts(All,AllT,Basis0)),
  delete_first( Basis0, X, Basis),
  Basis0 \== Basis,			% anything deleted ?
  !,
  put_atts( Class, class_atts(All,AllT,Basis)).
basis_drop( _, _).

basis_pivot( Class, Enter, Leave) :-
  get_atts( Class, class_atts(All,AllT,Basis0)),
  delete_first( Basis0, Leave, Basis1),
  put_atts( Class, class_atts(All,AllT,[Enter|Basis1])).

%
% remove the first occurence
%
delete_first( L,      _, Res) :- var(L), !, Res = L.
delete_first( [],     _, []).
delete_first( [Y|Ys], X, Res) :-
  ( X==Y ->
      Res = Ys
  ;
      Res = [Y|Tail],
      delete_first( Ys, X, Tail)
  ).
