%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   dump.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	Produce an external representation of the solver state.
*/

:- pragma( expand).
:- pragma( nodebug).

%
% attribute_goal( V, V:Atts) :- get_atts( V, Atts).
%
attribute_goal( V, Goal) :-
  dump_linear( V, Goals, Gtail),
  dump_nonzero( V, Gtail, []),
  l2w( Goals, Goal).

l2w( [],     true).
l2w( [X|Xs], Conj) :-
  ( Xs = [],	Conj = {X}
  ; Xs = [_|_], Conj = ({X},Xc),
		l2w( Xs, Xc)
  ).

all_attribute_goals( All, Goals) :-
	all_attribute_goals( All, Gsw, []),
	unwrap_list( Gsw, Gswu),
	sort( Gswu, Goals).

all_attribute_goals( []) --> [].
all_attribute_goals( [X|Xs]) -->
	attribute_goals( X),
	all_attribute_goals( Xs).

attribute_goals( V) -->
	dump_linear( V),
	dump_nonzero( V).

dump_linear( V) -->
  {
    get_atts( V, [lin(Lin),type(Type)]),
    !,
    decompose( Lin, H, _, I)
  },
  %
  % This happens if not all target variables can be made independend
  % Example: examples/option.pl:
  % | ?- go2(S,W).
  %
  % W = 21/4,
  % S>=0,
  % S<50 ? ;
  %
  % W>5,
  % S=221/4-W,		  this line would be missing !!!
  % W=<21/4
  %
  ( { Type=t_none ; get_atts( V, -target) } -> [] ; dump_var( t_none, V, I, H) ),
  %
  ( {Type=t_none, get_atts( V, -target) } ->	% nonzero produces such
       []
  ;
       dump_var( Type, V, I, H)
  ).
dump_linear( _) --> [].

dump_nonzero( V) -->
  {
    get_atts( V, [nonzero,lin(Lin)]),
    !,
    decompose( Lin, H, _, I)
  },
  dump_nz( V, H, I).
dump_nonzero( _) --> [].

% -----------------------------------------------------------------------




