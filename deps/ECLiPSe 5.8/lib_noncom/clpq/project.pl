%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   project.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

%
% Answer constraint projection
%

:- use_module( ordering).

project( Target) :-
	related_linear_vars( Target, LinTv, LinSys, LinAv),
	project( LinTv, LinAv),
	impose_ordering( Target, LinSys),
	!.
project( _) :-
	raise_exception( failed(project/1)).

project( [],  Cvas) :- !,			% no constraints on ground solutions ...
  drop_lin_atts( Cvas).
project( Tvs, Avs) :-
  mark_target( Tvs),
  reachable( Tvs, NlReachable), 		% problematic
  redundancy_vars( Avs),			% redundancy.pl
  make_target_indep( Tvs, Pivots),
  mark_target( NlReachable),			% after make_indep to express priority
  drop_dep( Avs),
  fm_elim( Avs, Tvs, Pivots).

mark_target( []).
mark_target( [V|Vs]) :-
  put_atts( V, target),
  mark_target( Vs).

mark_keep( []).
mark_keep( [V|Vs]) :-
  put_atts( V, keep),
  mark_keep( Vs).

%
% Collect the pivots in reverse order
% We have to protect the target variables pivot partners
% from redundancy eliminations triggered by fm_elim,
% in order to allow for reverse pivoting.
%
make_target_indep( Ts, Ps) :- make_target_indep( Ts, [], Ps).

make_target_indep( [],	   Ps, Ps).
make_target_indep( [T|Ts], Ps0,Pst) :-
  ( get_atts( T, [lin(Lin),type(Type)]),
    decompose( Lin, H, _, _),
    nontarget( H, Nt) ->
       Ps1 = [T:Nt|Ps0],
       put_atts( Nt, keep),
       pivot( T, Nt, Type)
  ;
       Ps1 = Ps0
  ),
  make_target_indep( Ts, Ps1,Pst).

nontarget( [V*_|Vs], Nt) :-
  ( get_atts( V, [-target,-keep_indep]) ->
      Nt = V
  ;
      nontarget( Vs, Nt)
  ).

drop_dep( Vs) :- var( Vs), !.
drop_dep( []).
drop_dep( [V|Vs]) :-
  drop_dep_one( V),
  drop_dep( Vs).

drop_dep_one( V) :-
  get_atts( V, [lin(Lin),type(t_none),-target,-keep,-nonzero]),
  \+ indep( Lin, V),
  !,
  put_atts( V, [-lin(_),-type(_),-class(_),-order(_),-strictness(_)]).
drop_dep_one( _).

drop_lin_atts( []).
drop_lin_atts( [V|Vs]) :-
  put_atts( V, [-lin(_),-type(_),-class(_),-order(_),-strictness(_)]),
  drop_lin_atts( Vs).

impose_ordering( Target, Sys) :-
	arrangement( Target, Arr),		% ordering.pl
	arrange_sys( Sys, Arr).

arrange_sys( [], _).
arrange_sys( [S|Ss], Arr) :-
	arrange( Arr, S),
	arrange_sys( Ss, Arr).

arrange( [],  _).
arrange( Arr, S) :- Arr = [_|_],
  class:allvars( S, All),
  order( Arr, 1, N),				% higher precedence
  order( All, N, _),				% lower precedence
  renorm_all( All),
  arrange_pivot( All).

order( Xs, N, M) :- var(Xs), !, N=M.
order( [], N, N).
order( [X|Xs], N, M) :-
  ( solver_variable( X, _, Xsolver),
    get_atts( Xsolver, order(O)),
    var(O) ->
      O=N,
      N1 is N+1,
      order( Xs, N1, M)
  ;
      order( Xs, N, M)
  ).

renorm_all( Xs) :- var( Xs), !.
renorm_all( [X|Xs]) :-
  ( get_atts( X, lin(Lin)) ->
      renormalize( Lin, New),
      put_atts( X, lin(New)),
      renorm_all( Xs)
  ;
      renorm_all( Xs)
  ).

arrange_pivot( Xs) :- var( Xs), !.
arrange_pivot( [X|Xs]) :-
  ( get_atts( X, [lin(Lin),type(t_none)]),
    decompose( Lin, [Y*_|_], _, _),
    nf_ordering( Y, X, <) ->
      pivot( X, Y, t_none),
      arrange_pivot( Xs)
  ;
      arrange_pivot( Xs)
  ).

related_linear_vars( Vs, Vsf, Sys, All) :-
	filter_vars( Vs, Vsf),
	linear_systems( Vsf, [], Sys),
	all_sys_vars( Sys, All, []).

filter_vars( [],     []).
filter_vars( [X|Tail], Res) :-
	( solver_variable( X, _, Xs) -> 	% q | r
	    Res = [Xs|Tailf],
	    filter_vars( Tail, Tailf)
	;
	    filter_vars( Tail, Res)
	).

%
% O(n^2), use sort/avl later, N is very close to 1 anyway
%
linear_systems( [],	 Si, Si).
linear_systems( [V|Vs], Si, So) :-
  ( var(V), get_atts( V, class(C)),
    not_memq( Si, C) ->
      linear_systems( Vs, [C|Si], So)
  ;
      linear_systems( Vs, Si, So)
  ).

not_memq( [],	  _).
not_memq( [Y|Ys], X) :-
  X \== Y,
  not_memq( Ys, X).

all_sys_vars( []) --> [].
all_sys_vars( [S|Ss]) -->
	{
	    class:allvars( S, All)
	},
	copy_otl( All),
	all_sys_vars( Ss).

%
% eclipse determinism detection problem ...
%
copy_otl( Xs) --> {var(Xs)}, !.
copy_otl( [X|Xs]) -->
	[ X ],
	copy_otl( Xs).
