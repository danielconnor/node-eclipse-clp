%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   bb.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma( expand).
:- pragma( nodebug).

bb_inf( Is, Term, Inf) :-
  bb_inf( Is, Term, Inf, 0.001).

bb_inf( Is, Term, Infimum, Eps) :-
  nf( Eps, ENf),
  nf:nf_constant( ENf, EpsN),
  wait_linear( Term, Nf, bb_inf_internal(Is,Nf,EpsN,Inf)),
  { Infimum =:= Inf }.

bb_inf_internal( Is, Lin, Eps, _) :-
  intern_intvars( Is, IsNf),
  remove_incumbent,
  deref( Lin, Lind),
  var_with_def_assign( Dep, Lind),
  determine_active_dec( Lind),
  bb_loop( Dep, IsNf, Eps).			% always fails
bb_inf_internal( _, _, _, Inf) :-
  incumbent( Inf).

bb_loop( Opt, Is, Eps) :-
  ( var(Opt),					% added ineqs may lead to binding
      iterate_dec( Opt, Inf)
  ; nonvar(Opt),
      Inf = Opt
  ),
  bound( Inf),
  violated_ints( Is, Eps, Viol),
  ( Viol = [] ->	update_incumbent( Inf)
  ; Viol = [First|_] -> First=V:Floor:_:Ceiling,
			branch( V, Floor, Ceiling),
			bb_loop( Opt, Is, Eps)
  ),
  fail.

bound( Inf) :-
  incumbent( Inc),
  !,
  arith_eval( Inf < Inc).
bound( _).

branch( V, U, _) :- { V =< U }.
branch( V, _, L) :- { V >= L }.

%
% Need only one as we branch on the first anyway ...
%
violated_ints( [],     _,   []).
violated_ints( [X|Xs], Eps, Vio) :-
  rhs_value( X, Rhs),
     arith_eval( floor(Rhs), Floor),
     arith_eval( ceiling(Rhs), Ceiling),
     ( arith_eval(min(Rhs-Floor,Ceiling-Rhs) < Eps) -> % adjustable
	 violated_ints( Xs, Eps, Vio)
     ;
	 Vio = [X:Floor:Rhs:Ceiling]		% one is enuf !
	 % violated_ints( Xs, Eps, Vs)
     ).

rhs_value( Xn, Value) :- nonvar(Xn), Value=Xn.
rhs_value( Xn, Value) :- var(Xn),
  deref_var( Xn, Xd),
  decompose( Xd, _, R, I),
  arith_eval( R+I, Value).

%
% allow more general expressions and conditions? integral(Exp) ???
%
intern_intvars( [],	[]).
intern_intvars( [X|Xs], Res) :-
  nf( X, Xnf),
  ( Xnf = [] -> 				% 0
     Res = Tail
  ; Xnf = [v(I,[])] ->
     arith:integerp( I),			% integer ?
     Res = Tail
  ; Xnf = [v(One,[V^1])],
    arith_eval(One=:=1) ->
     Res = [V|Tail]
  ;
     raise_exception( bb_inf(X))
  ),
  intern_intvars( Xs, Tail).


:- dynamic incumbent/1.

remove_incumbent :-
  retract( incumbent(_)),
  fail.
remove_incumbent.

update_incumbent( _) :-
  retract( incumbent(_)),
  fail.
update_incumbent( A) :-
  assert( incumbent(A)).

% ----------------------------- playground --------------------------

bb_inf_trace( Is, Term, Eps, Infimum) :-
  nf( Eps, ENf),
  nf:nf_constant( ENf, EpsN),
  wait_linear( Term, Nf, bb_inf_trace_lin(Is,Nf,EpsN,Inf)),
  { Infimum =:= Inf }.

bb_inf_trace_lin( Is, Lin, Eps, _) :-
  intern_intvars( Is, IsNf),
  remove_incumbent,
  deref( Lin, Lind),
  var_with_def_assign( Dep, Lind),
  determine_active_dec( Lind),
  bb_loop_trace( Dep, IsNf, Eps).		% always fails
bb_inf_trace_lin( _, _, _, Inf) :-
  incumbent( Inf).

bb_loop_trace( Opt, Is, Eps) :-
  ( var(Opt),
      iterate_dec( Opt, Inf)
  ; nonvar(Opt),
      Inf = Opt
  ),
  bound( Inf),
  violated_ints( Is, Eps, Viol),
  ( Viol = [] ->	update_incumbent( Inf),
			at_value( Is, Vs),
			print(incumbent(Inf,Vs)), nl
  ; Viol = [First|_] ->
			First=V:Floor:_:Ceiling,
			branch( V, Floor, Ceiling),
			bb_loop_trace( Opt, Is, Eps)
  ),
  fail.


bounds( [],	[]).
bounds( [X|Xs], [X:[L,U] | Tail]) :-
  integer_range( X, L, U),
  bounds( Xs, Tail).

integer_range( X, L, U) :-
  inf(X,Lf),
  sup(X,Uf),
  arith_eval( round(Lf), Li),	% precision problem ...
  arith_eval( round(Uf), Ui),
  ( Li =< Ui ->
     L = Li, U = Ui
  ;
     print( it_happend), nl,
     L = Ui, U = Li
  ).

at_value( [],	  []).
at_value( [X|Xs], [V|Vs]) :-
  rhs_value( X, V),
  at_value( Xs, Vs).


