%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   nf.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pragma( expand).
:- pragma( nodebug).

:- module_interface( nf).

:- op(700,xfx,$=).
:- op(700,xfx,$<>).
:- op(700,xfx,$>=).
:- op(700,xfx,$<=).
:- op(700,xfx,$=<).
:- op(700,xfx,$>).
:- op(700,xfx,$<).

:- export
	{}/1,
	 entailed/1,
	 $=  /2,
	 $<> /2,
	 $>= /2,
	 $<= /2,
	 $=< /2,
	 $>  /2,
	 $<  /2,

	nf/2,
	nf_split/3,
	wait_linear/3,
	nf_constant/2,
	write_macro/2.

:- define_macro( resubmit_eq/1, write_macro/2, [write,goal]).
:- define_macro( resubmit_lt/1, write_macro/2, [write,goal]).
:- define_macro( resubmit_le/1, write_macro/2, [write,goal]).
:- define_macro( resubmit_ne/1, write_macro/2, [write,goal]).

:- begin_module( nf).

:- use_module( macros). 			% apply in the above writes
:- use_module( compat).
:- use_module( arith).

:- import
	 solve/1,
	 solve_eq/1,
	 solve_lt/1,
	 solve_le/1,
	 solve_ne/1,
	 log_deref/4,
	 normalize_scalar/2,
	 add_linear_11/3,
	 ineq_one_s_p_0/1,
	 ineq_one_s_n_0/1,
	 ineq_one_n_p_0/1,
	 ineq_one_n_n_0/1,
	 ineq_one/4,
	 export_binding/2
 from clpr_intern.

write_macro( resubmit_eq(Nf), {Term=0}) :- nf2term( Nf, Term).
write_macro( resubmit_lt(Nf), {Term<0}) :- nf2term( Nf, Term).
write_macro( resubmit_le(Nf), {Term=<0}) :- nf2term( Nf, Term).
write_macro( resubmit_ne(Nf), {Term=\=0}) :- nf2term( Nf, Term).

%
% Goal delays until Term gets linear.
% At this time, Var will be bound to the normalform of Term.
%
%
:- tool( wait_linear/3, wait_linear_body/4).

wait_linear_body( Term, Var, Goal, Module) :-
  nf( Term, Nf),
  ( linear( Nf) ->
      Var = Nf,
      call( Goal, Module)
  ;
      term_variables( Nf, Vars),
      geler( Vars, wait_linear_retry(Nf,Var,Goal,Module))
  ).

wait_linear_retry( Nf0, Var, Goal, Module) :-
  repair( Nf0, Nf),
  ( linear( Nf) ->
      Var = Nf,
      call( Goal, Module)
  ;
      term_variables( Nf, Vars),
      geler( Vars, wait_linear_retry(Nf,Var,Goal,Module))
  ).

:- ensure_loaded( nf_common).
