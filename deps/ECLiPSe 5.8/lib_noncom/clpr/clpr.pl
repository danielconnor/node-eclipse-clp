%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                            version 1.4 (Eclipse port) %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   clpr.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_event_handler(76, true/0).
:- set_event_handler(77, true/0).

:- pragma( expand).
:- pragma( nodebug).

:- module_interface( clpr).

:- op(700,xfx,$=).
:- op(700,xfx,$<>).
:- op(700,xfx,$>=).
:- op(700,xfx,$<=).
:- op(700,xfx,$=<).
:- op(700,xfx,$>).
:- op(700,xfx,$<).

:- import
	 maximize/1,
	 minimize/1,
	 sup/2, inf/2,
	 bb_inf/3,
	 bb_inf_trace/4
 from clpr_intern.

:- use_module( macros). 			% todo: same for operators

:- import
	{}/1,
	 entailed/1,
	 $=  /2,
	 $<> /2,
	 $>= /2,
	 $<= /2,
	 $=< /2,
	 $>  /2,
	 $<  /2
 from nf.

:- import ordering/1 from ordering.

:- begin_module(clpr).

:- module_interface( clpr_intern).

:- begin_module( clpr_intern).

:- export
	 maximize/1,
	 minimize/1,
	 sup/2, inf/2,
	 bb_inf/3,
	 bb_inf_trace/4.

:- export					% those are for nf.pl only
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
	 export_binding/2.

:- import {}/1 from nf.
:- local basis/2, allvars/2.

:- use_module( [
	compat,
	macros,
	arith,
	nf,
	class
	       ]).

:- define_macro( nf_ordering/3, trans_ordering/2, [goal]).

trans_ordering( nf_ordering(A,B,Rel), Exp) :-	% xref store.pl
	Exp = (
		  get_atts( A, order( Oa)),
		  get_atts( B, order( Ob)),
		  true, % bug workaround
		  compare( Rel, Oa, Ob)
	      ).

:- define_macro( decompose/4, trans_decompose/2, [goal]).

trans_decompose( decompose(Lin,H,R,I), Lin=[I,R|H]).

:- ensure_loaded( [
	% pre,					% select one of pre/post
	post,
	dump, print,
	store,
	project,
	bv,
	ineq,
	redund,
	fourmotz,
	bb
		  ]).

export_binding( X, Y) :- nonvar(X), nonvar(Y), !.
export_binding( X, Y) :- nonvar(Y), !,
	( arith_eval( Y=:=0) ->
	    X = 0.0
	;
	    X = Y
	).
export_binding( X, X).
