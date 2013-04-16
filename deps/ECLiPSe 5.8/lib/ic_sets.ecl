% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers (ic wrapper)
%
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	(C) Imperial College London and Parc Technologies 2000
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: ic_sets.ecl,v 1.12 2003/01/10 16:19:06 js10 Exp $
%
% ----------------------------------------------------------------------

:- module(ic_sets).

:- lib(ic).
:- lib(ic_kernel).

tr_generic_sets(solver_module, ic).
tr_generic_sets(sbds_module, ic_sbds).

:- local macro(solver_module, tr_generic_sets/2, []).
:- local macro(sbds_module, tr_generic_sets/2, []).

:- include(generic_sets).

% ----------------------------------------------------------------------

:- comment(summary, "Solver over sets of integers (cooperates with lib(ic))").
:- comment(eg, "
% Example program: Steiner triplets
% Compute NB triplets of numbers from 1 to N such that
% any two triplets have at most one element in common.
% Try steiner(9,Sets).

:- lib(ic_sets).
:- lib(ic).

steiner(N, Sets) :-
	NB is N * (N-1) // 6,		% compute number of triplets
	intsets(Sets, NB, 1, N),	% initialise the set variables
	( foreach(S,Sets) do
	    #(S,3)			% constrain their cardinality
	),
	( fromto(Sets,[S1|Ss],Ss,[]) do
	    ( foreach(S2,Ss), param(S1) do
		#(S1 /\\ S2, C),		% constrain the cardinality
		C #=< 1			% of pairwise intersections
	    )
	),
        label_sets(Sets).		% search

label_sets([]).
label_sets([S|Ss]) :-
        insetdomain(S,_,_,_),
	label_sets(Ss).
").
