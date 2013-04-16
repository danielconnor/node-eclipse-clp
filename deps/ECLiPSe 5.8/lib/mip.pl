% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1998
% Copyright (C) Parc Technologies Ltd 1999
% Version:	$Id: mip.pl,v 1.15 2003/12/09 20:05:02 ks15 Exp $
%
% MIP branch and bound in ECLiPSe using lib(eplex)
%
% J.Schimpf, IC-Parc, 1/96
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
:- module(mip).
% ----------------------------------------------------------------------

:- comment(summary, "An example implementing MIP-style branch-and-bound").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(date, "$Date: 2003/12/09 20:05:02 $").
:- comment(copyright, "Parc Technologies").

:- use_module(library(eplex)).
:- lib(branch_and_bound).

:- export bb_inf/3.
:- export bb_inf/4.


% ----------------------------------------------------------------------
% MIP branch-and-bound
% ----------------------------------------------------------------------

bb_inf(IntVars, Expr, MinIpCost) :-
	bb_inf(IntVars, Expr, MinIpCost, eplex).

bb_inf(IntVars, Expr, MinIpCost, Pool) :-

        Pool: (Expr $= IpCost),
	% setup a simplex demon which wakes on bound changes
	Pool:eplex_solver_setup(min(Expr), IpCost, [], 9, [deviating_bounds]),
	Pool:eplex_get(handle, ProbHandle),

	% print some statistics
	lp_get(ProbHandle, vars, VArr),
	functor(VArr, _, NVars),
	length(IntVars, NIntVars),
	writeln(log_output, variables:NVars/intvars:NIntVars),

	% call force_integers/1 within a branch-and-bound framework
	int_tolerance(Delta),
	bb_min((
		force_integers(IntVars, ProbHandle),
		lp_get(ProbHandle, typed_solution, SolutionArr),
		lp_get(ProbHandle, cost, IpCost)
	    ),
	    IpCost, SolutionArr, OptSolutionArr, MinIpCost,
	    bb_options with [strategy:continue,delta:Delta]
	),

	% print some statistics
	lp_get(ProbHandle, statistics, [SS,SF|_]),
	writeln(log_output, (solver_succs=SS, solver_fails=SF)),
	lp_cleanup(ProbHandle),

	% instantiate solutions
	( VArr = OptSolutionArr, true -> true
	; writeln(error, "Instantiating solution lead to failure") ).


% Keep branching as long as there are integer-variables whose lp-solutions
% are not integral. The lp-demon will keep waking up during the process.

force_integers(IntVars, ProbHandle) :-
	( integer_violation(IntVars, BranchingVar, SplitValue, ProbHandle) ->
	    branch(BranchingVar, SplitValue),
	    force_integers(IntVars, ProbHandle)
	;
	    true    
	).


    branch(BranchingVar, SplitValue) :-
	S is round(SplitValue),
	( S > SplitValue ->
	    (
		% try the upper sub-range first
		set_var_bounds(BranchingVar, S, 1.0Inf)
	    ;
		S1 is S-1.0,
		set_var_bounds(BranchingVar, -1.0Inf, S1)
	    )
	;
	    (
		% try the lower sub-range first
		set_var_bounds(BranchingVar, -1.0Inf, S)
	    ;
		S1 is S+1.0,
		set_var_bounds(BranchingVar, S1, 1.0Inf)
	    )
	).


% ----------------------------------------------------------------------
% Variable selection
% ----------------------------------------------------------------------

integer_violation([X|Xs], FractVar, FractSol, ProbHandle) :-
	lp_var_solution(ProbHandle, X, Val),
	( abs(Val - round(Val)) >= int_tolerance ->
	    FractVar = X, FractSol = Val
	;
	    integer_violation(Xs, FractVar, FractSol, ProbHandle)
	).


/*
integer_violation(Xs, FractVar, FractSol) :-
	integer_violation(Xs, _, 1.0, FractVar),
%	integer_violation(Xs, _, 0.0, FractVar),
	lp_var_solution(FractVar, FractSol),
%	call(get_var_index(FractVar, Idx),eplex),
%	Idx1 is Idx+1,
%	writeln(x(Idx1):FractSol),
	true.

integer_violation([], BestX, BestDiff, BestX) :-
	BestDiff >= int_tolerance,	% Did we actually find one?
	BestDiff < 1.0.
integer_violation([X|Xs], BestX, BestDiff, Res) :-
	lp_var_solution(X, Val),
	Diff is abs(Val - round(Val)),
%	( var(X), Val \== 0.0 ->
%	    call(get_var_index(X, Idx),eplex),
%	    Idx1 is Idx+1,
%	    write('    '),
%	    writeln(x(Idx1)=Val)
%	;
%	    true
%	),
	(
	    Diff >= int_tolerance,
%	    better(BestX, BestDiff, X, Diff)
%	    Diff < BestDiff
	    Diff =< BestDiff	% use 1.0 initially
%	    Diff > BestDiff	% use 0.0 initially
	->
	    integer_violation(Xs, X, Diff, Res)
	;
	    integer_violation(Xs, BestX, BestDiff, Res)
	).
*/

% prefer general integers to binaries
better(OldX, _OldDiff, _NewX, _NewDiff) :-
	free(OldX), !.
better(OldX, OldDiff, NewX, NewDiff) :-
	( get_var_bounds(OldX, _, 1.0) ->
	    ( get_var_bounds(NewX, _, 1.0) ->
		NewDiff =< OldDiff
	    ;
		true
	    )
	;
	    ( get_var_bounds(NewX, _, 1.0) ->
		fail
	    ;
		NewDiff =< OldDiff
	    )
	).


find_index([X|Xs], N, FractVar) :-
	( call(get_var_index(X, N),eplex) ->
	    FractVar=X
	;
	    find_index(Xs, N, FractVar)
	).


% ----------------------------------------------------------------------
% end_module(mip).
% ----------------------------------------------------------------------

