% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1997-1998
% Copyright (C) Parc Technologies Ltd 1999
% Version:	$Id: repairfd.pl,v 1.4 2002/05/30 17:15:28 amc4 Exp $
%
%
% IDENTIFICATION:       repairfd.pl
%
% Contents:	CLP Repair example labeling
%
% Authors:	Hani El-Sakkout, Stefano Novello, Joachim Schimpf
%		IC-Parc
% ----------------------------------------------------------------------

% Example repair labeling strategy


% ----------------------------------------------------------------------
:- module(repairfd).
% ----------------------------------------------------------------------

:- lib(fd).
:- lib(repair).


:- export repair/1.

repair(ConflictSet) :-
	( conflict_vars([C|_onflict]) ->
		indomain(C),
		repair(ConflictSet)
	; conflict_constraints(ConflictSet, [C|_onflictConstraints]) ->
		term_variables(C, Vars),
		deleteffc(Var,Vars, _),
		Var tent_get Val,
		(Var = Val ; Var #\= Val), % choice
		repair(ConflictSet)
	;
		true
	).


:- export repair/0.	% backward compatibility

repair :-
	( conflict_vars([C|_onflict]) ->
		indomain(C),
		repair
	; conflict_constraints([C|_onflictConstraints]) ->
		term_variables(C, Vars),
		deleteffc(Var,Vars, _),
		Var tent_get Val,
		(Var = Val ; Var #\= Val), % choice
		repair
	;
		true
	).

