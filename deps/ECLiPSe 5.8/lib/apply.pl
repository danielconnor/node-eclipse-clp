% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: apply.pl,v 1.5 2004/10/20 15:04:42 js10 Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% sccsid("%W%             %E%").
% sccscr("%Z%  Copyright 1991 ECRC GmbH ").
%
% IDENTIFICATION:       apply.pl
%
% AUTHOR:               Joachim Schimpf
%
% CONTENTS:             apply/2,3
%

:- module(apply).
:- export apply/2.
:- export apply_/3.

:- export syntax_option(var_functor_is_apply).

:- comment(summary, "The apply/2 higher-order predicate").
:- comment(copyright, "Imperial College London and ICL").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(date, "$Date: 2004/10/20 15:04:42 $").
:- comment(desc, html("
    This library defines the apply/2 predicate which constructs a goal
    from a term and a list of additional arguments:
    <PRE>
    	?- P=plus(1), apply(P, [3,X]).
	P = plus(1)
	X = 4
	Yes (0.00s cpu)
    </PRE>
    Loading this library also enables the syntax option var_functor_is_apply.
    This means that it is allowed to write terms with variables functors,
    which will be parsed as apply/2 terms which can the be executed.
    The above example can thus be written as:
    <PRE>
    	?- P=plus(1), P(3,X).
	P = plus(1)
	X = 4
	Yes (0.00s cpu)
    </PRE>
")).

:- comment(apply/2, [
    summary:"The apply/2 higher-order predicate",
    args:[
	"Term":"An atom or compound term",
	"Args":"A list of arbitrary terms"
    ],
    desc:html("
    Invokes a goal that is formed by appending the elements of the
    list Args as additional arguments to Term.
    "),
    eg:"
    % The following three examples all invoke the goal plus(1,2,X):

    ?- apply(plus, [1,2,X]).
    X = 3

    ?- apply(plus(1), [2,X]).
    X = 3

    ?- apply(plus(1,2), [X]).
    X = 3


    % Error:

    ?- apply(plus(1),[2]).
    calling an undefined procedure plus(1, 2) in module eclipse
    Abort
    "
]).

:- tool(apply/2, apply_/3).

apply_(Term, AddArgs, Module) :- atom(Term), !,
	Goal =.. [Term|AddArgs],
	call(Goal)@Module.
apply_(Term, AddArgs, Module) :-
	Term =.. List,
	append(List, AddArgs, NewList),
	Goal =.. NewList,
	call(Goal)@Module.

