% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and ICL 1995-1999
% Version:	$Id: check.pl,v 1.4 2001/09/13 17:48:55 js10 Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 *	sccsid("%W%		%E%").
 *	sccscr("%Z%  Copyright 1990 ECRC GmbH ").
 *
 * IDENTIFICATION:	check.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


%
% check/0 will print:
% - predicates whose visibility has been declared but that are not defined,
% - predicates that have been declared as a tool (with tool/1)
%   but have not been linked to a tool body,
% - predicates whose type (external or b_external) has been declared but
%   that are not defined or declared (for the visibility),
% - tool interfaces whose tool body is not defined.
% - predicates that have been referenced (e.g. compiled a call to it)
%   but that is not defined and have no declaration of any type.
%
%check\_all/0 will do the same in all unlocked modules.
%

:- module(check).
:- export check/0, check/1.


:- import
	current_macro_body/5,
        get_flag_body/4,
	proc_flags/4,
        tool_body_/4
    from sepia_kernel.

:- tool(check/0, check/1).
check(Module) :-
    current_module(Module),
    current_functor(Pred),
    arg(2, Pred) < 256,
    get_flag_body(Pred, defined, _, Module),	/* speed things up */
    (
        (
            undef_declared(Pred, Def_mod, Vis, Module),
            warn_undef_declared(Pred, Def_mod, Vis, Module)
        )
        ;
        (
            undef_tool_body(Pred, Body, Mod_body, Module),
            warn_undef_tool_body(Pred, Body, Mod_body, Module)
        )
        ;
        (
            undef_typed(Pred, Type, Module),
            warn_undef_typed(Pred, Type, Module)
        )
        ;
        (
            undef_referenced(Pred, Module),
            warn_undef_referenced(Pred, Module)
        )
        ;
        (
            local_not_referenced(Pred, Module),
            warn_local_not_referenced(Pred, Module)
        )
        ;
        (
            imported_not_referenced(Pred, Module),
            warn_imported_not_referenced(Pred, Module)
        )
    ),
    fail.
check(Module) :-
    current_module(Module).

:- export check_all/0.
check_all :-
    current_module(Module),
    not(is_locked(Module)),
    printf('\n%w :\n', [Module]), flush(output),
    check(Module),
    fail.
check_all.

% the visibility Vis of Pred is declared in Mod but is not defined
% i.e. imported from Def_mod but not defined in Def_mod or
%      declared as local/export/global in Mod but not defined.
undef_declared(Pred, Def_mod, Vis, Mod) :-
    get_flag_body(Pred, declared, on, Mod),
    get_flag_body(Pred, defined, off, Mod),
    get_flag_body(Pred, visibility, Vis, Mod),
    get_flag_body(Pred, definition_module, Def_mod, Mod).

% Pred is a tool interface defined in Module but his Body is undefined.
undef_tool_body(Pred, Body, Mod_body, Module) :-
    get_flag_body(Pred, definition_module, Module, Module),
    get_flag_body(Pred, tool, on, Module),
    get_flag_body(Pred, defined, on, Module),
    tool_body_(Pred, Body, Mod_body, Module),
    get_flag_body(Body, declared, off, Module),
    get_flag_body(Body, defined, off, Module).

% Pred is declared as being of type Type (external/b_external/tool)
% in Module but there is no declared or defined predicate visible
% under that name.
undef_typed(Pred, Type, Module) :-
    (
        get_flag_body(Pred, tool, on, Module)
        ->
            Type = tool
        ;
            get_flag_body(Pred, call_type, Type, Module),
            Type \== prolog % i.e. external or b_external
    ),
    get_flag_body(Pred, defined, off, Module),
    get_flag_body(Pred, declared, off, Module),
    not(get_flag_body(Pred, visibility, global, Module)).

% pred is referenced but not declared of any type or defined.
undef_referenced(Pred, Module) :-
    get_flag_body(Pred, defined, off, Module),
    get_flag_body(Pred, declared, off, Module),
    get_flag_body(Pred, call_type, prolog, Module),
    get_flag_body(Pred, tool, off, Module),
    not(get_flag_body(Pred, visibility, global, Module)).

% local defined predicates that are not referenced
local_not_referenced(Pred, Module) :-
    get_flag_body(Pred, defined, on, Module),
    get_flag_body(Pred, visibility, local, Module),
    proc_flags(Pred, 1, Flag, Module),
    Flag /\ 16'20000000 =\= 0,
    get_flag_body(Pred, declared, off, Module),
    get_flag_body(Pred, call_type, prolog, Module),
    not(current_macro_body(_, Pred, _, _, Module)),
    not(defined_handler(Pred, Module)).

% imported predicates that are not referenced
imported_not_referenced(Pred, Module) :-
    get_flag_body(Pred, defined, on, Module),
    get_flag_body(Pred, visibility, imported, Module),
    proc_flags(Pred, 1, Flag, Module),
    Flag /\ 16'20000000 =\= 0,
    get_flag_body(Pred, declared, on, Module),
    get_flag_body(Pred, definition_module, Where, Module),
    get_module_info(Module, imports, Imports),
    not(member(Where,Imports)),
    % This is a heuristics: we might import something because
    %   an imported library imports that explicitly
    not((
	member(Use,Imports),
	get_flag_body(Pred, visibility, imported, Use))),
    not(current_macro_body(_, Pred, _, _, Module)).

defined_handler(Pred, Module) :-
    current_error(Err),
    get_error_handler(Err, Pred, Module).
defined_handler(Pred, Module) :-
    current_interrupt(Int, _),
    get_interrupt_handler(Int, Pred, Module).

warn_undef_declared(N/A, Def_mod, imported, _) :- !,
    printf('%s/%d\tis imported from %w but is not defined.\n%b',
           [N, A, Def_mod]).
warn_undef_declared(N/A, Mod, Vis, _) :-
    printf('%s/%d\tis declared as %w in %w but is not defined.\n%b',
           [N, A, Vis, Mod]).

warn_undef_tool_body(Pred_int, Pred_body, Mod_body, _) :-
    printf('The tool body %w (body of %w in %w) is not defined or declared.\n%b',
           [Pred_body, Pred_int, Mod_body]).

warn_undef_typed(N/A, tool, _) :- !,
    printf('%s/%d\tis declared as a tool interface but has no tool body.\n%b',
           [N, A]).
warn_undef_typed(N/A, Type, _) :-
    printf('%s/%d\tis declared as %w but is not declared nor defined.\n%b',
           [N, A, Type]).

warn_undef_referenced(N/A, _) :-
    printf('%s/%d\thas been referenced but is not declared or defined.\n%b',
           [N, A]).

warn_local_not_referenced(N/A, Module) :-
    printf('%s/%d\tis local in %w but is not declared or used.\n%b',
           [N, A, Module]).

warn_imported_not_referenced(N/A, Module) :-
    printf('%s/%d\tis imported in %w but is not used.\n%b',
           [N, A, Module]).
