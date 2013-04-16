% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and Parc Technologies Ltd 2002
% Version:	$Id: lint.ecl,v 1.4 2003/09/02 19:23:08 ks15 Exp $
%
%
% TODO/ideas:
%
% - calling tools inside tool bodies without using @/2
%
% - bad builtin call patterns
%	? =.. [+|ListWithFixedLength]
%	call(+)
%	var(+)
%
% - deprecated builtins
%	assert,retract
%
% - obsolete predicates
%	name/2
%
% ----------------------------------------------------------------------


:- module(lint).

:- lib(module_options).
:- lib(source_processor).

:- comment(summary, "Heuristic program checker").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Imperial College London and Parc Technologies").
:- comment(date, "$Date: 2003/09/02 19:23:08 $").
:- comment(desc, html("
    This library analyses an ECLiPSe source module or file and generates
    warning messages for dubious programming constructs and violation
    of naming conventions.
    ")).


%----------------------------------------------------------------------
% Option handling
%----------------------------------------------------------------------

:- local struct(options(
    	singletons,
    	head_unify,
	missing_else,
    	naming_conventions
	)).


% Skeleton option structure with defaults for the user-settable fields
default_options(options with [
    	singletons:on,
    	head_unify:on,
    	missing_else:on,
    	naming_conventions:on
    ]).

% User-settable option names and their structure index
valid_option_field(singletons,		singletons of options).
valid_option_field(head_unify,		head_unify of options).
valid_option_field(missing_else,	missing_else of options).
valid_option_field(naming_conventions,	naming_conventions of options).

% Type checks for user-settable options
valid_option_value(_, off) :- !.
valid_option_value(_, on) :- !.


%----------------------------------------------------------------------
% Main predicate
%----------------------------------------------------------------------

:- export lint/1.
:- comment(lint/1, [
    summary:"Check the given source module file",
    args:[
	"File":"Name of the source file (atom or string)"
    ],
    amode:lint(+),
    desc:html("
    Analyse a source file, do various heuristic checks on the code,
    and print warnings if appropriate. This is equivalent to
    <PRE>
	lint(File, [])
    </PRE>
    "),
    see_also:[lint/2]]).

lint(File) :-
	lint(File, []).


:- export lint/2.
:- comment(lint/2, [
    summary:"Check the given source module file",
    args:[
	"File":"Name of the source file (atom or string)",
	"Options":"A list of Option:Value structures"
    ],
    amode:lint(+,+),
    desc:html("
    Analyse a source file, do various heuristic checks on the code,
    and print warnings if appropriate.
<P>
    The possible options are:
<DL>
    <DT>head_unify ('off' or 'on')</DT>
	<DD>warn if a clause with a cut contains aliasing in the head,
	which may indicate non-steadfast code. Default: on.</DD>
    <DT>missing_else ('off' or 'on')</DT>
	<DD>warn if a conditions does not have an else case, since this
	will fail and is more clearly written using a simple conjunction
	or once/1. Default: on.</DD>
    <DT>naming_conventions ('off' or 'on')</DT>
	<DD>enable/disable naming conventions checks for variable names,
	predicate names and module names. Default: on.</DD>
    <DT>singletons ('off' or 'on')</DT>
	<DD>enable/disable singleton variable checks. Default: on.</DD>
</DL>
    "),
    see_also:[lint/1]]).


lint(File, OptionList) :-
	( get_options(OptionList, Options) ->
	    source_open(File, [no_macro_expansion], SP0),
	    (
		fromto(begin, _, Class, end),
		fromto(SP0, SP1, SP2, SPend),
		param(Options)
	    do
		source_read(SP1, SP2, Class, SourceTerm),
		( lint_clause(Class, SP1, SourceTerm, Options) ->
		    true
		;
		    arg(term of source_term, SourceTerm, Term),
		    message("Couldn't analyse this: %w", [Term], SP1)
		)
	    ),
	    source_close(SPend, [])
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error),
	    abort
	).


lint_clause(end, _SP, _, _).
lint_clause(var, SP, _, _) :-
	message("Variable clause", [], SP).
lint_clause(handled_directive, SP, source_term with [term:Term,vars:_Vars], Options) :-
	lint_directive(Term, SP, Options).
lint_clause(directive, SP, source_term with [term:Term,vars:_Vars], Options) :-
	lint_directive(Term, SP, Options).
lint_clause(query, _SP, _, _).
lint_clause(comment, _SP, _, _).
lint_clause(clause, SP, source_term with [term:Term,vars:Vars], Options) :-
	( Options = options with naming_conventions:on ->
	    check_varnames(Vars, SP)
	;
	    true
	),
	( Options = options with singletons:on ->
	    check_singletons(Term, SP)
	;
	    true
	),
	lint_clause(Term, SP, Options).


lint_directive(:- module(M), SP, options with naming_conventions:on) ?- !,
	check_module_name(M, SP).
lint_directive(:- module(M,_), SP, options with naming_conventions:on) ?- !,
	check_module_name(M, SP).
lint_directive(:- module(M,_,_), SP, options with naming_conventions:on) ?- !,
	check_module_name(M, SP).
lint_directive(_, _, _).


lint_clause((Head :- Goal), SP, Options) ?- !,
	lint_head(Head, SP, Options),
	check_var_goal(Goal, SP),
	lint_goal(Goal, SP, Cut, Options),
	( Cut == yes, Options = options with head_unify:on, repeated_vars(Head, Vars) ->
	    message("Aliasing in head of clause with cut: %w", [Vars], SP)
	;
	    true
	).
lint_clause((Head ?- Goal), SP, Options) ?- !,
	lint_head(Head, SP, Options),
	lint_goal(Goal, SP, _Cut, Options).
lint_clause(Head, SP, Options) :-
	lint_head(Head, SP, Options).


lint_head(H, SP, Options) :-
	( atom(H) ; compound(H) ), !,
	functor(H, Name, _),
	( Options = options with naming_conventions:on ->
	    check_predicate_name(Name, SP)
	;
	    true
	).
lint_head(H, SP, _Options) :-
	message("Not a valid clause head: %w", [H], SP).


lint_goal(G, _, _, _) :- var(G), !.
lint_goal((G1,G2), SP, Cut, Options) ?- !,
	check_var_goal(G1, SP),
	check_var_goal(G2, SP),
	lint_goal(G1, SP, Cut, Options),
	lint_goal(G2, SP, Cut, Options).
lint_goal((G1->G2;G3), SP, Cut, Options) ?- !,
	check_var_goal(G1, SP),
	check_var_goal(G2, SP),
	check_var_goal(G3, SP),
	lint_goal(G1, SP, Cut, Options),
	lint_goal(G2, SP, Cut, Options),
	lint_goal(G3, SP, Cut, Options).
lint_goal((G1->G2), SP, Cut, Options) ?- !,
	check_var_goal(G1, SP),
	check_var_goal(G2, SP),
	( Options = options with missing_else:on ->
	    message("Conditional -> without ; should be avoided (use once/1)",[],SP)
	;
	    true
	),
	lint_goal(G1, SP, Cut, Options),
	lint_goal(G2, SP, Cut, Options).
lint_goal((G1;G2), SP, Cut, Options) ?- !,
	check_var_goal(G1, SP),
	check_var_goal(G2, SP),
	lint_goal(G1, SP, Cut, Options),
	lint_goal(G2, SP, Cut, Options).
lint_goal(!, _SP, yes, _Options).
lint_goal(G, SP, Cut, Options) :-
	( atom(G) ; compound(G) ), !,
	functor(G, Name, Arity),
	% Probably not too helpful to check callee names...
	% ( Options = options with naming_conventions:on ->
	%     check_predicate_name(Name, SP)
	% ;
	%     true
	% ),
	check_builtin_calls(G, SP, Options),
	functor(Pattern, Name, Arity),
	( meta_predicate_pattern(Pattern) ->
	    % some of G's arguments may need to be processed themselves
	    (
	        for(I,1,Arity),
		param(G,Pattern,SP,Cut,Options)
	    do
		arg(I, G, Arg),
	        arg(I, Pattern, ArgSpec),
		( direct_call(ArgSpec) ->
		    lint_goal(Arg, SP, Cut, Options)
		;
		    true
		)
	    )
	;
	    true
	).
lint_goal(G, SP, _, _Options) :-
	message("Not a callable Goal: %w", [G], SP).


check_var_goal(Goal, SP) :-
	( nonvar(Goal) -> true ;
	    message("Variable goal (%w): consider using @/2 or call/1", [Goal], SP)
	).


:- mode direct_call(+).
direct_call(:).
direct_call(u).
direct_call(e).
direct_call(s).

message(String, Args, source_position with [file:F,line:L,module:_M]) :-
	printf(warning_output, "%n--- File %s, line %d:%n", [F,L]),
	printf(warning_output, String, Args),
	nl(warning_output).


%----------------------------------------------------------------------
% Check for some silly builtin call patterns
%----------------------------------------------------------------------

:- mode check_builtin_calls(+,+,+).

check_builtin_calls(call(G), SP, _Options) :-
	nonvar(G), !,
	message("Unnecessary call/1:%n    %w", [call(G)], SP).
check_builtin_calls(X=..L, SP, _Options) :-
	proper_list(L),
	L = [F|_],
	nonvar(F),
	!,
	S =.. L,
	message("Unnecessary use of =../2:%n    %w%nUse instead:%n    %w",
		[X=..L, X=S], SP).
check_builtin_calls(X is N, SP, _Options) :-
	number(N),
	!,
	message("Unnecessary use of is/2:%n    %w%nUse instead:%n    %w",
		[X is N, X = N], SP).
check_builtin_calls(_, _, _).


    proper_list([]) ?- true.
    proper_list([_|T]) ?- proper_list(T).


%----------------------------------------------------------------------
% Singleton check
%----------------------------------------------------------------------

check_singletons(Term, SP) :-
	collect_variables(Term, [], Vars),
	sort(0, =<, Vars, SortedVars),
	(
	    SortedVars = [_X|Xs],
	    classify_vars(_X, Xs, Singletons, _),
	    Singletons = [_|_]
	->
	    message("Singleton variables: %w", [Singletons], SP)
	;
	    true
	).

repeated_vars(Term, Multis) :-
	collect_variables(Term, [], Vars),
	sort(0, =<, Vars, SortedVars),
	SortedVars = [_X|Xs],	% may fail
	classify_vars(_X, Xs, _Singletons, Multis),
	Multis = [_|_].

:- mode collect_variables(?,?,-).
collect_variables(_X, Xs, [_X|Xs]) :-
	var(_X), !.
collect_variables(T, Xs, Xs) :-
	atomic(T), !.
collect_variables([T|Ts], Xs0, Xs) :- !,
	collect_variables(T, Xs0, Xs1),
	collect_variables(Ts, Xs1, Xs).
collect_variables(T, Xs0, Xs) :-
	T =.. [_|L],
	collect_variables(L, Xs0, Xs).

classify_vars(_X, [], Singles, []):-
	(
	    get_var_info(_X, name, Name),
	    atom_string(Name, S),
	    not substring(S, "_", 1)
	->
	    Singles = [Name]
	;
	    Singles = []
	).
classify_vars(_X, [_Y|Ys], Singles, Multis) :-
	( _X == _Y ->
	    (
		get_var_info(_X, name, Name)
	    ->
		Multis = [Name|Multis0]
	    ;
		Multis = Multis0
	    ),
	    skip_same_vars(_Y, Ys, Singles, Multis0)
	;
	    (
		get_var_info(_X, name, Name),
		atom_string(Name, S),
		not substring(S, "_", 1)
	    ->
		Singles = [Name|Singles0]
	    ;
		Singles = Singles0
	    ),
	    classify_vars(_Y,Ys, Singles0, Multis)
	).

skip_same_vars(_, [], [], []).
skip_same_vars(_X, [_Y|Ys], Singles, Multis) :-
	( _X == _Y ->
	     skip_same_vars(_Y, Ys, Singles, Multis)
	;
	     classify_vars(_Y,Ys, Singles, Multis)
	).


%----------------------------------------------------------------------
% Name checks
%----------------------------------------------------------------------

check_predicate_name(Name, SP) :-
	atom_string(Name, NameS),
	string_list(NameS, NameChars),
	( good_pred_name(NameChars, []) -> true ;
	    message("Questionable predicate name: %w", [Name], SP)
	).

check_module_name(Name, SP) :-
	atom_string(Name, NameS),
	string_list(NameS, NameChars),
	( good_module_name(NameChars, []) -> true ;
	    message("Questionable module name: %w", [Name], SP)
	).

check_varnames(Vars, SP) :-
	( foreach([Name|_], Vars), param(SP) do
	    atom_string(Name, NameS),
	    string_list(NameS, NameChars),
	    ( good_var_name(NameChars, []) -> true ;
	        message("Questionable variable name: %w", [Name], SP)
	    )
	).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Grammar rules for name checking
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

opt_words --> [].
opt_words --> words.

words -->
	upper_case,
	opt_lower_cases,
	opt_words.

opt_digits --> [].
opt_digits -->
	digit,
	opt_digits.

lower_cases -->
	lower_case,
	opt_lower_cases.

opt_lower_cases --> [].
opt_lower_cases -->
	lower_case,
	opt_lower_cases.

opt_ul_lc --> [].
opt_ul_lc --> underline, lower_cases, opt_ul_lc.

opt_symbols --> [].
opt_symbols --> symbol, opt_symbols.

upper_case --> [C], {get_chtab(C,upper_case)}.
lower_case --> [C], {get_chtab(C,lower_case)}.
underline --> [C], {get_chtab(C,underline)}.
digit --> [C], {get_chtab(C,digit)}.
symbol --> [C], {get_chtab(C,Class),symbolclass(Class)}.

    symbolclass(symbol).
    symbolclass(first_comment).
    symbolclass(second_comment).
    symbolclass(escape).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Definitions dependent on coding conventions
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

good_var_name --> underline, opt_words, opt_digits.
good_var_name --> words, opt_digits.

good_pred_name --> lower_cases, opt_ul_lc, opt_digits.
good_pred_name --> symbol, opt_symbols.

good_module_name --> lower_cases, opt_ul_lc.

