%-----------------------------------------------------------------------------
:- module(dual_var).
%-----------------------------------------------------------------------------

:- export dual_var_print/2.

:- meta_attribute(dual_var, [print:dual_var_print/2, unify:unify_dual/2]).
:- export struct(dual_var(dual_val, eplex_idx, type, primal_rhs,
                          primal_lhs_range, susps, pool, next)).

:- export var_dual/6.
:- export get_dual/3.

:- export always_set_dual/3.

:- export set_dual/3.
:- export get_idx/3.
:- export get_rhs/3.
:- export get_lhs_range/3.
:- export set_lhs_range/3.
:- export get_type/3.
:- export satisfiable_primal_cstr/2.

%-----------------------------------------------------------------------------


% dual_var attribute handlers

unify_dual(_, Attribute) :-
    /* NOT A DUAL-VAR */
    var(Attribute).
unify_dual(Term, Attribute) :-
    /* DUAL-VAR */
    compound(Attribute),
    unify_term_dual(Term, Attribute).

unify_term_dual(Value, _) :-
    /* DUAL-VAR and NONVAR - instantiated */
    nonvar(Value),
    true.
unify_term_dual(Y{AttrY}, AttrX) :-
    -?->
    /* DUAL-VAR and VAR */
    unify_dual_dual(Y, AttrY, AttrX).

unify_dual_dual(Y, AttrY, AttrX) :-
    /* DUAL-VAR and NON-DUAL-VAR - share attribute */
    var(AttrY),
    AttrX = AttrY,
    add_attribute(Y, AttrY).
unify_dual_dual(_, AttrY, AttrX) :-
    /* DUAL-VAR and DUAL-VAR - add them */
    % but what to do about eplex indexes?
    % probably something like eplex chain of solver ids
    nonvar(AttrY),
    AttrY = dual_var with [dual_val:DualY],
    AttrX = dual_var with [dual_val:DualX],
    merge_suspension_lists(susps of dual_var, AttrX,
			   susps of dual_var, AttrY),
    (DualY = DualX ->
         true
    ;
         NewDual is DualY + DualX,
         setarg(dual_val of dual_var, AttrY, NewDual),
         schedule_suspensions(susps of dual_var, AttrY),
         wake
    ).

dual_var_print(_{Attr}, Printed) :-
    -?->
    nonvar(Attr),
    printed_dv_attributes(Attr, Printed).

printed_dv_attributes(Attr, Printed) :-
    ( compound(Attr) ->
        Attr = dual_var with [
                              dual_val:Dual,
                              pool:Pool,
                              next:NextAttr
                             ],
        Printed = [Pool:[dual_val:Dual]|Rest],
        printed_dv_attributes(NextAttr, Rest)
    ;
        % chain terminated by atom end
        Printed = []
    ).

% ----------------------------------------------------------------------

% creating a new dual_var variable

var_dual(Var, Dual, Idx, Type, Rhs, Pool) :-
        get_dual_attr(Var, Pool, Attr),
        Attr = dual_var with [
                              dual_val:Dual,
                              eplex_idx:Idx,
                              type:Type,
                              primal_rhs:Rhs
                             ].

get_dual_attr(X{dual_var:Attr0}, Pool, Attr) ?-
        ( var(Attr0) ->
              new_dual_attr(X, Pool, Attr)
        ;
              Attr0 = dual_var with [pool:Pool0, next:Next],
              % should not fail unless Attr0 incorrect
              ( Pool0 == Pool ->
                    Attr = Attr0
              ;
                    get_dual_attr1(Next, Attr0, Pool, Attr)
              )
        ).
get_dual_attr(X, Pool, Attr) :-           % make a new dual_var variable
        free(X),
        new_dual_attr(X, Pool, Attr).

get_dual_attr1(ThisAttr, Attr0, Pool, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_dual_attrstruct(Pool, Attr),
	setarg(next of dual_var, Attr0, Attr).
get_dual_attr1(ThisAttr, _Attr0, Pool, Attr) :-
        ThisAttr = dual_var with [pool:Pool0, next:Next],
        ( Pool0 == Pool ->
              Attr = ThisAttr
        ;
              get_dual_attr1(Next, ThisAttr, Pool, Attr)
        ).

new_dual_attr(X, Pool, Attr) :-                     % make a new bfs variable:
        new_dual_attrstruct(Pool, Attr),
        add_attribute(X, Attr, dual_var).

:- mode new_dual_attrstruct(+,-).
new_dual_attrstruct(Pool, Attr) :-
        Attr = dual_var with [
                              pool:Pool,
                              next:end
                             ],
        init_suspension_list(susps of dual_var, Attr).

% From a dual_var attr, searches for the attribute corresponding to
% that for the first argument. Fails if none found. 
get_dual_attr_for_pool(Pool, Attr0, Attr) :-
        compound(Attr0), 
	get_dual_attr_for_pool1(Pool, Attr0, Attr).

get_dual_attr_for_pool1(Pool, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = dual_var with [pool:Pool0, next:NextAttr],
	(Pool0 == Pool ->
	     Attr0 = Attr
	;    
	     get_dual_attr_for_pool1(Pool, NextAttr, Attr)
	).

% -------------------------------------------------------------------------

satisfiable_primal_cstr(_{Attr0}, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with [
                          type:Type,
                          primal_rhs:Rhs,
                          primal_lhs_range:Min..Max
                         ],
    ground(Min..Max),
    ( Type == (=<) -> Min =< Rhs
    ; Type == (=:=) -> Min =< Rhs, Max >= Rhs
    ; Type == (>=) -> Max >= Rhs ).

% dual_var attribute setting and testing predicates

get_idx(_{Attr0}, Idx, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with eplex_idx:Idx.

get_rhs(_{Attr0}, Rhs, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with primal_rhs:Rhs.

get_lhs_range(_{Attr0}, Range, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with primal_lhs_range:Range.

set_lhs_range(_{Attr0}, Lo..Hi, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with primal_lhs_range:Lo0..Hi0,
    Lo1 is max(Lo, Lo0),
    Hi1 is min(Hi, Hi0),
    setarg(primal_lhs_range of dual_var, Attr, Lo1..Hi1).

get_type(_{Attr0}, Type, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with type:Type.

get_dual(_{Attr0}, Dual, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with dual_val:Dual.

always_set_dual(_{Attr0}, Dual, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with dual_val:Old,
    ( Old =:= Dual ->
        true
    ;
        setarg(dual_val of dual_var, Attr, Dual)
    ),
    schedule_suspensions(susps of dual_var, Attr),
    wake.

set_dual(_{Attr0}, Dual, Pool) :-
    -?->
    get_dual_attr_for_pool(Pool, Attr0, Attr),
    Attr = dual_var with dual_val:Old,
    ( Old =:= Dual ->
        true
    ;
        setarg(dual_val of dual_var, Attr, Dual),
        schedule_suspensions(susps of dual_var, Attr),
        wake
    ).
