% ----------------------------------------------------------------------
%
% Description:	ECLiPSe best-first search library
%
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	(C) IC-Parc, Imperial College London 1995-1999
% Author/s:	Andrew Eremin, IC-Parc
% Version:      $Id: bfs.ecl,v 1.20 2004/12/15 16:39:04 ae5 Exp $
%
% ----------------------------------------------------------------------


% ----------------------------------------------------------------------
:- module(bfs).

:- lib(constraint_pools).
:- lib(eplex).

% ----------------------------------------------------------------------
%
% Meta-attribute
%
% ----------------------------------------------------------------------

:- meta_attribute(bfs, [unify:unify_bfs/2]).

:- local struct(
                bfs(
                    optimal_val,
                    type,
                    node_info,
                    pseudocost:pseudocost,
                    solver,
                    next
                   )
               ).

% ----------------------------------------------------------------------
%
% Bfs var node info structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_node_info(
                              id,   % int: node number
                              lo,   % float: node lower bound
                              hi,   % float: node upper bound
                              val,  % float: node optimal solution value
                              rc    % float: node reduced cost
                             )
               ).

% ----------------------------------------------------------------------
%
% Pools
%
% ----------------------------------------------------------------------

:- export bfs_instance/1.	 % bfs_instance(+PoolName)

:- local struct(bfs_constraint_type(branch)).

% ----------------------------------------------------------------------
%
% Best-first search tree structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_tree(
                         pool,           % the pool with which the
                                         % solver is associated
                         module,         % caller module
                         
                         data,           % arbitrary prob data that
                                         % branches may want to access:
                                         % probably variables in some
                                         % structured form since
                                         % branches are stored over
                                         % failure and cannot contain
                                         % explicit vars
                         feas_check,     % the arbitrary global
                                         % feasibility check
                                         % (intention is other than
                                         % variable integrality)
                         
                         shelf,          % ECLiPSe shelf for storing
                                         % various results over failures
                         cost,           % var: variable for the
                                         %      optimal cost
                         integral_obj,   % atom: yes or no, is the cost
                                         %       of all feasible
                                         %       solutions integral?
                         int_tolerance,  % float: integrality tolerance
                         sense,          % min or max: optimisation sense
                         node_susp,      % suspension list containing
                                         % the suspension for the node 
                                         % solver 
                         vars,           % []: list of all vars currently
                                         %     belonging to this tree
                         nodes,          % [Node1,...,Nodek]:
                                         %       list of unsolved nodes
                                         %       awaiting solution
                                         %       with local state in
                                         %       node_select order
                         next_node_no,   % int: the id for the next
                                         %      node created
                         best_bound,     % float: current best global
                                         %        feasible solution
                                         %        objective value
                         gubs,           % [GUB1,...,GUBm]: list of
                                         %    bfs_gub structures for
                                         %    the GUBs of this problem
                         stats:stats,    % structure holding
                                         % statistics info
                         info_messages,  % on, off: info message status
                                         % Search Control: node selection
                         node_select,    % atom: best_first, depth_first,
                                         %       best_estimate
                                         %       node selection criteria
                                         % Search Control: problem division
                         alpha_min,      % float: constants used in
                         alpha_max,      % float: utilising variable
                                         %        up/down estimations
                                         %        for branching:
                                         %        variable estimation =
                                         %         alpha_min *
                                         %          min(up/down est) +
                                         %         alpha_max *
                                         %          max(up/down est)
                         beta_const,     % float: constants used in
                         beta_pc,        % float: calculating variable
                         beta_lb,        % float: up/down estimations:
                                         %        up/down estimation = 
                                         %         beta_const + 
                                         %         beta_pc * pseudocost + 
                                         %         beta_lb * lowerbound
                         pcd_average,    % float: average observed
                                         %        down-branch pseudocost
                         pcd_count,      % int: number of observed
                                         %      down-branches
                         pcu_average,    % float: average observed
                                         %        up-branch pseudocost
                         pcu_count,      % int: number of observed
                                         %      up-branches
                         pc_init,        % atom: average,calculated,cost:
                                         %       pseudocost initialization
                                         %       method:
                                         %       set to current average,
                                         %       calulated,
                                         %       set to objective cost
                                         %       coefficient
                         pc_ratio,       % float: ratio in (0.0..1.0)
                                         %        of max work in calculating
                                         %        pseudocosts to root node
                                         %        solution work
                         pc_update,      % atom: average,first,last:
                                         %       pseudocost update method
                                         %       set to average/first/last
                                         %       of observed degradations
                         lb_time         % int: time limit in seconds
                                         %      for lower bound calculation
                        )
               ).

% ----------------------------------------------------------------------
%
% Branch-and-Price search tree node structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_node(
                         id,          % int: node number
                         state,       % arbitrary local state for the
                                      % node solver
                         rank,        % node ranking for node selection
                         objval,      % float: relaxed optimal
                                      %        solution value
                         parent_obj,  % float: relaxed optimal
                                      %        solution value of
                                      %        parent node
                                      % these two are needed when
                                      % using pseudocost-based
                                      % methods
                         pc_update,   % pseudocost to update after
                                      % solving node: [GUBNo, BPNo, Dir]
                         frac_vars,   % []: integer vars with fractional
                                      %     node optimal solution 
                         branches     % []: current branching decisions
                        )
               ).

% ----------------------------------------------------------------------
%
% Generalised Upper/Lower bound structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_gub(
                        vars,
                        refs,
                        pseudocosts
                       )
               ).

% ----------------------------------------------------------------------
%
% Pseudocost structure
%
% ----------------------------------------------------------------------

:- local struct(
                pseudocost(
                           pcd,
                           pcd_count,
                           pcu,
                           pcu_count
                          )
               ).

% ----------------------------------------------------------------------
%
% Statistics structure
%
% ----------------------------------------------------------------------

:- local struct(
                stats(
                      start_time,
                      total_time,
                      first_sol_time,
                      opt_sol_time,
                      no_sols,
                      solve_time,
                      separate_time,
                      nodes_separated,
                      nodes_solved
                     )
               ).

% ----------------------------------------------------------------------
%
% bfs attribute handlers
%
% ----------------------------------------------------------------------

unify_bfs(_, Attr) :-
        var(Attr).                   % Ignore if not a bfs var
unify_bfs(Term, Attr) :-
        compound(Attr),
        unify_term_bfs(Term, Attr).

:- mode unify_term_bfs(?, +).
unify_term_bfs(X, Attr) :-
        nonvar(X),                   % bfs var and NONVAR - instantiated
        instantiation_deviates_for_bfs_pool(Attr, X).
unify_term_bfs(Y{bfs:AttrY}, AttrX) :-
        -?->
        unify_bfs_bfs(Y, AttrY, AttrX).

unify_bfs_bfs(_, AttrY, AttrX) :-
        var(AttrY),	            % No attribute for this extension
        AttrX = AttrY.	            % Transfer the attribute
unify_bfs_bfs(_, AttrY, AttrX) :-
        nonvar(AttrY),              % bfs var and bfs var
        unify_bfs_pools(AttrX, AttrY).

instantiation_deviates_for_bfs_pool(ThisAttr, X) :-
        ( compound(ThisAttr) ->
              ThisAttr = bfs with [
                                   solver:Pool,
                                   optimal_val:Val,
                                   next:NextAttr
                                  ],
              ( X = Val -> % instantiated to its optimal_val
                    true
              ;     % otherwise wake solver
                    get_pool_handle(Handle, Pool),
                    ( Handle = bfs_tree with nodes:[] ->
                          % finished solving so this was a true optimal
                          schedule_suspensions(node_susp of bfs_tree, Handle),
                          wake
                    ;
                          % Still processing: the optimal val is only
                          % current optimal in the tree, and quite
                          % likely the instantiation has been caused
                          % by the tree search at another node, so
                          % do not rewake the solver now because it
                          % could fail incorrectly.
                          true
                    )
              ),
              instantiation_deviates_for_bfs_pool(NextAttr, X)
	;    
              % chain terminated by atom 'end'
              true
        ).

unify_bfs_pools(ThisAttrX, AttrY) :-
        ThisAttrX = bfs with [
                              solver:Pool,
                              next:NextAttrX
                             ],
        remove_bfs_attr_for_pool(Pool, AttrY, ThisAttrY, NextAttrY),
        ( compound(ThisAttrY) ->
              % two variables in the same solver are unified,
              % send an equality constraint to the solver and wake it
              % Note to AE: how do I get back the vars?
              get_pool_handle(Handle, Pool),
              schedule_suspensions(node_susp of bfs_tree, Handle),
              wake
        ;
              % Y has no attribute for Pool
              ThisAttrY = end
        ),
        % continue with the rest of X and Ys chains
        unify_bfs_pools(NextAttrX, ThisAttrX, NextAttrY).

unify_bfs_pools(ThisAttrX, Attr0, AttrY) :-
        ( compound(ThisAttrX) ->
              ( compound(AttrY) ->
                    ThisAttrX = bfs with [
                                          solver:Pool,
                                          next:NextAttrX
                                         ],
                    remove_bfs_attr_for_pool(Pool, AttrY, ThisAttrY, NextAttrY),
                    ( compound(ThisAttrY) ->
                          % two variables in the same solver are unified,
                          % send an equality constraint for the two columns
                          % to the external solver and wake it
                          % Note to AE: how do I get back the vars?
                          get_pool_handle(Handle, Pool),
                          schedule_suspensions(node_susp of bfs_tree, Handle),
                          wake
                    ;
                          % Y has no attribute for Pool
                          ThisAttrY = end
                    ),
                    % continue with the rest of X and Ys chains
                    unify_bfs_pools(NextAttrX, ThisAttrX, NextAttrY)
              ;
                    % Ys chain terminated by atom'end'
                    true
              )
        ;
              % Xs chain terminated by atom 'end'
              % put the rest of Ys chain here
              setarg(next of bfs, Attr0, AttrY)
        ).

% From a bfs_attr, removes the attribute corresponding to that for the
% first argument form the chain and returns it. Fails if none found. 
remove_bfs_attr_for_pool(Pool, ThisAttr, Attr, RestAttr) :-
        % no need to test for var(ThisAttr) in chain
        ThisAttr = bfs with [solver:ThisPool, next:NextAttr],
	(ThisPool == Pool ->
             RestAttr = NextAttr,
             setarg(next of bfs, ThisAttr, end),
	     Attr = ThisAttr
	;    
             RestAttr = ThisAttr,
	     dechain_bfs_attr_for_pool1(Pool, NextAttr, ThisAttr, Attr)
	).
        
dechain_bfs_attr_for_pool1(Pool, ThisAttr, Attr0, Attr) :-
        % no need to test for var(ThisAttr) in chain
        ( ThisAttr = bfs with [solver:ThisPool, next:NextAttr] ->
              (ThisPool == Pool ->
                   setarg(next of bfs, Attr0, NextAttr),
                   setarg(next of bfs, ThisAttr, end),
                   Attr = ThisAttr
              ;    
                   dechain_bfs_attr_for_pool1(Pool, NextAttr, ThisAttr, Attr)
              )
        ;     % chain terminated by atom 'end'
              ThisAttr = Attr
        ).

% ----------------------------------------------------------------------

get_bfs_attr(X{bfs:Attr0}, Pool, Attr) ?-
        ( var(Attr0) ->
              new_bfs_attr(X, Pool, Attr)
        ;
              Attr0 = bfs with [solver:Pool0,next:Next],
              % should not fail unless Attr0 incorrect
              ( Pool0 == Pool ->
                    Attr = Attr0
              ;
                    get_bfs_attr1(Next, Attr0, Pool, Attr)
              )
        ).
get_bfs_attr(X, Pool, Attr) :-           % make a new bfs variable
        free(X),
        new_bfs_attr(X, Pool, Attr).

get_bfs_attr1(ThisAttr, Attr0, Pool, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_bfs_attrstruct(Pool, Attr),
	setarg(next of bfs, Attr0, Attr).
get_bfs_attr1(ThisAttr, _Attr0, Pool, Attr) :-
        ThisAttr = bfs with [solver:Pool0,next:Next],
        ( Pool0 == Pool ->
              Attr = ThisAttr
        ;
              get_bfs_attr1(Next, ThisAttr, Pool, Attr)
        ).

new_bfs_attr(X, Pool, Attr) :-                       % make a new bfs variable:
        new_bfs_attrstruct(Pool, Attr),
        add_attribute(X, Attr, bfs),                 % add a bfs attribute
        get_pool_item(Pool, BFSTree),
        BFSTree = bfs_tree with vars:Vars,
        ( var(Vars) -> Vars0 = [X] ; Vars0 = [X|Vars] ),
        setarg(vars of bfs_tree, BFSTree, Vars0), % add it to the bfs tree
        true.

:- mode new_bfs_attrstruct(+,-).
new_bfs_attrstruct(Pool, Attr) :-
        Attr = bfs with [
                         type: real,
                         node_info:[],
                         pcu: 0,
                         pcu_count: none,
                         pcd: 0,
                         pcd_count: none,
                         solver:Pool,
                         next:end
                        ].

% From a bfs_attr, searches for the attribute corresponding to that for the
% first argument. Fails if none found. 
get_bfs_attr_for_pool(Pool, Attr0, Attr) :-
        compound(Attr0), 
	get_bfs_attr_for_pool1(Pool, Attr0, Attr).

get_bfs_attr_for_pool1(Pool, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = bfs with [solver:Pool0,next:NextAttr],
	(Pool0 == Pool ->
	     Attr0 = Attr
	;    
	     get_bfs_attr_for_pool1(Pool, NextAttr, Attr)
	).

get_node_info(Id, Val, [V|Rest]) :-
        V = bfs_node_info with id:I,
        ( Id > I ->
              get_node_info(Id, Val, Rest)
        ;
          Id = I ->
              Val = V
        ).

bfs_get_node_info(Var, NodeId, Val, _Pool) :-
        nonvar(Var), !,
        Val = bfs_node_info with [
                                  id:NodeId,
                                  lo:Var,
                                  hi:Var,
                                  val:Var,
                                  rc:0
                                 ].
bfs_get_node_info(Var{bfs:Attr0}, NodeId, Val, Pool) ?-
        get_bfs_attr_for_pool(Pool, Attr0, Attr),
        Attr = bfs with [
                         type:Type,
                         node_info:Vals
                        ],
        nonvar(NodeId),
        ( get_node_info(NodeId, Val, Vals) ->
            % found a bfs_node_info struct for this node
            true
        ;
            % no bfs_node_info struct for this node,
            % implicitly has original bounds, 0 val and rc
            Val = bfs_node_info with [
                                      id:NodeId,
                                      lo:NLo,
                                      hi:NHi,
                                      val:0,
                                      rc:0
                                     ],
            get_var_bounds(Var, Lo, Hi),
            ( Type == integer ->
                  NLo is ceiling(Lo),
                  NHi is floor(Hi)
            ;
                  NLo = Lo,
                  NHi = Hi
            )
        ).

insert_node_info(Id, Val, Vals, NewVals) :-
        ( Vals = [V|Rest] ->
              V = bfs_node_info with id:I,
              ( Id > I ->
                    NewVals = [V|Rest0],
                    insert_node_info(Id, Val, Rest, Rest0)
              ; Id =:= I ->
                    % we are actually updating info,
                    % not inserting a new structure
                    Val = bfs_node_info with [
                                              lo:Lo,
                                              hi:Hi,
                                              val:T,
                                              rc:RC
                                             ],
                    V = bfs_node_info with [
                                            lo:L,
                                            hi:H
                                           ],
                    ( var(Lo) -> true
                    ; var(L) -> setarg(lo of bfs_node_info, V, Lo)
                    ; NL is max(Lo, L), setarg(lo of bfs_node_info, V, NL) ),
                    ( var(Hi) -> true
                    ; var(H) -> setarg(hi of bfs_node_info, V, Hi)
                    ; NH is min(Hi, H), setarg(hi of bfs_node_info, V, NH) ),
                    ( var(T) -> true ; setarg(val of bfs_node_info, V, T) ),
                    ( var(RC) -> true ; setarg(rc of bfs_node_info, V, RC) ),
                    NewVals = Vals
              ; Id < I ->
                    % place here in list
                    NewVals = [Val, V|Rest]
              )
        ;
              % empty list, insert here
              NewVals = [Val]
        ).

bfs_new_node_info(Var, Node, Val, Pool) :-
	get_bfs_attr(Var, Pool, Attr),
        Attr = bfs with [type:T, node_info:Vals],
        Node = bfs_node with [id:Id, frac_vars:Vars],
        Val = bfs_node_info with [id:Id, val:V],
        ( ( nonvar(V), T == integer,
            get_pool_handle(bfs_tree with int_tolerance:IntTol, Pool),
            abs(V-round(V)) > IntTol ) ->
              setarg(frac_vars of bfs_node, Node, [Var:V|Vars])
        ;
              true
        ),
        insert_node_info(Id, Val, Vals, NewVals),
        setarg(node_info of bfs, Attr, NewVals).

bfs_node_info(Var, Lo, Hi, Val, RC, Pool) :-
        Info = bfs_node_info with [lo:Lo, hi:Hi, val:Val, rc:RC],
        bfs_node_info(Var, Info, Pool).

bfs_node_info(Var, Val, Pool) :-
        get_pool_item(Pool, Handle), 
	( Handle == 0 ->
              printf(error, "Bfs instance has no tree set up in %w%n",
                     [Pool:node_info(Var, Val)]),
              abort
	;
              Handle = bfs_tree with nodes:[Node|_],
              ( var(Val) ->
                    Node = bfs_node with id:Id,
                    bfs_get_node_info(Var, Id, Val, Pool)
              ;
                    bfs_new_node_info(Var, Node, Val, Pool)
              )
        ).

bfs_get_optimal_val(_{bfs:Attr0}, OptVal, Pool) ?-
        get_bfs_attr_for_pool(Pool, Attr0, bfs with optimal_val:OptVal).

bfs_get_type(_{bfs:Attr0}, Type, Pool) ?-
        get_bfs_attr_for_pool(Pool, Attr0, bfs with type:Type).

bfs_var_get(Var, node_val, Val, Pool) :- !,
        get_pool_item(Pool, Handle), 
	( Handle == 0 ->
              printf(error, "Bfs instance has no tree set up in %w%n",
                     [Pool:var_get(Var, node_val, Val)]),
              abort
	; Handle = bfs_tree with nodes:[bfs_node with id:Id|_] ->
              ( number(Var) -> Val = Var
              ; bfs_get_node_info(Var, Id, bfs_node_info with val:Val, Pool) )
        ;
              Handle = bfs_tree with nodes:[],
              printf(error, "Bfs tree has no current node in %w%n",
                     [Pool:var_get(Var, node_val, Val)]),
              abort
        ).
bfs_var_get(Var, optimal_val, Val, Pool) :- !,
        ( number(Var) ->
              Val = Var
        ;
              bfs_get_optimal_val(Var, Val, Pool)
        ).
bfs_var_get(Var, type, Val, Pool) :- !,
        ( integer(Var) ->
              Val = integer
        ; number(Var) ->
              Val = real
        ;
              bfs_get_type(Var, Val, Pool)
        ).
bfs_var_get(Var, What, Val, Pool) :-
	error(6, bfs_var_get(Var, What, Val, Pool)).

bfs_var_set(_{bfs:Attr0}, optimal_val, Val, Pool) ?- !,
        get_bfs_attr_for_pool(Pool, Attr0, Attr),
        ( Attr = bfs with type: integer ->
              Opt is fix(float(Val))
        ;
              Opt = Val
        ),
        setarg(optimal_val of bfs, Attr, Opt).
bfs_var_set(Var, What, Val, Pool) :-
	error(6, bfs_var_set(Var, What, Val, Pool)).

bfs_integers(Ints, Pool) :-
        ( var(Ints) ->
              get_bfs_attr(Ints, Pool, Attr),
              setarg(type of bfs, Attr, integer)
        ;
              (
                  foreach(Var, Ints),
                  param(Pool)
              do
                  get_bfs_attr(Var, Pool, Attr),
                  setarg(type of bfs, Attr, integer)
              )
        ).

bfs_branch(Branch, Pool) :-
        post_typed_pool_constraint(Pool, branch of bfs_constraint_type,
                                   Branch).

bfs_info_message(bfs_tree with info_messages:OnOff, String, Vars) :-
        ( OnOff == on -> printf(String, Vars) ; true ).

bfs_statistics(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                next_node_no:NTotal,
                                stats:Stats
                               ],
        Stats = stats with [
                            total_time:TTotal,
                            nodes_solved:NSolve,
                            solve_time:TSolve,
                            nodes_separated:NSep,
                            separate_time:TSep,
                            no_sols:NSol,
                            first_sol_time:TFirst,
                            opt_sol_time:TOpt
                           ],
        printf("%n%n%w:statistics:%n%tTotal nodes:"
                         " %d%n%tTotal time: %w" 
                         "%n%tNodes solved: %d%n%tNode solution time: %w"
                         "%n%tNodes separated: %d%n%tNode separation time: %w"
                         "%n%tSolutions found: %d%n%tFirst solution time: %w"
                         "%n%tOptimal solution time: %w%n%n",
                         [Pool, NTotal, TTotal, NSolve, TSolve,
                          NSep, TSep, NSol, TFirst, TOpt]).

bfs_node_cost(NodeCost, Pool) :-
        get_pool_handle(bfs_tree with nodes:[Node|_], Pool),
        setarg(objval of bfs_node, Node, NodeCost).

bfs_get(What, Val, Pool) :-
        nonvar(What),!,
        get_pool_item(Pool, Handle), 
	( Handle == 0 ->
	    printf(error, "Bfs instance has no tree set up in %w%n",
		[Pool:get(What, Val)]),
	    abort
	;
	    bfs_get1(What, Handle, Val)
	).
bfs_get(What, Val, Pool) :-
        error(4, Pool:get(What, Val)).

bfs_get1(frac_vars, Handle, Val) :- !,
        Handle = bfs_tree with nodes:[bfs_node with frac_vars:Vars|_],
        ( foreach(Var:_, Vars), foreach(Var, Val) do true ).
bfs_get1(branches, bfs_tree with nodes:[bfs_node with branches:Val|_],
         Val) :- !.
bfs_get1(data, bfs_tree with data:Val, Val) :- !.
bfs_get1(What, bfs_tree with pool:Pool, Val) :-
	error(6, Pool:get(What, Val)).

bfs_set(What, Val, Pool) :-
        nonvar(What),
        nonvar(Val),!,
        get_pool_item(Pool, Handle), 
	( Handle == 0 ->
	    printf(error, "Bfs instance has no tree set up in %w%n",
		[Pool:set(What, Val)]),
	    abort
	;
	    bfs_set1(What, Handle, Val)
	).
bfs_set(What, Val, Pool) :-
        error(4, Pool:set(What, Val)).

bfs_set1(feas_check, Handle, Val) :- !,
        functor(Val, Functor, Arity),
        Handle = bfs_tree with module:Module,
        current_predicate(Functor/Arity)@Module,
        setarg(feas_check of bfs_tree, Handle, Val).
bfs_set1(data, Handle, Val) :- !,
        setarg(data of bfs_tree, Handle, Val).
bfs_set1(node_select, Handle, Val) :- !,
        setarg(node_select of bfs_tree, Handle, Val).
bfs_set1(alpha_min, Handle, Val) :- !,
        setarg(alpha_min of bfs_tree, Handle, Val).
bfs_set1(alpha_max, Handle, Val) :- !,
        setarg(alpha_max of bfs_tree, Handle, Val).
bfs_set1(beta_const, Handle, Val) :- !,
        setarg(beta_const of bfs_tree, Handle, Val).
bfs_set1(beta_pc, Handle, Val) :- !,
        setarg(beta_pc of bfs_tree, Handle, Val).
bfs_set1(beta_lb, Handle, Val) :- !,
        setarg(beta_lb of bfs_tree, Handle, Val).
bfs_set1(pc_init, Handle, Val) :- !,
        setarg(pc_init of bfs_tree, Handle, Val).
bfs_set1(pc_update, Handle, Val) :- !,
        setarg(pc_update of bfs_tree, Handle, Val).
bfs_set1(pc_ratio, Handle, Val) :- !,
        setarg(pc_ratio of bfs_tree, Handle, Val).
bfs_set1(lb_time, Handle, Val) :- !,
        setarg(lb_time of bfs_tree, Handle, Val).
bfs_set1(int_tolerance, Handle, Val) :- !,
        setarg(int_tolerance of bfs_tree, Handle, Val).
bfs_set1(info_messages, Handle, Val) :- !,
        setarg(info_messages of bfs_tree, Handle, Val).
bfs_set1(What, bfs_tree with pool:Pool, Val) :-
	error(6, Pool:set(What, Val)).


:- tool(bfs_solver_setup/3, bfs_solver_setup1/4).

bfs_solver_setup1(Sense, Solver, Pool, Module) :-
        bfs_solver_setup(Sense, Solver, [], Pool, Module).

:- tool(bfs_solver_setup/4, bfs_solver_setup/5).

bfs_solver_setup(Sense, Solver, OptionList, Pool, Module) :-
        ( var(Solver) ->
              error(4, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ;
              true
        ),
        ( var(Sense) ->
              error(4, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ; Sense == min ->
              true
        ; Sense == max ->
              true
        ; error(6, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ),
        get_pool_handle(Handle, Pool),
        % setup tree
        Handle = bfs_tree with [
                                module:Module,
                                shelf:Shelf,
                                sense:Sense,
                                cost:CostVar,
                                best_bound:1.0Inf,
                                nodes:[RootNode],
                                next_node_no:1,
                                pool:Pool
                               ],
        init_suspension_list(node_susp of bfs_tree, Handle),
        set_var_bounds(CostVar, -1.0Inf, 1.0Inf),
        % create a shelf for storing info over failures
        shelf_create(info(0), Shelf),
        % create a root node bfs_node struct
        RootNode = bfs_node with [
                                  id:0,
                                  rank: Rank,
                                  branches:[]
                                 ],
        ( atom(Solver), is_constraint_pool(Solver) ->
              get_pool_item(Solver, Item),
              % if the solver supplied is a constraint pool name we
              % have built-in node optimization and separation
              % predicates for eplex problems, otherwise
              % they must be user-supplied
              %( Item = prob with [vars:VarArr, objcoeffs:Expr] ->
              ( functor(Item, prob, _) ->
                    Solver:eplex_get(vars, VarArr),
                    Solver:eplex_get(objective, Objective),
                    Objective =.. [Sense, Expr],
                    Solve = Pool:minimize_eplex_node(Item),
                    lp_set(Item, reduced_cost, yes),
                    lp_set(Item, keep_basis, yes),
                    RootNode = bfs_node with state:[],
                    DefaultSeparation = Pool:deg_est(eplex(Item)),
                    %( bfs_integral_objective(Expr, VarArr, Pool) ->
                    ( bfs_integral_objective(Expr, Pool) ->
                          setarg(integral_obj of bfs_tree, Handle, yes)
                    ;
                          true
                    ),
                    % process option list and fill in default values
                    process_options(OptionList, Handle, Item, Separation),
                    fill_in_defaults(Handle, DefaultSeparation, Separation),
                    % try to identify GUB constraints to improve branching
                    gub_constraints(Item, Handle, Pool)
              ;
                    concat_string([Pool,
                                   ": no built-in node optimization or"
                                   " separation predicates for pool ",
                                   Solver], Message),
                    writeln(warning_output, Message),
                    abort
              )
        %; Solver = prob with [vars:VarArr, objcoeffs:Expr] ->
        ; functor(Solver, prob, _) ->
              lp_get(Solver, vars, VarArr),
              lp_get(Solver, objective, Objective),
              Objective =.. [Sense, Expr],
              Solve = Pool:minimize_eplex_node(Solver),
              lp_set(Solver, reduced_cost, yes),
              lp_set(Solver, keep_basis, yes),
              RootNode = bfs_node with state:[],
              DefaultSeparation = Pool:deg_est(eplex(Solver)),
              %( bfs_integral_objective(Expr, VarArr, Pool) ->
              ( bfs_integral_objective(Expr, Pool) ->
                    setarg(integral_obj of bfs_tree, Handle, yes)
              ;
                    true
              ),
              % process option list and fill in default values
              process_options(OptionList, Handle, Solver, Separation),
              fill_in_defaults(Handle, DefaultSeparation, Separation),
              % try to identify GUB constraints to improve branching
              gub_constraints(Solver, Handle, Pool)
        ;
              Solve = Solver,
              % process option list and fill in default values
              process_options(OptionList, Handle, Separation),
              fill_in_defaults(Handle, DefaultSeparation, Separation)
        ),
        Handle = bfs_tree with node_select:NodeSelect,
        ( ( Solve = Pool:minimize_eplex_node(_),
            lp_get(presolve, 1),
            lp_get(optimizer, xpress),
            (Separation = Pool:deg_est(_); Separation = Pool:strong(_)) ) ->
              % We disallow the use of XPRESS with estimate-based
              % methods on pre-solved problems because IF there is an
              % abort (probably hitting iteration limit before finding
              % a feasible basis) AND the external solver is XPRESS-MP
              % AND the problem was pre-solved, THEN the variable
              % column numbers get returned in the wrong order causing
              % incorrect solutions at future nodes
              Separation1 = Pool:fracvar,
              ( NodeSelect == best_estimate ->
                    % have to change the node selection method also
                    % since best_estimate requires some sort of
                    % estimate-based spearation method
                    setarg(node_select of bfs_tree, Handle, best_first),
                    NewNS = best_first
              ;
                    NewNS = NodeSelect
              ),
              bfs_info_message(Handle, "%w : cannot use lower-bounding based"
                               " estimate methods for variable/branch"
                               " selection with pre-solved XPRESS problems,"
                               " parameters changed to%n%tseparation: fracvar"
                               " %n%tnode_select: %w%n", [Pool, NewNS])
        ;
              Separation1 = Separation
        ),
        Handle = bfs_tree with node_select:NodeSelect0,
        ( NodeSelect0 == best_first ->
              Rank = -1.0Inf
        ; NodeSelect0 == depth_first ->
              Rank = ""
        ; NodeSelect0 == best_estimate ->
              Rank = -1.0Inf
        ; NodeSelect0 == two_phase ->
              Rank = ""
        ;
              error(4, bfs_solver_setup(Solver, OptionList, Pool))
        ),
        % make a suspension for the node processing
        % and insert it in the node_susp suspension list of Handle
        make_suspension(bfs_body(Handle, Solve, Separation1, Pool),
                        8, Susp),
        enter_suspension_list(node_susp of bfs_tree, Handle, Susp).

bfs_solver_cleanup(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with node_susp:SuspList,
        ( foreach(Susp, SuspList) do kill_suspension(Susp) ).

bfs_solve(Obj, Pool) :-
        get_pool_handle(Handle, Pool),
        cputime(StartTime),
        setarg(start_time of bfs_tree, Handle, StartTime),
        setarg(no_sols of bfs_tree, Handle, 0),
        Handle = bfs_tree with sense:Sense,
        ( Sense == min ->
            setarg(best_bound of bfs_tree, Handle, 1.0Inf)
        ; Sense == max ->
            setarg(best_bound of bfs_tree, Handle, -1.0Inf)
        ),
        schedule_suspensions(node_susp of bfs_tree, Handle),
        wake,
        cputime(EndTime),
        TotalTime is EndTime - StartTime,
        setarg(total_time of bfs_tree, Handle, TotalTime),
        % return the optimal solution value
        Handle = bfs_tree with [
                                node_select:NodeSelect,
                                best_bound:Obj
                               ],
        ( Sense == min ->
              Obj < 1.0Inf
        ; Sense == max ->
              Obj > -1.0Inf
        ),
        % replace the root node for later resolution
        ( NodeSelect == best_first ->
              Rank = -1.0Inf
        ; NodeSelect == depth_first ->
              Rank = ""
        ; NodeSelect == best_estimate ->
              Rank = -1.0Inf
        ; NodeSelect == two_phase ->
              Rank = ""
        ),
        RootNode = bfs_node with [
                                  id:0,
                                  rank: Rank,
                                  branches:[]
                                 ],
        setarg(nodes of bfs_tree, Handle, [RootNode]).

gub_constraints(EplexHandle, Handle, Pool) :-
        % we need to find gub constraints in the problem and
        % identify suitable reference rows for them
        Handle = bfs_tree with int_tolerance:IntTol,
        lp_get(EplexHandle, constraints_norm, NormCstrs),
        (
            foreach(Type:[Const*One|VarTerms], NormCstrs),
            fromto(OtherCstrs, Out, In, []),
            fromto(GUBSets, GOut, GIn, []),
            param(IntTol, Pool)
        do
            ( Type == (>=) ->
                  % not a GUB
                  Out = [Type:[Const*One|VarTerms]|In],
                  GOut = GIn
            ; gub_structure(VarTerms, GUBSet, Const, IntTol, Pool) ->
                  % found a GUB constraint
                  Out = In,
                  GOut = [GUBSet|GIn]
            ;
                  % not a GUB
                  Out = [Type:[Const*One|VarTerms]|In],
                  GOut = GIn
            )
        ),
        (
            foreach(GUBSet, GUBSets),
            fromto(GUBs, Out, In, []),
            param(OtherCstrs)
        do
            ( reference_row(OtherCstrs, GUBSet, Vars, RefTerms, PCStructs) ->
                  % found a suitable reference row for this GUB
                  % create a bfs_gub structure
                  GUB = bfs_gub with [
                                      vars:Vars,
                                      refs:RefTerms,
                                      pseudocosts:PCStructs
                                     ],
                  Out = [GUB|In]
            ;
                  % cannot find a suitable reference row for
                  % this GUB
                  Out = In
            )
        ),
        ( GUBs == [] ->
              true
        ;
              GUBStruct =.. [[]|GUBs],
              setarg(gubs of bfs_tree, Handle, GUBStruct),
              Handle = bfs_tree with nodes:[RootNode],
              setarg(branches of bfs_node, RootNode,
                     [lp_add(EplexHandle, [], [])])
        ).

process_options([], _, _, _) ?- !, true.
process_options([O|Os], Handle, EplexHandle, Separation) ?- !,
	process_option(O, Handle, EplexHandle, Separation),
	process_options(Os, Handle, EplexHandle, Separation).
process_options(_NonList, Handle, _, _) :-
        Handle = bfs_tree with pool:Pool,
        bfs_info_message(Handle, "%w : options not proper list."
                         " Ignored.%n,", [Pool]).

process_option(separation(S), bfs_tree with pool:Pool, Eplex, Separation) ?- !,
        process_separation(S, Pool, Eplex, Separation).
process_option(O, Handle, _, Separation) ?- !,
        process_option(O, Handle, Separation).

process_separation(Val, Pool, Eplex, Separation) :- !,
        ( Val = deg_est -> Separation = Pool:deg_est(eplex(Eplex))
        ; Val = fracvar -> Separation = Pool:fracvar
        ; Val = strong -> Separation = Pool:strong(eplex(Eplex))
        ; Val = enhanced -> Separation = Pool:enhanced(eplex(Eplex))
        ; Val = gub -> Separation = Pool:gub
        ; Separation = Val ).

process_options([], _, _) ?- !, true.
process_options([O|Os], Handle, Separation) ?- !,
	process_option(O, Handle, Separation),
	process_options(Os, Handle, Separation).
process_options(_NonList, Handle, _) :-
        Handle = bfs_tree with pool:Pool,
        bfs_info_message(Handle, "%w : options not proper list."
                         " Ignored.%n,", [Pool]).

process_option(separation(Separation), _Handle, Separation) :- !.
process_option(global_feasibility_check(Val), Handle, _) ?- !,
        bfs_set1(feas_check, Handle, Val).
process_option(data(Val), Handle, _) ?- !,
        bfs_set1(data, Handle, Val).
process_option(node_select(Val), Handle, _) ?- !,
        bfs_set1(node_select, Handle, Val).
process_option(alpha(AlphaMin, AlphaMax), Handle, _) ?- !,
	bfs_set1(alpha_min, Handle, AlphaMin),
	bfs_set1(alpha_max, Handle, AlphaMax).
process_option(beta(BetaConst, BetaPC, BetaLB), Handle, _) ?- !,
	bfs_set1(beta_const, Handle, BetaConst),
	bfs_set1(beta_pc, Handle, BetaPC),
	bfs_set1(beta_lb, Handle, BetaLB).
process_option(pseudo_cost(PCInit, PCUpdate, PCRatio), Handle, _) ?- !,
	bfs_set1(pc_init, Handle, PCInit),
	bfs_set1(pc_update, Handle, PCUpdate),
        ( var(PCRatio) -> true
        ; 0 < PCRatio, PCRatio < 1 -> bfs_set1(pc_ratio, Handle, PCRatio)
        ; bfs_info_message(Handle, "Invalid pc_ratio (ignored): %w%n", [PCRatio]) ).
process_option(lower_bound(Val), Handle, _) ?- !,
	bfs_set1(lb_time, Handle, Val).
process_option(int_tolerance(Val), Handle, _) ?- !,
        ( 0 =< Val, Val =< 0.5 -> bfs_set1(int_tolerance, Handle, Val)
        ; bfs_info_message(Handle, "Invalid int_tolerance (ignored): %w%n", [Val]) ).
process_option(info_messages(Val), Handle, _) ?- !,
        bfs_set1(info_messages, Handle, Val).
process_option(NoOpt, Handle, _) :-
	bfs_info_message(Handle, "Invalid option (ignored): %w%n", [NoOpt]).

fill_in_defaults(Handle, DefaultSeparation, Separation) :-
        Handle = bfs_tree with [
                                data:Data,
                                feas_check:FeasCheck,
                                integral_obj:IntObj,
                                int_tolerance:IntTol,
                                vars:Vars,
                                node_select:NodeSelect,
                                pcd_count:0-0,
                                pcu_count:0-0,
                                alpha_min:AlphaMin,
                                alpha_max:AlphaMax,
                                beta_const:BetaConst,
                                beta_pc:BetaPC,
                                beta_lb:BetaLB,
                                pc_init:PCInit,
                                pc_update:PCUpdate,
                                pc_ratio:PCRatio,
                                lb_time:LB,
                                total_time:0,
                                no_sols:0,
                                solve_time:0,
                                separate_time:0,
                                nodes_separated:0,
                                nodes_solved:0,
                                info_messages:OnOff,
                                pool:Pool
                               ],
        ( var(Data) -> Data = 0 ; true ),
        ( var(FeasCheck) -> FeasCheck = true ; true ),
        ( var(IntObj) -> IntObj = no ; true ),
        ( var(IntTol) -> IntTol = 1e-05 ; true ),
        ( var(Vars) -> Vars = [] ; true ),
        ( var(Separation) ->
              ( var(DefaultSeparation) ->
                    concat_string([Pool,
                                   ": no node separation method"
                                   " specified."], Message),
                    writeln(warning_output, Message),
                    abort
              ;
                    Separation = DefaultSeparation
              )
        ;
              true
        ),
        ( var(NodeSelect) -> NodeSelect = best_first ; true ),
        ( var(AlphaMin) -> AlphaMin = 2 ; true ),
        ( var(AlphaMax) -> AlphaMax = 1 ; true ),
        ( var(BetaConst) -> BetaConst = 0 ; true ),
        ( var(BetaPC) -> BetaPC = 1 ; true ),
        ( var(BetaLB) -> BetaLB = 1 ; true ),
        ( var(PCInit) -> PCInit = calculated ; true ),
        ( var(PCUpdate) -> PCUpdate = average ; true ),
        ( var(PCRatio) -> PCRatio = 0.05 ; true ),
        ( var(LB) -> LB = 1 ; true ),
        ( var(OnOff) -> OnOff = off ; true ).

bfs_integral_objective(Expr1+Expr2, Pool) :-
        bfs_integral_objective(Expr1, Pool),
        bfs_integral_objective(Expr2, Pool).
bfs_integral_objective(C*Var, Pool) :-
        integer(C),
        bfs_var_get(Var, type, integer, Pool).
bfs_integral_objective(Var, Pool) :-
        bfs_var_get(Var, type, integer, Pool).

bfs_integral_objective([], _, _).
bfs_integral_objective([J:C|Rest], VarArr, Pool) :-
        integer(C),
        J1 is J + 1,
        arg(J1, VarArr, Var),
        bfs_var_get(Var, type, integer, Pool),
        bfs_integral_objective(Rest, VarArr, Pool).

:- demon bfs_body/4.
bfs_body(Handle, Solve, Separation, Pool) :-
        Handle = bfs_tree with [
                                module:Module,
                                nodes:Nodes0,
                                solve_time:TSolve,
                                nodes_solved:NSolve
                               ],
        ( Nodes0 = [Node|Nodes] ->
              Node = bfs_node with id:NodeId,
              bfs_info_message(Handle, "%w:Processing node %w%n%tsolving relaxation ... ",
                               [Pool, NodeId]),
              cputime(T1),
              % call node minimization goal
              setarg(frac_vars of bfs_node, Node, []),
              ( call(Solve)@Module ->
                    Node = bfs_node with objval:ObjVal,
                    bfs_info_message(Handle, " done, z = %w%n", [ObjVal])
              ;
                    setarg(objval of bfs_node, Node, 1.0Inf),
                    bfs_info_message(Handle, " infeasible%n", [])
              ),
              % do some statistics collection
              cputime(T2),
              TSolve1 is TSolve + T2 - T1,
              setarg(solve_time of bfs_tree, Handle, TSolve1),
              NSolve1 is NSolve + 1,
              setarg(nodes_solved of bfs_tree, Handle, NSolve1),
              bfs_info_message(Handle, "%tseparating ... ", []),
              cputime(T3),
              Handle = bfs_tree with [
                                         
                                      feas_check:FeasCheck,
                                         
                                      sense:Sense,
                                      vars:Vars,
                                      integral_obj:IntObj,
                                      cost:Cost,
                                      best_bound:BestBound,
                                      node_select:NodeSelect,
                                      next_node_no:Id1,
                                      separate_time:TSep,
                                      nodes_separated:NSep,
                                      start_time:TStart,
                                      no_sols:NSols
                                     ],
              Node = bfs_node with [
                                    id:NodeId,
                                    objval:NodeCost,
                                    rank:Rank,
                                    frac_vars:FracVars,
                                    state:State,
                                    branches:Branches
                                   ],
              ( NodeCost == 1.0Inf ->
                    % fathomed by infeasibility
                    bfs_info_message(Handle, "%tfathomed by infeasibility%n", []),
                    setarg(nodes of bfs_tree, Handle, Nodes)
              ; fathom_by_bounds(IntObj, NodeCost, BestBound) ->
                    % fathomed by bounds
                    bfs_info_message(Handle, "%tfathomed by bounds%n", []),
                    setarg(nodes of bfs_tree, Handle, Nodes)
              ; global_feasible(Node, FeasCheck, Module) ->
                    % globally feasible solution,
                    % optimal within tolerance bound
                    bfs_info_message(Handle, "%tglobally feasible%n", []),
                    % do some statistics collection
                    cputime(T),
                    TSolution is T - TStart,
                    setarg(opt_sol_time of bfs_tree, Handle, TSolution),
                    % update incumbent and prune nodes
                    set_var_bounds(Cost, -1.0Inf, NodeCost),
                    ( IntObj == yes ->
                          NewBound is fix(NodeCost),
                          PruneBound is NodeCost - 1
                    ;
                          NewBound = NodeCost,
                          PruneBound = NodeCost
                    ),
                    setarg(best_bound of bfs_tree, Handle, NewBound),
                    prune_nodes(PruneBound, NodeSelect, Nodes, NewNodes),
                    setarg(nodes of bfs_tree, Handle, NewNodes),
                    Handle = bfs_tree with vars:Vars,
                    (
                        foreach(Var, Vars),
                        param(NodeId, Pool)
                    do
                        ( var(Var) ->
                            bfs_get_node_info(Var, NodeId,
                                              bfs_node_info with val:Val, Pool),
                            bfs_var_set(Var, optimal_val, Val, Pool)
                        ;
                            true
                        )
                    ),
                    ( NSols = 0 ->
                          % first feasible solution
                          % if we are using a two-phase strategy
                          % we should reorder open nodes
                          ( NodeSelect == two_phase ->
                                ( Sense = min -> Order = (@<) ; Order = (@>) ),
                                node_reorder_dfs_bfs(NewNodes, NewNodes0,
                                                     PruneBound, Order),
                                setarg(nodes of bfs_tree, Handle, NewNodes0),
                                setarg(node_select of bfs_tree, Handle, best_first)
                          ; true ),
                          setarg(first_sol_time of bfs_tree, Handle, TSolution),
                          setarg(no_sols of bfs_tree, Handle, 1)
                    ;
                          % new optimal solution
                          NSols1 is NSols + 1,
                          setarg(no_sols of bfs_tree, Handle, NSols1)
                    )
              ;
                    (
                        foreach(Var, Vars),
                        foreach(Lo..Hi, LoHis),
                        param(NodeId, Pool)
                    do
                        bfs_get_node_info(Var, NodeId,
                                          bfs_node_info with [lo:Lo, hi:Hi],
                                          Pool)
                    ),
                    call(Separation)@Module,
                    collect_typed_pool_constraints(Pool,
                                                   branch of bfs_constraint_type,
                                                   ScoredBranches),
                    (
                        foreach([Score, Branch], ScoredBranches),
                        fromto(Nodes, In, Out, NewNodes),
                        foreach(Diff-NewNode, ChildRanks),
                        count(Id, Id1, Id2),
                        param(NodeSelect, Sense, NodeCost, State, Branches,
                              FracVars, Vars, LoHis, Pool)
                    do
                        NewNode = bfs_node with [
                                                 id:Id,
                                                 rank:ChildRank,
                                                 parent_obj:NodeCost,
                                                 state:State,
                                                 branches:ChildBranches
                                                ],
                        (
                            foreach(Var, Vars),
                            foreach(Lo..Hi, LoHis),
                            param(NewNode, Pool)
                        do
                            bfs_new_node_info(Var, NewNode,
                                              bfs_node_info with [lo:Lo, hi:Hi],
                                              Pool)
                        ),
                        ( number(Score) ->
                              Diff = Score
                        ;
                              Score = [PCVars, PCStruct, Diff, UpDown],
                              setarg(pc_update of bfs_node, NewNode,
                                     [PCStruct, Diff, UpDown])
                        ),
                        ( Branch = NewNode ->
                              true
                        ; Branch =.. [(::), Var, Lo..Hi] ->
                              % it was a var bound update
                              bfs_new_node_info(Var, NewNode,
                                                bfs_node_info with [lo:Lo, hi:Hi],
                                                Pool),
                              ChildBranches = Branches
                        ; Branches = [lp_add(EplexHandle, Cstrs, [])] ->
                              % it was a gub
                              normalise_cstrs([Branch], [Cstr], []),
                              ChildBranches = [lp_add(EplexHandle, [Cstr|Cstrs], [])]
                        ;
                              % it was some user defined branch
                              ChildBranches = [Branch|Branches]
                        ),
                        ( NodeSelect == best_first ->
                              % using a static node selection method
                              % so order among children by fractional change
                              % the rank given to a node here is the bound
                              % on the node objective obtained from the parent
                              % and the frac change Diff from parent to node,
                              % standard ordering ensures we choose among them
                              % in Bound then Frac order
                              ( Sense = min ->
                                    ChildRank = NodeCost-Diff,
                                    node_insert((@<), NewNode, In, Out)
                              ; % Sense = max
                                    Diff1 is 1 - Diff,
                                    ChildRank = NodeCost-Diff1,
                                    node_insert((@<), NewNode, In, Out)
                              )
                        ; NodeSelect == depth_first ->
                              true
                        ; NodeSelect == two_phase ->
                              true
                        ; NodeSelect == best_estimate ->
                              % using an estimate based selection method, so the order
                              % of the nodes is taken care of automatically
                              % Note to AE: this may not give the correct
                              % estimates with gub branching?
                              ( Sense = min ->
                                    Order = (@<),
                                    Functor = (+)
                              ; % Sense = max
                                    Order = (@>),
                                    Functor = (-)
                              ),
                              PCStruct = pseudocost with [pcd:PCD, pcu:PCU],
                              (
                                  foreach(Var:Val, FracVars),
                                  fromto(NodeCost, In, Out, CommonRank),
                                  param(Pool, PCVars, Functor)
                              do
                                  ( var_member(Var, PCVars) ->
                                        Out = In
                                  ;
                                        get_bfs_attr(Var, Pool,
                                                     bfs with [pcd:D, pcu: U]),
                                        Delta is min(D*(Val-floor(Val)),
                                                     U*(ceiling(Val)-Val)),
                                        Goal =.. [Functor, In, Delta, Out],
                                        call(Goal)
                                  )
                              ),
                              ( UpDown == d -> Delta is PCD * Diff
                              ; Delta is PCU * Diff ), % UpDown == u
                              Goal =.. [Functor, CommonRank, Delta, ChildRank],
                              call(Goal),
                              node_insert(Order, NewNode, In, Out)
                        ;
                              printf(error, "%w: unknown node_select type %w in"
                                     " separate_node/2%n",
                                     [Pool, NodeSelect]),
                              abort
                        )
                    ),
                    ( ( NodeSelect == depth_first ; NodeSelect == two_phase ) ->
                          sort(1, =<, ChildRanks, Sorted),
                          (
                              foreach(_-NewNode, Sorted),
                              fromto(Nodes, In, Out, NewNodes),
                              count(N, 0, _),
                              param(Pool, Rank)
                          do
                              ( N =< 255 ->
                                    char_code(C, N),
                                    concat_string([Rank, C], ChildRank),
                                    NewNode = bfs_node with rank:ChildRank,
                                    node_insert((@<), NewNode, In, Out)
                              ;
                                    printf(error, "%w: maximum number"
                                           " of children at a node"
                                           " exceeded (256)%n",
                                           [Pool]),
                                    abort
                              )
                          )
                    ; true ),
                    NextId is Id2 + 1,
                    cputime(T4),
                    TSep1 is TSep + T4 - T3,
                    setarg(separate_time of bfs_tree, Handle, TSep1),
                    NSep1 is NSep + 1,
                    setarg(nodes_separated of bfs_tree, Handle, NSep1),
                    setarg(next_node_no of bfs_tree, Handle, NextId),
                    setarg(nodes of bfs_tree, Handle, NewNodes),
                    bfs_info_message(Handle, "done, nodes %d..%d created%n",
                                     [Id1, Id2])
              ),
              schedule_suspensions(node_susp of bfs_tree, Handle),
              wake
        ;
              true
        ).

var_member(Var, [H|T]) :-
        ( Var == H -> true ; var_member(Var, T) ).

bfs_minimize_eplex_node(EplexHandle, Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                cost:Cost,
                                shelf:Shelf,
                                nodes:[Node|_],
                                pc_ratio:PCRatio,
                                no_sols:NSols,
                                pool:Pool
                               ],
        Node = bfs_node with [
                              id:NodeId,
                              state:State,
                              branches:Branches
                             ],
        ( NodeId = 0 ->
              % root node, no need to block/fail
              lp_solve(EplexHandle, Obj),
              lp_get(EplexHandle, vars, VarArr),
              VarArr =.. [_|Vars],
              ( 
                  foreach(Var, Vars),
                  param(EplexHandle, Pool)
              do
                  lp_var_get(EplexHandle, Var, solution, Val),
                  lp_var_get(EplexHandle, Var, reduced_cost, RC),
                  get_var_bounds(Var, Lo, Hi),
                  Info = bfs_node_info with [
                                             lo:Lo,
                                             hi:Hi,
                                             val:Val,
                                             rc:RC
                                            ],
                  Pool:node_info(Var, Info)
              ),
              lp_get(EplexHandle, simplex_iterations, Its),
              Node = bfs_node with frac_vars:FracVars,
              length(FracVars, NFrac),
              PCIt is fix(ceiling((PCRatio*Its)/(2*NFrac))),
              setarg(pc_ratio of bfs_tree, Handle, PCIt),
              lp_get(EplexHandle, basis, NewState),
              setarg(state of bfs_node, Node, NewState)
        ;
              \+ \+ ( lp_set(EplexHandle, basis, State),
                      lp_get(EplexHandle, vars, VarArr),
                      VarArr =.. [_|Vars],
                      call_priority(( 
                                        foreach(Var, Vars),
                                        param(Pool)
                                    do
                                        Pool:node_info(Var, Info),
                                        Info = bfs_node_info with [lo:Lo, hi:Hi],
                                        set_var_bounds(Var, Lo, Hi)
                                    ), 2),
                      ( foreach(Branch, Branches) do call(Branch) ),
                      lp_solve(EplexHandle, Obj),
                      ( NSols > 0 ->
                            reduced_cost_pruning(EplexHandle, Cost)
                      ;
                            true
                      ),
                      (
                          foreach(Var, Vars),
                          foreach(Info, Vals),
                          param(EplexHandle)
                      do
                          lp_var_get(EplexHandle, Var, solution, Val),
                          lp_var_get(EplexHandle, Var, reduced_cost, RC),
                          get_var_bounds(Var, Lo, Hi),
                          Info = bfs_node_info with [
                                                        lo:Lo,
                                                        hi:Hi,
                                                        val:Val,
                                                        rc:RC
                                                    ]
                      ),
                      lp_get(EplexHandle, basis, NewState),
                      shelf_set(Shelf, 1, [Obj, Vals, NewState])
              ),
              lp_get(EplexHandle, vars, VarArr),
              VarArr =.. [_|Vars],
              shelf_get(Shelf, 1, [Obj, Vals, NewState]),
              shelf_set(Shelf, 1, []),
              ( 
                  foreach(Var, Vars),
                  foreach(Info, Vals),
                  param(Pool)
              do
                  Pool:node_info(Var, Info)
              ),
              setarg(state of bfs_node, Node, NewState)
        ),
        setarg(objval of bfs_node, Node, Obj).

bfs_update_pseudocosts(Pool) :-
        get_pool_handle(Handle, Pool),
        ( Handle = bfs_tree with nodes:[bfs_node with id:0|_] ->
              true
        ;
              bfs_update_pc(Handle)
        ).

bfs_update_pc(Handle) :-
        Handle = bfs_tree with [
                                nodes:[Node|_],
                                pc_update:PCUpdate,
                                pcd_count:PCDCount-PCDTotal,
                                pcu_count:PCUCount-PCUTotal
                               ],
        Node = bfs_node with [
                              objval:Obj,
                              parent_obj:ObjP,
                              pc_update:[PCStruct, Frac, Direction]
                             ],
        PseudoCost is (Obj - ObjP)/Frac,
        % update pseudocosts
        PCStruct = pseudocost with [
                                    pcd:Pcd,
                                    pcd_count:PcdTerm,
                                    pcu:Pcu,
                                    pcu_count:PcuTerm
                                   ],
        ( Direction == u ->
              % update the global pcu counters
              % for pcu initialisation
              NewPCUCount is PCUCount + 1,
              NewPCUTotal is PCUTotal + PseudoCost,
              NewPCUAverage is NewPCUTotal/NewPCUCount,
              setarg(pcu_average of bfs_tree, Handle,
                     NewPCUAverage),
              setarg(pcu_count of bfs_tree, Handle,
                     NewPCUCount-NewPCUTotal),
              % update the pcu counters involved in the branch
              ( PcuTerm = 0-Pcu ->
                    NewPcuCount = 1,
                    NewPcuTotal = PseudoCost,
                    NewPcu = PseudoCost
              ;
                    PcuTerm = PcuCount-PcuTotal,
                    NewPcuCount is PcuCount + 1,
                    NewPcuTotal is PcuTotal + PseudoCost,
                    ( PCUpdate = first ->
                          NewPcu = Pcu
                    ; PCUpdate = last ->
                          NewPcu = PseudoCost
                    ; % PCUpdate = average
                          NewPcu is NewPcuTotal/NewPcuCount
                    )
              ),
              setarg(pcu_count of pseudocost, PCStruct,
                     NewPcuCount-NewPcuTotal),
              setarg(pcu of pseudocost, PCStruct, NewPcu)
        ; Direction == d ->
          % update the global pcd counters
          % for pcd initialisation
              NewPCDCount is PCDCount + 1,
              NewPCDTotal is PCDTotal + PseudoCost,
              NewPCDAverage is NewPCDTotal/NewPCDCount,
              setarg(pcd_average of bfs_tree, Handle,
                     NewPCDAverage),
              setarg(pcd_count of bfs_tree, Handle,
                     NewPCDCount-NewPCDTotal),
              % update the pcd counters involved in the branch
              ( PcdTerm = 0-Pcd ->
                    NewPcdCount = 1,
                    NewPcdTotal = PseudoCost,
                    NewPcd = PseudoCost
              ;
                    PcdTerm = PcdCount-PcdTotal,
                    NewPcdCount is PcdCount + 1,
                    NewPcdTotal is PcdTotal + PseudoCost,
                    ( PCUpdate = first ->
                          NewPcd = Pcd
                    ; PCUpdate = last ->
                          NewPcd = PseudoCost
                    ; % PCUpdate = average
                          NewPcd is NewPcdTotal/NewPcdCount
                    )
              ),
              setarg(pcd_count of pseudocost, PCStruct,
                     NewPcdCount-NewPcdTotal),
              setarg(pcd of pseudocost, PCStruct, NewPcd)
        ).

node_reorder_dfs_bfs(DepthFirstNodes, BestFirstNodes, Bound, Order) :-
        (
            foreach(Node, DepthFirstNodes),
            fromto([], In, Out, BestFirstNodes),
            param(Bound, Order)
        do
            Node = bfs_node with parent_obj:NodeCost,
            Call =.. [Order, NodeCost, Bound],
            ( call(Call) ->
                  setarg(rank of bfs_node, Node, NodeCost-0),
                  node_insert(Order, Node, In, Out)
            ;
                  Out = In
            )
        ).

node_insert(Order, Node, Nodes0, Nodes) :-
        ( Nodes0 == [] ->
              Nodes = [Node]
        ;
              Nodes0 = [Node0|Rest],
              Node = bfs_node with rank:Rank,
              Node0 = bfs_node with rank:Rank0,
              Call =.. [Order, Rank, Rank0],
              ( call(Call) ->
                    Nodes = [Node|Nodes0]
              ;
                    Nodes = [Node0|Nodes1],
                    node_insert(Order, Node, Rest, Nodes1)
              )
        ).

prune_nodes(Bound, best_first, Nodes0, Nodes) :- !,
        % for best_first the order in the list is just that of the
        % lowerbound so we can prune everything after the first with
        % parent_obj >= Bound
        prune_all_nodes(Bound, Nodes0, Nodes).
prune_nodes(Bound, _NodeSelect, Nodes0, Nodes) :-
        % otherwise the order in the list may differ from that of the
        % lowerbound so we must check each node
        prune_nodes(Bound, Nodes0, Nodes).
        
prune_all_nodes(Bound, Nodes0, Nodes) :-
        ( Nodes0 == [] ->
              Nodes = []
        ;
              Nodes0 = [Node|Rest],
              Node = bfs_node with parent_obj:NodeBound,
              ( NodeBound >= Bound ->
                    Nodes = []
              ;
                    Nodes = [Node|Nodes1],
                    prune_all_nodes(Bound, Rest, Nodes1)
              )
        ).

prune_nodes(Bound, Nodes0, Nodes) :-
        ( Nodes0 == [] ->
              Nodes = []
        ;
              Nodes0 = [Node|Rest],
              Node = bfs_node with parent_obj:NodeBound,
              ( NodeBound >= Bound ->
                    Nodes = Nodes1
              ;
                    Nodes = [Node|Nodes1]
              ),
              prune_nodes(Bound, Rest, Nodes1)
        ).

fathom_by_bounds(IntObj, Obj, Bound) :-
        ( IntObj == yes ->
              ceiling(Obj) > Bound
        ;
              Obj > Bound
        ).

global_feasible(bfs_node with frac_vars:[], FeasCheck, Module) :-
        call(FeasCheck)@Module.

depth_first(Ranks, Base) :-
        sort(1, >=, Ranks, Sorted),
        (
            foreach(_-Rank, Sorted),
            count(N, 1, _),
            param(Base)
        do
            concat_string([Base, N], Rank)
        ).

gub_structure([], [], _Pool).
gub_structure([Val*Var|Rest], [Var|Terms], Pool) :-
        Val =:= 1,
        bfs_var_get(Var, type, integer, Pool),
        gub_structure(Rest, Terms, Pool).

gub_structure([], [], Val, IntTol, _Pool) :-
        abs(Val + 1) =< IntTol.
gub_structure([Val*Var|Rest], Terms, Sum, IntTol, Pool) :-
        ( (Val =:= 1, bfs_var_get(Var, type, integer, Pool)) ->
              Terms = [Var|Terms0],
              Sum0 = Sum
        ;
              Terms = Terms0,
              get_var_bounds(Var, Lo, Hi),
              Sum0 is Sum + min(Val*Lo, Val*Hi)
        ),
        gub_structure(Rest, Terms0, Sum0, IntTol, Pool).

reference_row([_Type:[_Const*_One|Terms]|Cstrs], GUBSet,
              Vars, RefTerms, PCStructs) :-
        ( reference_row1(GUBSet, Terms, RefTerms0) ->
              % Note to AE: should we commit to just the first
              % possible reference 
              % row we find or collect them all as generators for
              % branching constraints? Probably that would then be too
              % expensive to check them all each time, but it might
              % create better branches
              sort(RefTerms0, RefTerms),
              (
                  foreach(_Coeff*Var, RefTerms),
                  foreach(1*Var, Vars),
                  foreach(pseudocost with [pcd_count:none,
                                           pcu_count:none], PCList)
              do
                  true
              ),
              PCStructs =.. [[]|PCList]
        ;
              reference_row(Cstrs, GUBSet, Vars, RefTerms, PCStructs)
        ).

reference_row1([], _Terms, []).
reference_row1([Var|GUBSet], Terms, [RefTerm|RefTerms]) :-
        reference_row(Var, Terms, RefTerm, Terms0),
        reference_row1(GUBSet, Terms0, RefTerms).

reference_row(Var, [Val0*Var0|Terms], RefTerm, Terms0) :-
        ( Var == Var0 ->
              RefTerm= Val0*Var0,
              Terms0 = Terms
        ;
              Terms0 = [Val0*Var0|Terms1],
              reference_row(Var, Terms, RefTerm, Terms1)
        ).

branchpoint([Coeff:VarCount:VarSum|BranchPoints], RefTotal, NVars, Diff) :-
        ( Coeff > RefTotal ->
              % this branch set includes vars with coeff above the
              % suggested ref val
              branchpoint(BranchPoints, RefTotal, NVars, Diff)
        ;
              % maximal branch set with no coeffs above the suggested
              % ref val
              NVars = VarCount,
              Diff = VarSum
        ).

% Note to AE: try to get rid of the eplex handle form this call
bfs_deg_est(eplex(EplexHandle), Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                nodes:[Node|_],
                                gubs:GUBs
                               ],
        Node = bfs_node with id:NodeId,
        % update pseudocosts for this node
        ( NodeId > 0 ->
              bfs_update_pc(Handle)
        ;
              true
        ),
        % find the var to branch on
        bfs_deg_est(Handle, eplex(EplexHandle), Pool, Result, PseudoCosts),
        ( Result = g(GN, NVars, Down) ->
              % initialize any GUB pseudocosts
              (
                  foreacharg(bfs_gub with pseudocosts:PCStructs, GUBs),
                  foreach(PseudoCost, PseudoCosts)
              do
                  ( PseudoCost == not_stored ->
                        true
                  ;
                        PseudoCost = BP-[PCD, PCU],
                        arg(BP, PCStructs, PCStruct),
                        setarg(pcd_count of pseudocost, PCStruct, 0-PCD),
                        setarg(pcd of pseudocost, PCStruct, PCD),
                        setarg(pcu_count of pseudocost, PCStruct, 0-PCU),
                        setarg(pcu of pseudocost, PCStruct, PCU)
                  )
              ),
              arg(GN, GUBs, GUB),
              GUB = bfs_gub with [
                                  vars:Vars,
                                  pseudocosts:PCStructs
                                 ],
              Up is 1 - Down,
              arg(NVars, PCStructs, PCStruct),
              (
                  for(_, 1, NVars),
                  foreach(Term, Terms),
                  fromto(Vars, [Term|Rest], Rest, _)
              do
                  true
              ),
              term_variables(Terms, TermVars),
              bfs_branch([[TermVars, PCStruct, Down, d], sum(Terms) =:= 0], Pool),
              bfs_branch([[TermVars, PCStruct, Up, u], sum(Terms) =:= 1], Pool)
        ;
              Result = v(Idx),
              Node = bfs_node with frac_vars:Vars,
              % initialize any variable pseudocosts
              (
                  foreach(Var:_, Vars),
                  foreach(PseudoCost, PseudoCosts),
                  param(Pool)
              do
                  ( PseudoCost == not_stored ->
                        true
                  ;
                        PseudoCost = [PCD, PCU],
                        get_bfs_attr(Var, Pool, Attr),
                        setarg(pcd_count of bfs, Attr, 0-PCD),
                        setarg(pcd of bfs, Attr, PCD),
                        setarg(pcu_count of bfs, Attr, 0-PCU),
                        setarg(pcu of bfs, Attr, PCU)
                  )
              ),
              VarArr =.. [[]|Vars],
              arg(Idx, VarArr, Var:Val),
              Lo is ceiling(Val),
              Up is Lo - Val,
              Hi is floor(Val),
              Down is Val - Hi,
              get_bfs_attr(Var, Pool, bfs with pseudocost:PCStruct),
              bfs_branch([[[Var], PCStruct, Down, d], Var :: -1.0Inf..Hi], Pool),
              bfs_branch([[[Var], PCStruct, Up, u], Var :: Lo..1.0Inf], Pool)
        ).
        
bfs_deg_est(Handle, NodeType, Pool, _, _) :-
        % branching on best estimate
        bfs_impose_node_state(NodeType, Pool),
        Handle = bfs_tree with [
                                int_tolerance:IntTol,
                                nodes:[Node|_Nodes],
                                gubs:GUBs,
                                shelf:Shelf,
                                alpha_min:AlphaMin,
                                alpha_max:AlphaMax,
                                beta_const:BetaConst,
                                beta_pc:BetaPC,
                                beta_lb:BetaLB,
                                pc_init:PCInit,
                                pc_ratio:PCIt,
                                lb_time:LBIt
                               ],
        Node = bfs_node with [
                              id:NodeId,
                              objval:NodeCost,
                              frac_vars:Vars
                             ],
        ( ( PCInit == calculated, PCIt >= LBIt ) ->
              PCOverLB = true
        ;
              PCOverLB = fail
        ),
        (
            foreacharg(GUB, GUBs),
            foreach(PseudoCost, GPseudoCosts),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(_, ScoreIn, ScoreOut, _),
            param(Handle, NodeType, Pool, IntTol, NodeId, NodeCost,
                  AlphaMin, AlphaMax,
                  BetaConst, BetaPC, BetaLB, PCOverLB)
        do
            GUB = bfs_gub with [
                                vars:Vars,
                                refs:Refs,
                                pseudocosts:PCStructs
                               ],
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Pool, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info with val:Val, Pool),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  PseudoCost = not_stored,
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, and calculate score
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  ( BetaPC =:= 0 ->
                        % only lower bounds in estimate
                        PCD = 0,
                        PCU = 0,
                        PseudoCost = not_stored
                  ;
                        % otherwise get pseudocosts
                        get_gub_pseudocosts(Handle, NodeType, Pool,
                                            NodeCost, GUB, NVars, Sum,
                                            PCD, PCU, Initializing),
                        ( call(Initializing) ->
                              PseudoCost = NVars-[PCD, PCU]
                        ;
                              PseudoCost = not_stored
                        )
                  ),
                  ( BetaLB =:= 0 ->
                        % only pseudocosts in estimate
                        LD = 0,
                        LU = 0
                  ;
                        ( call(PCOverLB),
                          arg(NVars, PCStructs, 
                              pseudocost with [pcd_count:0-LD,
                                               pcu_count:0-LU]) ) ->
                            % if pseudocosts have just been calculated and are
                            % stronger, use them as lower bounds
                            true
                  ;
                            % otherwise get lower bounds
                            (
                                for(_, 1, NVars),
                                foreach(Term, Terms),
                                fromto(Vars, [Term|Rest], Rest, _)
                            do
                                true
                            ),
                            get_gub_lowerbounds(Handle, NodeType, Pool,
                                                NodeCost, Terms,
                                                LD, LU)
                  ),
                  DownScore is BetaConst + (BetaPC * PCD * Sum) +
                               (BetaLB * LD),
                  UpScore is BetaConst + (BetaPC * PCU * (1-Sum)) +
                             (BetaLB * LU),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = g(GN, NVars, Sum)
                  )
            )
            
        ),
        ( nonvar(Result) ->
              PseudoCosts = GPseudoCosts
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var using deg_est
              (
                  foreach(Var:Val, Vars),
                  count(I, 1, _),
                  fromto(_, ScoreIn, ScoreOut, _),
                  fromto(_, In, Out, Result),
                  foreach(PseudoCost, PseudoCosts),
                  param(Handle, NodeType, Pool, NodeCost,
                        AlphaMin, AlphaMax,
                        BetaConst, BetaPC, BetaLB, PCOverLB)
              do
                  Up is ceiling(Val) - Val,
                  Down is Val - floor(Val),
                  ( BetaPC =:= 0 ->
                        % only lower bounds in estimate
                        PCD = 0,
                        PCU = 0,
                        PseudoCost = not_stored
                  ;
                        % otherwise get pseudocosts
                        get_pseudocosts(Handle, NodeType, Pool,
                                        NodeCost, Var, Val,
                                        PCD, PCU, Initializing),
                        ( call(Initializing) ->
                              PseudoCost = [PCD, PCU]
                        ;
                              PseudoCost = not_stored
                        )
                  ),
                  ( BetaLB =:= 0 ->
                        % only pseudocosts in estimate
                        LD = 0,
                        LU = 0
                  ;
                        ( call(PCOverLB),
                          get_bfs_attr(Var, Pool,
                                       bfs with [pcd_count:0-LD,
                                                 pcu_count:0-LU]) ) ->
                            % if pseudocosts have just been calculated and are
                            % stronger, use them as lower bounds
                            true
                  ;
                            % otherwise get lower bounds
                            get_lowerbounds(Handle, NodeType, Pool,
                                            NodeCost, Var, Val,
                                            LD, LU)
                  ),
                  DownScore is BetaConst + (BetaPC * PCD * Down) +
                               (BetaLB * LD),
                  UpScore is BetaConst + (BetaPC * PCU * Up) +
                             (BetaLB * LU),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = v(I)
                  )
              )
        ),
        shelf_set(Shelf, 1, [Result, PseudoCosts]),
        fail.
bfs_deg_est(bfs_tree with shelf:Shelf, _, _, Result, PseudoCosts) :-
        shelf_get(Shelf, 1, [Result, PseudoCosts]),
        shelf_set(Shelf, 1, []).

bfs_fracvar(Pool) :-
        % branching on gub if available or most fractional var
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                int_tolerance:IntTol,
                                nodes:[Node|_],
                                gubs:GUBs
                               ],
        Node = bfs_node with [
                              id:NodeId,
                              frac_vars:Vars
                             ],
        (
            foreacharg(GUB, GUBs),
            fromto(_, In, Out, Result),
            fromto(IntTol, ScoreIn, ScoreOut, _),
            param(Pool, IntTol, NodeId)
        do
            GUB = bfs_gub with [
                                vars:Vars,
                                refs:Refs
                               ],
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Pool, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info with val:Val, Pool),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  Score is min(Sum, (1 - Sum)),
                  ( Score =< ScoreIn ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        (
                            for(_, 1, NVars),
                            foreach(Term, Terms),
                            fromto(Vars, [Term|Rest], Rest, _)
                        do
                            true
                        ),
                        Out = Terms:Sum
                  )
            )
        ),
        ( nonvar(Result) ->
              Result = Terms:Down,
              Up is 1 - Down,
              bfs_branch([Down, sum(Terms) =:= 0], Pool),
              bfs_branch([Up, sum(Terms) =:= 1], Pool)
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var
              (
                  foreach(Var:Val, Vars),
                  fromto(IntTol, DiffIn, DiffOut, _),
                  fromto(_, In, Out, Val-Var)
              do
                  Diff is abs(round(Val) - Val),
                  ( Diff > DiffIn ->
                        DiffOut = Diff,
                        Out = Val-Var
                  ;
                        DiffOut = DiffIn,
                        Out = In
                  )
              ),
              Lo is ceiling(Val),
              Up is Lo - Val,
              Hi is floor(Val),
              Down is Val - Hi,
              bfs_branch([Down, Var :: -1.0Inf..Hi], Pool),
              bfs_branch([Up, Var :: Lo..1.0Inf], Pool)
        ).

bfs_enhanced(eplex(EplexHandle), Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                int_tolerance:IntTol,
                                nodes:[Node|_],
                                gubs:GUBs
                               ],
        Node = bfs_node with [
                              id:NodeId,
                              frac_vars:Vars
                             ],
        (
            foreacharg(GUB, GUBs),
            fromto(_, In, Out, Result),
            fromto(IntTol, ScoreIn, ScoreOut, _),
            param(Pool, IntTol, NodeId)
        do
            GUB = bfs_gub with [
                                vars:Vars,
                                refs:Refs
                               ],
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Pool, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info with val:Val, Pool),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  Score is min(Sum, (1 - Sum)),
                  ( Score =< ScoreIn ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        (
                            for(_, 1, NVars),
                            foreach(Term, Terms),
                            fromto(Vars, [Term|Rest], Rest, _)
                        do
                            true
                        ),
                        Out = Terms:Sum
                  )
            )
        ),
        ( nonvar(Result) ->
              Result = Terms:Down,
              Up is 1 - Down,
              bfs_branch([Down, sum(Terms) =:= 0], Pool),
              bfs_branch([Up, sum(Terms) =:= 1], Pool)
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var
              (
                  foreach(Var:Val, Vars),
                  foreach(Frac-Val-Var, FracVars),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U)
              do
                  Frac is Val - floor(Val),
                  ( Frac < 0.5 ->
                        LOut is max(Frac, LIn),
                        UOut = UIn
                  ; Frac > 0.5 ->
                        LOut = LIn,
                        UOut is min(Frac, UIn)
                  ;
                        LOut = 0.5,
                        UOut = 0.5
                  )
              ),
              LBound is 0.8 * L,
              UBound is 0.2 + 0.8 * U,
              (
                  foreach(FracPart-Val-Var, FracVars),
                  fromto(-1.0Inf, ScoreIn, ScoreOut, _),
                  fromto(_, In, Out, Val-Var),
                  param(EplexHandle, LBound, UBound)
              do
                  ( (FracPart >= LBound, FracPart =< UBound) ->
                        lp_get(EplexHandle, objective, Objective),
                        Objective =.. [_MinMax, Expr],
                        ( bfs_objective_coeff(Expr, Var, Score) ->
                        %EplexHandle = prob with objcoeffs:ObjCoeffs,
                        %lp_var_occurrence(Var, EplexHandle, J),
                        %( member(J:Score, ObjCoeffs) ->
                              true
                        ;
                              Score = 0
                        ),
                        ( Score =< ScoreIn ->
                              ScoreOut = ScoreIn,
                              Out = In
                        ;
                              ScoreOut = Score,
                              Out = Val-Var
                        )
                  ;
                        ScoreOut = ScoreIn,
                        Out = In
                  )
              ),
              Lo is ceiling(Val),
              Up is Lo - Val,
              Hi is floor(Val),
              Down is Val - Hi,
              bfs_branch([Down, Var :: -1.0Inf..Hi], Pool),
              bfs_branch([Up, Var :: Lo..1.0Inf], Pool)
        ).

% Note to AE: try to get rid of the eplex handle form this call
bfs_strong(eplex(EplexHandle), Pool) :-
        get_pool_handle(Handle, Pool),
        bfs_strong(Handle, eplex(EplexHandle), Pool, Result),
        ( Result = g(GN, NVars, Down) ->
              Handle = bfs_tree with gubs:GUBs,
              arg(GN, GUBs, GUB),
              GUB = bfs_gub with vars:Vars,
              Up is 1 - Down,
              (
                  for(_, 1, NVars),
                  foreach(Term, Terms),
                  fromto(Vars, [Term|Rest], Rest, _)
              do
                  true
              ),
              bfs_branch([Down, sum(Terms) =:= 0], Pool),
              bfs_branch([Up, sum(Terms) =:= 1], Pool)
        ;
              Result = v(Idx),
              Handle = bfs_tree with nodes:[bfs_node with frac_vars:Vars|_],
              VarArr =.. [[]|Vars],
              arg(Idx, VarArr, Var:Val),
              Lo is ceiling(Val),
              Up is Lo - Val,
              Hi is floor(Val),
              Down is Val - Hi,
              bfs_branch([Down, Var :: -1.0Inf..Hi], Pool),
              bfs_branch([Up, Var :: Lo..1.0Inf], Pool)
        ).

bfs_strong(Handle, NodeType, Pool, _) :-
        bfs_impose_node_state(NodeType, Pool),
        Handle = bfs_tree with [
                                int_tolerance:IntTol,
                                nodes:[Node|_Nodes],
                                gubs:GUBs,
                                shelf:Shelf,
                                alpha_min:AlphaMin,
                                alpha_max:AlphaMax
                               ],
        Node = bfs_node with [
                              id:NodeId,
                              objval:NodeCost,
                              frac_vars:Vars
                             ],
        (
            foreacharg(GUB, GUBs),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(_, ScoreIn, ScoreOut, _),
            param(Handle, NodeType, Pool, IntTol, NodeId, NodeCost,
                  AlphaMin, AlphaMax)
        do
            GUB = bfs_gub with [
                                vars:Vars,
                                refs:Refs
                               ],
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Pool, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info with val:Val, Pool),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  (
                      for(_, 1, NVars),
                      foreach(Term, Terms),
                      fromto(Vars, [Term|Rest], Rest, _)
                  do
                      true
                  ),
                  get_gub_lowerbounds(Handle, NodeType, Pool,
                                      NodeCost, Terms,
                                      DownScore, UpScore),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = g(GN, NVars, Sum)
                  )
            )
            
        ),
        ( nonvar(Result) ->
              true
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var
              (
                  foreach(Var:Val, Vars),
                  count(I, 1, _),
                  foreach(Frac-I-Val-Var, FracVars),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U)
              do
                  Frac is Val - floor(Val),
                  ( Frac < 0.5 ->
                        LOut is max(Frac, LIn),
                        UOut = UIn
                  ; Frac > 0.5 ->
                        LOut = LIn,
                        UOut is min(Frac, UIn)
                  ;
                        LOut = 0.5,
                        UOut = 0.5
                  )
              ),
              LBound is 0.8 * L,
              UBound is 0.2 + 0.8 * U,
              (
                  foreach(FracPart-Idx-Val-Var, FracVars),
                  fromto(_, ScoreIn, ScoreOut, _),            
                  fromto(_, In, Out, Result),
                  param(Handle, NodeType, Pool, NodeCost,
                        AlphaMin, AlphaMax, LBound, UBound)
              do
                  ( (FracPart >= LBound, FracPart =< UBound) ->
                        get_lowerbounds(Handle, NodeType, Pool,
                                        NodeCost, Var, Val,
                                        DownScore, UpScore),
                        Score is AlphaMin * min(DownScore, UpScore) +
                                 AlphaMax * max(DownScore, UpScore),
                        ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                              ScoreOut = ScoreIn,
                              Out = In
                        ;
                              ScoreOut = Score,
                              Out = v(Idx)
                        )
                  ;
                        ScoreOut = ScoreIn,
                        Out = In
                  )
              )
        ),
        shelf_set(Shelf, 1, Result),
        fail.
bfs_strong(bfs_tree with shelf:Shelf, _, _, Result) :-
        shelf_get(Shelf, 1, Result),
        shelf_set(Shelf, 1, []).

bfs_impose_node_state(NodeType, Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with [
                                vars:Vars,
                                nodes:[Node|_Nodes]
                              ],
        Node = bfs_node with [
                              id:NodeId,
                              state:State,
                              branches:Branches
                             ],
        % apply branching decisions & propagate
        % call them all as atomic op to limit wakings
        % since we will be doing this often
        call_priority((
                          foreach(Var, Vars),
                          param(NodeId, Pool)
                      do
                          bfs_get_node_info(Var, NodeId,
                                            bfs_node_info with [lo:Lo, hi:Hi],
                                            Pool),
                          set_var_bounds(Var, Lo, Hi)
                      ), 2),
        call_priority((
                          foreach(Branch, Branches)
                      do
                          call(Branch)
                      ), 2),
        ( NodeType = eplex(EplexHandle) ->
              lp_set(EplexHandle, basis, State)
        ;
              true
        ).

get_gub_pseudocosts(Handle, NodeType, Pool, NodeCost, GUB, BranchPoint, Sum,
                    PseudoCostDown, PseudoCostUp, Initializing) :-
        Handle = bfs_tree with [
                                pcd_average:PcDown,
                                pcu_average:PcUp,
                                pc_init:PcInit,
                                pc_ratio:Limit
                               ],
        NodeType = eplex(EplexHandle),
        GUB = bfs_gub with [
                            vars:Vars,
                            pseudocosts:PCStructs
                           ],
        arg(BranchPoint, PCStructs, PseudoCosts),
        PseudoCosts = pseudocost with [
                                       pcd:PCD,
                                       pcd_count:PCD_Count,
                                       pcu:PCU
                                      ],
        ( PCD_Count = none ->
              ( PcInit == calculated ->
                    % explicitly calculating initial pseudocosts:
                    estimate_degradation(Handle, NodeType, Pool,
                                         lp_add(EplexHandle,
                                                [(=:=):[0*1|Vars]], []),
                                         Limit, ObjValDown),
                    PseudoCostDown is abs((ObjValDown - NodeCost)/Sum),
                    estimate_degradation(Handle, NodeType, Pool,
                                         lp_add(EplexHandle,
                                                [(=:=):[-1*1|Vars]], []),
                                         Limit, ObjValUp),
                    PseudoCostUp is abs((ObjValUp - NodeCost)/(1 - Sum))
              ;
                PcInit == average ->
                    % initializing to current average:
                    PseudoCostDown = PcDown,
                    PseudoCostUp = PcUp
              ;
                    printf(error, "%w: unknown GUB pseudocost"
                           " initialization method %w%n",
                           [Pool, PcInit]),
                    flush(ouput),
                    abort
              ),
              Initializing = true,
              setarg(pcd_count of pseudocost, PseudoCosts, 0-PseudoCostDown),
              setarg(pcd of pseudocost, PseudoCosts, PseudoCostDown),
              setarg(pcu_count of pseudocost, PseudoCosts, 0-PseudoCostUp),
              setarg(pcu of pseudocost, PseudoCosts, PseudoCostUp)
        ;
              Initializing = fail,
              PseudoCostDown = PCD,
              PseudoCostUp = PCU
        ).

get_pseudocosts(Handle, NodeType, Pool, NodeCost, Var, Val,
		PseudoCostDown, PseudoCostUp, Initializing) :-
        Handle = bfs_tree with [
                                pcd_average:PcDown,
                                pcu_average:PcUp,
                                pc_init:PcInit,
                                pc_ratio:Limit
                               ],
        get_bfs_attr(Var, Pool, Attr),
        Attr = bfs with [
                         pcd:PCD,
                         pcd_count:PCD_Count,
                         pcu:PCU
                        ],
        ( PCD_Count = none ->
              ( PcInit == calculated ->
                    % explicitly calculating initial pseudocosts:
                    Hi is floor(Val),
                    estimate_degradation(Handle, NodeType, Pool,
                                         set_var_bounds(Var, -1.0Inf, Hi),
                                         Limit, ObjValDown),
                    PseudoCostDown is abs((ObjValDown - NodeCost)/(Val - Hi)),
                    Lo is ceiling(Val),
                    estimate_degradation(Handle, NodeType, Pool,
                                         set_var_bounds(Var, Lo, 1.0Inf),
                                         Limit, ObjValUp),
                    PseudoCostUp is abs((ObjValUp - NodeCost)/(Lo - Val))
              ;
                PcInit == cost ->
                    % initializing to cost coefficient:
                    ( NodeType = eplex(EplexHandle) ->
                         lp_get(EplexHandle, objective, Objective),
                         Objective =.. [_MinMax, Expr],
                         ( bfs_objective_coeff(Expr, Var, Cost) ->
                         %EplexHandle = prob with objcoeffs:ObjCoeffs,
                         %lp_var_occurrence(Var, EplexHandle, J),
                         %( member(J:Cost, ObjCoeffs) ->
                               true
                         ;
                               Cost = 0
                         )
                    ;
                         printf(error, "%d: Don't know how to get the cost"
                                " coefficient for %d in get_pseudocosts/8%n",
                                [Pool, Var]),
                         abort
                    ),
                    % Note to AE:
                    % so we have to be able to GET the cost
                    % coefficient from somewhere
                    PseudoCostDown = Cost,
                    PseudoCostUp = Cost
              ;
                PcInit == average ->
                    % initializing to current average:
                    PseudoCostDown = PcDown,
                    PseudoCostUp = PcUp
              ;
                    printf(error, "%w: unknown pseudocost"
                           " initialization method %w%n",
                           [Pool, PcInit]),
                    flush(ouput),
                    abort
              ),
              
              Initializing = true,
              
              setarg(pcd_count of bfs, Attr, 0-PseudoCostDown),
              setarg(pcd of bfs, Attr, PseudoCostDown),
              setarg(pcu_count of bfs, Attr, 0-PseudoCostUp),
              setarg(pcu of bfs, Attr, PseudoCostUp)
        ;
              
              Initializing = fail,
              
              PseudoCostDown = PCD,
              PseudoCostUp = PCU
        ).

bfs_objective_coeff(Expr1+Expr2, X, C) :-
        ( bfs_objective_coeff(Expr1, X, C) -> true
        ; bfs_objective_coeff(Expr2, X, C) -> true
        ; fail ).
%bfs_objective_coeff(Expr1+_Expr2, X, C) :-
%        bfs_objective_coeff(Expr1, X, C).
%bfs_objective_coeff(_Expr1+Expr2, X, C) :-
%        bfs_objective_coeff(Expr2, X, C).
bfs_objective_coeff(C*Var, X, C) :-
        Var == X, !.
bfs_objective_coeff(Var, X, 1) :-
        Var == X, !.

get_gub_lowerbounds(Handle, eplex(EplexHandle), Pool, NodeCost, Terms, LD, LU) :-
        Handle = bfs_tree with lb_time:Limit,
        length(Terms, NVars),
        GLimit is NVars * Limit,
        normalise_cstrs([sum(Terms) =:= 0, sum(Terms) =:= 1],
                        [Cstr1, Cstr2], []),
        estimate_degradation(Handle, NodeType, Pool,
                             lp_add(EplexHandle, Cstr1, []),
                             GLimit, ObjValDown),
        LD is abs(ObjValDown - NodeCost),
        estimate_degradation(Handle, NodeType, Pool,
                             lp_add(EplexHandle, Cstr2, []),
                             GLimit, ObjValUp),
        LU is abs(ObjValUp - NodeCost).

get_lowerbounds(Handle, NodeType, Pool, NodeCost, Var, Val, LD, LU) :-
        Handle = bfs_tree with lb_time:Limit,
        Hi is floor(Val),
        estimate_degradation(Handle, NodeType, Pool,
                             set_var_bounds(Var, -1.0Inf, Hi),
                             Limit, ObjValDown),
        LD is abs(ObjValDown - NodeCost),
        Lo is ceiling(Val),
        estimate_degradation(Handle, NodeType, Pool,
                             set_var_bounds(Var, Lo, 1.0Inf),
                             Limit, ObjValUp),
        LU is abs(ObjValUp - NodeCost).

estimate_degradation(bfs_tree with shelf:Shelf, eplex(EplexHandle),
                     _Pool, Bound, Limit, _DegVal) :-
        shelf_set(Shelf, 1, 1.0Inf),
        call(Bound),
        lp_get(optimizer, Optimizer),
        ( Optimizer == cplex ->
              lp_get(optimizer_param(iteration_limit), ItLim),
              lp_set(optimizer_param(iteration_limit), Limit)
        ;
              lp_get(EplexHandle, optimizer_param(iteration_limit), ItLim),
              lp_set(EplexHandle, optimizer_param(iteration_limit), Limit)
        ),
        lp_get(EplexHandle, method, Method),
        lp_set(EplexHandle, method, dual),
        set_event_handler(eplex_suboptimal, true/0),
        set_event_handler(eplex_abort, exit_abort/0),
        block( ( lp_solve(EplexHandle, Obj) -> true ; Obj = 1.0Inf ),
               abort,
               % IF there was an abort (probably hit iteration limit
               % before finding a feasible basis) AND presolve was
               % turned on AND the external solver is XPRESS-MP, THEN
               % the problem is not postsolved, and row/column numbers
               % can be completely wrong resulting in solution
               % information being returned in the incorrect order and
               % possibly variable bounds getting out of sync from now
               % on. Currently we avoid this by just disallowing the
               % use of a presolved XPRESS problem with estimate
               % degradation but we could alternatively check here and
               % reload the problem and bounds if necessary
               Obj = 1.0Inf
             ),
        set_event_handler(eplex_suboptimal, eplex_result_handler/2),
        set_event_handler(eplex_abort, eplex_result_handler/2),
        shelf_set(Shelf, 1, Obj),
        ( Optimizer == cplex ->
              lp_set(optimizer_param(iteration_limit), ItLim)
        ;
              lp_set(EplexHandle, optimizer_param(iteration_limit), ItLim)
        ),
        lp_set(EplexHandle, method, Method),
        fail.
estimate_degradation(bfs_tree with shelf:Shelf, _, _, _, _, DegVal) :-
        shelf_get(Shelf, 1, DegVal),
        shelf_set(Shelf, 1, []).

exit_abort :- exit_block(abort).

%-----------------------------------------------------------------------
% Pools
%-----------------------------------------------------------------------

:- local record(bfs_pools). % list of bfs pool names

create_bfs_pool(Pool) :-
	create_constraint_pool(Pool, property(arity) of bfs_constraint_type,
                               [
                                 deg_est/1 -> bfs_deg_est/2,
                                 strong/1 -> bfs_strong/2,
                                 enhanced/1 -> bfs_enhanced/2,
                                 fracvar/0 -> bfs_fracvar/1,
                                 node_cost/1 -> bfs_node_cost/2,
                                 impose_node_state/1 -> bfs_impose_node_state/2,
                                 update_pseudocosts/0 -> bfs_update_pseudocosts/1,
                                 minimize_eplex_node/1 -> bfs_minimize_eplex_node/2,
                                 var_get/3 -> bfs_var_get/4,
                                 integers/1 -> bfs_integers/2,
                                 get/2 -> bfs_get/3,
                                 set/2 -> bfs_set/3,
                                 node_info/2 -> bfs_node_info/3,
                                 node_info/5 -> bfs_node_info/6,
                                 statistics/0 -> bfs_statistics/1,
                                 solve/1 -> bfs_solve/2,
                                 solver_setup/2 -> bfs_solver_setup/3,
                                 solver_setup/3 -> bfs_solver_setup/4,
                                 solver_cleanup/0 -> bfs_solver_cleanup/1,
                                 bfs_branch/1 -> bfs_branch/2
                               ]).


bfs_instance(PoolName) :-
        ( lp_get(optimizer, xpress), lp_get(optimizer_version, 13) ->
              % warn the user that there are bugs when using
              % XPRESS-MP 13.26 as external solver
              % I am unsure whether this is related to the bugs
              % already reported to Dash, or something new
              printf(warning_output, "Warning: XPRESS-MP 13.26 is known to give"
                     " incorrect (suboptimal) solutions on some test"
                     " instances%n", [])
        ; true ),
	( is_constraint_pool(PoolName),
	  recorded(bfs_pools, PoolName) % is a bfs pool
	->
            % if pool exists, check if it is currently empty 
	    ( pool_is_empty(PoolName),
	      get_pool_item(PoolName, 0) % has no associated solver
	    ->
		true
	    ;
                printf(error, "Bfs instance still in use in %w%n", [bfs_instance(PoolName)]),
		abort
	    )
	;
%	    ( current_module(PoolName) ->
%		  error(6, bfs_instance(PoolName))
%	    ;
		  record(bfs_pools, PoolName),
		  create_bfs_pool(PoolName)
%	    )
	),
	set_pool_item(PoolName, Handle),
	Handle = bfs_tree with [pool:PoolName].

bfs_pool_associate_solver(PoolName, Handle) :-
	get_pool_item(PoolName, 0), % does not already have a solver
	set_pool_item(PoolName, Handle),
	Handle = bfs_tree with [pool:PoolName].

get_pool_handle(Handle, Pool) :-
	get_pool_item(Pool, Handle),
	Handle = bfs_tree with [].

% ----------------------------------------------------------------------

:- comment(summary, "Best-first search library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Imperial College London and Parc Technologies").
:- comment(date, "$Date: 2004/12/15 16:39:04 $").
:- comment(status, prototype).

:- comment(include, bfs_comments).
