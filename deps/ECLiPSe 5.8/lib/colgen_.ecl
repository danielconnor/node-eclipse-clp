:- lib(constraint_pools).
:- lib(linearize).
:- lib(hash).
:- lib(eplex).
:- lib(dual_var).
:- lib(bfs).

:- import
     lp_var_non_monotonic_set_bounds/4
   from eplex_s.

% ----------------------------------------------------------------------
%
% Meta-attribute related stuff
%
% ----------------------------------------------------------------------

:- export cg_var_print/2.

:- meta_attribute(colgen, [print:cg_var_print/2, unify:unify_colgen/2,
                           get_bounds:cg_get_bounds/3,
                           set_bounds:cg_set_bounds/3]).

:- export struct(
                 colgen(
                        mp_val,
                        cost,
                        coeffs,
                        aux,
                        lo,
                        hi,
                        type,
                        master_prob,                        
                        solver,
                        next
                       )
                ).

% ----------------------------------------------------------------------
%
% Pools
%
% ----------------------------------------------------------------------

:- export colgen_instance/1.	 % colgen_instance(+PoolName)

% ----------------------------------------------------------------------
%
% Predicates with pool argments (don't reexport these in colgen!)
%
% ----------------------------------------------------------------------

:- reexport var_dual/6,             % var_dual/5
            get_dual/3,             % get_dual/2
            get_idx/3,              % get_idx/2
            get_rhs/3,              % get_rhs/2
            always_set_dual/3,      % always_set_dual/2
            set_dual/3              % set_dual/2
            from dual_var.

:- export bp_solve/2.               % solve/1
:- export cg_solver_setup/3.        % solver_setup/2
:- export cg_solver_setup/4.        % solver_setup/3
:- export cg_integers/2.            % integers/1
:- export add_cg_pool_constraint/3. % identified_constraint/2
:- export cg_eq/3.		    % =:=/2
:- export cg_ge/3.		    % >=/2
:- export cg_le/3.		    % =</2
:- export cg_sp_count/2.            % cg_subproblem_count/1
:- export cg_sp_sol/2.              % cg_subproblem_solution/1
:- export cg_sp_rc_sum/2.           % cg_subproblem_rc_sum/1
:- export cg_minimize/5.            % minimize/4
:- export cg_minimize/4.            % minimize/3
:- export cg_var_get/4.             % var_get/3
:- export cg_get/3.                 % get/2

:- local struct(cg_constraint_type(mp_only, mp_sp, mp_branch)).

:- export op(700, xfx, [$>=, $=, $=<, $::]).

% ----------------------------------------------------------------------
%
% Problem handle structure
%
% ----------------------------------------------------------------------

:- export struct(
                 cg_prob(
                         % Global State:
                         master_prob,       % the master problem
                                            % eplex handle
                         bfs_tree,          % bfs_tree for branch-and-price
                         mp_susp,           % suspension list containing the
                                            % suspension for the MP solver
                         const_obj,         % real: the constant part of
                                            %       the cost funciton
                         phase1_obj,        % Expr: the artificial variable
                                            %       cost fn for phase 1
                                            %       of two-phase
                         sp_solution_call,  % the user supplied subproblem
                                            % solution predicate
                         sp_count,          % int: the number of
                                            %      subproblems
                         pool,              % the associated constraint pool
                         tolerance,         % float: tolerance for optimality
                         info_messages,     % on, off: info message
                                            % status
                         on_degeneracy,     % stop, continue: should we
                                            %                 halt when
                                            %                 we find
                                            %                 degeneracy,
                                            %                 or continue
                                            %                 if so the
                                            %                 sp solver
                                            %                 is assumed
                                            %                 to deal
                                            %                 with it
                                            %
                         stabilisation,     % of, on(), user-defined:
                                            %          the policy to
                                            %          perform basis
                                            %          stabilisation
                         stab_terms,        % [StabTerm1,...,StabTermM]:
                                            %           list of
                                            %           stabilisation
                                            %           terms 
                         stab_iter_counts,  % c(BCount,CCount):
                                            %           record of how
                                            %           many iterations
                                            %           since
                                            %           stabilisation
                                            %           var bound and
                                            %           coeff update
                                            %
                         disallow,          % lp, clp, off:
                                            %           policy for
                                            %           active
                                            %           prevention of
                                            %           duplicate
                                            %           columns
                                            %
                         basis_perturbation,% on, off: should we try
                                            %          and perturb external
                                            %          solver basis when appear
                                            %          to be at optimal when
                                            %          external solver returns
                                            %          same basis after adding
                                            %          columns:
                                            %          off - no
                                            %          on - temporarily set 
                                            %               the external solver
                                            %               to always perturb
                                            %
			 upper_bound,       % float: current bounds on solution
                         lower_bound,       %        objective value
                         integral_obj,      % atom: yes or no, is the cost
                                            %       of all feasible
                                            %       solutions integral?
                         duals,             % array: array of current
                                            %        master problem dual values
                         idx_lookup,        % hash table: lookup table to
                                            %             convert master problem
                                            %             constraint ids into
                                            %             external solver row ids
                         sp_obj_terms,      % array: array of implicit sum
                                            %        terms forming the obj
                                            %        function of subproblems
                                            %        in same order as the
                                            %        duals array that are
                                            %        their coeffs in it
                         mp_cols_added,     % int: total number of columns
                                            %      added to MP
			 mp_vars,           % [Var1,...,Varj]:
                                            %       list of all mp vars
			 sep_call,          % [sp(C):mp(ObjSense,ObjVar),
                                            %  sp(Arg1):mp(Idx1,Var1),...,
                                            %  sp(Argi):mp(Idxi,Vari)]:
                                            %    list of structural subproblem
                                            %    vars with subproblem argument
                                            %    and master problem
                                            %    coefficient row index
                         % Column Management:
			 col_del,           % atom: ?,none:
                                            %       column deletion strategy
                         shelf              % store for intermediate results
                        )
                ).

:- export struct(
                 stab_term(
                           idx,
                           plus_var,
                           plus_coeff,
                           plus_bound,
                           minus_var,
                           minus_coeff,
                           minus_bound
                          )
                ).

% ----------------------------------------------------------------------
%
% Subproblem handle structure
%
% ----------------------------------------------------------------------

:- export struct(
                 sp_prob(
                         master_pool,       % atom: the MP pool to which to
                                            % post cg_subproblem_solution/1 
                         cutoff,            % float: the bound for termination
                                            %        of column generation
                         cost,              % var: dual_var for SP solution
                                            %      cost coefficient in MP
                         coeff_vars,        % [Ai,...,An]: list of dual_var
                                            %      vars for SP solution
                                            %      coefficients in original
                                            %      constraints of MP
                         aux,               % term: arbitrary additional
                                            %       data stored in colgen
                                            %       attribute of MP vars
                         disallow,          % list of cstrs to post if
                                            % actively preventing
                                            % duplicate columns
                         status,            % phase1, phase2, degenerate
                         module             % module: module in which
                                            %         to call sp_call
                         )
                ).
                         
% ----------------------------------------------------------------------
%
% Subproblem solution structure
%
% ----------------------------------------------------------------------

:- export struct(
                 sp_sol(
                        cost,              % number: cost coefficient in MP
                        coeff_vars,        % [Ai,...,An]: list of reals:
                                           %      coefficients in original
                                           %      constraints of MP
                        aux,               % term: arbitrary additional
                                           %       data
                        lo,                % lower bound
                        hi,                % upper bound
                        type               % type integer or real
                       )
                ).

% ----------------------------------------------------------------------
%
% Temporary var info structure
%
% ----------------------------------------------------------------------

:- local struct(
                cg_var_info(
                            lo,
                            hi,
                            val,
                            reduced_cost,
                            type,
                            attr
                           )
               ).

% ----------------------------------------------------------------------
%
% cg attribute handlers
%
% ----------------------------------------------------------------------

unify_colgen(_, Attr) :-
        var(Attr).                   % Ignore if not a colgen var
unify_colgen(Term, Attr) :-
        compound(Attr),
        unify_term_colgen(Term, Attr).

:- mode unify_term_colgen(?, +).
unify_term_colgen(X, Attr) :-
        nonvar(X),                   % colgen var and NONVAR - instantiated
        instantiation_deviates_for_pool(Attr, X).
unify_term_colgen(Y{colgen:AttrY}, AttrX) :-
        -?->
        unify_colgen_colgen(Y, AttrY, AttrX).

unify_colgen_colgen(_, AttrY, AttrX) :-
        var(AttrY),	            % No attribute for this extension
        AttrX = AttrY.	            % Transfer the attribute
unify_colgen_colgen(_, AttrY, AttrX) :-
        nonvar(AttrY),              % colgen var and colgen var
        unify_pools(AttrX, AttrY).

instantiation_deviates_for_pool(ThisAttr, X) :-
        ( compound(ThisAttr) ->
              ThisAttr = colgen with [
                                      mp_val:Val,

                                      cost:Cost,
                                      solver:Pool,
                                      
                                      next:NextAttr
                                     ],
              ( float(X) =:= 0 ->
                  true
              ; var(Cost) ->
                  printf(warning_output,
                         "Warning: instantiating a variable for"
                         " %w with unspecified cost coefficient to"
                         " %w - assuming zero cost%n", [Pool, X])
              ;
                  get_pool_handle(Handle, Pool),
                  Handle = cg_prob with const_obj:Const*One,
                  Const0 is Const + Cost * float(X),
                  setarg(const_obj of cg_prob, Handle, Const0*One)
              ),
              ( X = Val -> % instantiated to its mp_val
                    true
              ;     % otherwise wake the mp
                    % should probably post a constraint
                    % to the sp disallowing the
                    % corresponding sp solution
                    schedule_suspensions(master_prob of colgen, ThisAttr),
                    wake
              ),
              instantiation_deviates_for_pool(NextAttr, X)
	;    
              % chain terminated by atom 'end'
              true
        ).

unify_pools(ThisAttrX, AttrY) :-
        ThisAttrX = colgen with [
                                 solver:Pool,
                                 cost:Cost,
                                 coeffs:Coeffs,
                                 aux:Aux,
                                 next:NextAttrX
                                ],
        remove_cg_attr_for_pool(Pool, AttrY, ThisAttrY, NextAttrY),
        (   % if Y has an attribute for Pool they must match
            ThisAttrY = colgen with [
                                     cost:Cost,
                                     coeffs:Coeffs,
                                     aux:Aux
                                    ],
            % two variables in the same solver are unified,
            % send an equality constraint for the two columns
            % to the external solver and wake it
            schedule_suspensions(master_prob of colgen, ThisAttrX),
            wake
        ;
            % Y has no attribute for Pool
            ThisAttrY = end
        ),
        % continue with the rest of X and Ys chains
        unify_pools(NextAttrX, ThisAttrX, NextAttrY).

unify_pools(ThisAttrX, Attr0, AttrY) :-
        ( compound(ThisAttrX) ->
              ( compound(AttrY) ->
                    ThisAttrX = colgen with [
                                             solver:Pool,
                                             cost:Cost,
                                             coeffs:Coeffs,
                                             aux:Aux,
                                             next:NextAttrX
                                            ],
                    remove_cg_attr_for_pool(Pool, AttrY, ThisAttrY, NextAttrY),
                    (   % if Y has an attribute for Pool they must match
                        ThisAttrY = colgen with [
                                                 cost:Cost,
                                                 coeffs:Coeffs,
                                                 aux:Aux
                                                ],
                        % two variables in the same solver are unified,
                        % send an equality constraint for the two columns
                        % to the external solver and wake it
                        schedule_suspensions(master_prob of colgen, ThisAttrX),
                        wake
                    ;
                        % Y has no attribute for Pool
                        ThisAttrY = end
                    ),
                    % continue with the rest of X and Ys chains
                    unify_pools(NextAttrX, ThisAttrX, NextAttrY)
              ;
                    % Ys chain terminated by atom'end'
                    true
              )
        ;
              % Xs chain terminated by atom 'end'
              % put the rest of Ys chain here
              setarg(next of colgen, Attr0, AttrY)
        ).

% From a cg_attr, removes the attribute corresponding to that for the
% first argument form the chain and returns it. Fails if none found. 
remove_cg_attr_for_pool(Pool, ThisAttr, Attr, RestAttr) :-
        % no need to test for var(ThisAttr) in chain
        ThisAttr = colgen with [solver:ThisPool, next:NextAttr],
	(ThisPool == Pool ->
             RestAttr = NextAttr,
             setarg(next of colgen, ThisAttr, end),
	     Attr = ThisAttr
	;    
             RestAttr = ThisAttr,
	     dechain_cg_attr_for_pool1(Pool, NextAttr, ThisAttr, Attr)
	).
        
dechain_cg_attr_for_pool1(Pool, ThisAttr, Attr0, Attr) :-
        % no need to test for var(ThisAttr) in chain
        ( ThisAttr = colgen with [solver:ThisPool, next:NextAttr] ->
              (ThisPool == Pool ->
                   setarg(next of colgen, Attr0, NextAttr),
                   setarg(next of colgen, ThisAttr, end),
                   Attr = ThisAttr
              ;    
                   dechain_cg_attr_for_pool1(Pool, NextAttr, ThisAttr, Attr)
              )
        ;     % chain terminated by atom 'end'
              ThisAttr = Attr
        ).

% get_bounds handler

cg_get_bounds(_Var{colgen:Attr}, Lwb, Upb) ?-
        ( var(Attr) -> true
        ; cg_get_attr_bounds(Attr, -1.0Inf, 1.0Inf, Lwb, Upb) ).

cg_get_attr_bounds(colgen with [lo:Lo1, hi:Hi1, next:Next],
                   Lo0, Hi0, Lo, Hi) ?-
        Lo1 =< Hi0,
        Hi1 >= Lo0,
	Lo2 is max(Lo0, Lo1),
	Hi2 is min(Hi0, Hi1),
        cg_get_attr_bounds(Next, Lo2, Hi2, Lo, Hi).
cg_get_attr_bounds(end, Lo0, Hi0, Lo, Hi) ?-
        Lo0 = Lo, Hi0 = Hi.

cg_set_bounds(Var{colgen:Attr}, Lwb, Upb) ?-
        ( var(Attr) -> true
        ; cg_set_attr_bounds(Var, Attr, Lwb, Upb) ).

pos_inf(1e+20) :- !.
pos_inf(1.0Inf) :- !.

neg_inf(-1e+20) :- !.
neg_inf(-1.0Inf) :- !.

cg_set_attr_bounds(Var, Attr, Lwb, Upb) :-
        ( compound(Attr) ->
              Attr = colgen with [
                                  coeffs:Coeffs,
                                  solver:Pool,
                                  lo:Lwb0,
                                  hi:Upb0,
                                  type:Type,
                                  next:NextAttr
                                 ],
              ( Type == real ->
                  Lwb1 = Lwb,
                  Upb1 = Upb
              ; Type == integer ->
                  ( neg_inf(Lwb) -> Lwb1 = Lwb ; Lwb1 is fix(ceiling(Lwb)) ),
                  ( pos_inf(Upb) -> Upb1 = Upb ; Upb1 is fix(floor(Upb)) )
              ),
              ( Lwb1 > Lwb0 ->
                  ( (neg_inf(Lwb0) ; neg_inf(Lwb1)) ->
                      Handle = cg_prob with master_prob: MPHandle,
                      ( var(MPHandle) ->
                          % master problem lp not set up yet, will be
                          % taken care of in cg_solver_setup
                          true
                      ; lp_var_occurrence(Var, MPHandle, _I) ->
                          lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                      ;
                          % Var not yet added to master problem lp, will be
                          % taken care of when added
                          true
                      ),
                      LwbDiff = 0
                  ;
                      LwbDiff is Lwb1 - Lwb0
                  ),
                  setarg(lo of colgen, Attr, Lwb1)
              ;
                  LwbDiff = 0
              ),
              ( Upb1 < Upb0 ->
                  ( (pos_inf(Upb0) ; pos_inf(Upb1)) ->
                      Handle = cg_prob with master_prob: MPHandle,
                      ( var(MPHandle) ->
                          % master problem lp not set up yet, will be
                          % taken care of in cg_solver_setup
                          true
                      ; lp_var_occurrence(Var, MPHandle, _I) ->
                          lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                      ;
                          % Var not yet added to master problem lp, will be
                          % taken care of when added
                          true
                      ),
                      UpbDiff = 0
                  ;
                      UpbDiff is Upb1 - Upb0
                  ),
                  setarg(hi of colgen, Attr, Upb1)
              ;
                  UpbDiff = 0
              ),
              ( LwbDiff = 0, UpbDiff = 0 ->
                  true
              ;
                  get_pool_handle(Handle, Pool),
                  Handle = cg_prob with [
                                         master_prob: MPHandle,
                                         sp_solution_call: SolveSubProblem
                                        ],
                  ( var(MPHandle) ->
                      % master problem lp not set up yet, will be
                      % taken care of in cg_solver_setup
                      true
                  ; lp_var_occurrence(Var, MPHandle, _I) ->
                      lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                  ;
                      % Var not yet added to master problem lp, will be
                      % taken care of when added
                      true
                  ),
                  ( var(SolveSubProblem) ->
                      true
                  ; var(Coeffs) ->
                      true
                  ;
                      SolveSubProblem =.. [_, sp_prob with coeff_vars:DualVars|_],
                      (
                          foreach(DualVar, DualVars),
                          param(Pool, Coeffs, LwbDiff, UpbDiff)
                      do
                          get_idx(DualVar, Ident, Pool),
                          ( once member(Ident-Val, Coeffs) ->
                              get_lhs_range(DualVar, Lo0, Hi0),
                              Lo1 is Lo0 + Val*LwbDiff,
                              Hi1 is Hi0 + Val*UpbDiff,
                              set_lhs_range(DualVar, Lo1, Hi1)
                          ;
                              true
                          )
                      )
                  )
              ),
              cg_set_attr_bounds(Var, NextAttr, Lwb, Upb)
	;    
              % chain terminated by atom 'end'
              true
        ).

cg_var_print(_{colgen:Attr}, Printed) ?-
        nonvar(Attr), 
        printed_cg_attributes(Attr, Printed).

printed_cg_attributes(Attr, Printed) :-
        ( compound(Attr) ->
              Attr = colgen with [
                                  solver:Pool,
                                  mp_val:Val,
                                  cost:Cost,
                                  coeffs:Coeffs,
                                  aux:Aux,
                                  lo:Lo,
                                  hi:Hi,
                                  next:NextAttr
                                 ],
              Printed = [Pool:[mp_val:Val, cost:Cost, coeffs:Coeffs,
                               aux:Aux, lo:Lo, hi:Hi]|Rest],
              printed_cg_attributes(NextAttr, Rest)
	;    
              % chain terminated by atom 'end'
              Printed = []
        ).

get_cg_attr(X{colgen:Attr0}, Pool, Attr) ?-
        ( var(Attr0) ->
              new_cg_attr(X, Pool, Attr)
        ;
              Attr0 = colgen with [solver:Pool0,next:Next],
              % should not fail unless Attr0 incorrect
              ( Pool0 == Pool ->
                    Attr = Attr0
              ;
                    get_cg_attr1(Next, Attr0, Pool, Attr)
              )
        ).
get_cg_attr(X, Pool, Attr) :-           % make a new colgen variable
        free(X),
        new_cg_attr(X, Pool, Attr).

get_cg_attr1(ThisAttr, Attr0, Pool, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_cg_attrstruct(Pool, Attr),
	setarg(next of colgen, Attr0, Attr).
get_cg_attr1(ThisAttr, _Attr0, Pool, Attr) :-
        ThisAttr = colgen with [solver:Pool0,next:Next],
        ( Pool0 == Pool ->
              Attr = ThisAttr
        ;
              get_cg_attr1(Next, ThisAttr, Pool, Attr)
        ).

new_cg_attr(X, Pool, Attr) :-           % make a new colgen variable:
        new_cg_attrstruct(Pool, Attr),
        add_attribute(X, Attr, colgen). % and add a colgen attribute

:- mode new_cg_attrstruct(+,?).
new_cg_attrstruct(Pool, Attr) :-
        Attr = colgen with [mp_val:0, solver:Pool, lo: 0.0, hi: 1.0Inf,
                            type:real, next:end],
        init_suspension_list(master_prob of colgen, Attr).

% From a cg_attr, searches for the attribute corresponding to that for the
% first argument. Fails if none found. 
get_cg_attr_for_pool(Pool, Attr0, Attr) :-
        compound(Attr0), 
	get_cg_attr_for_pool1(Pool, Attr0, Attr).

get_cg_attr_for_pool1(Pool, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = colgen with [solver:Pool0,next:NextAttr],
	(Pool0 == Pool ->
	     Attr0 = Attr
	;    
	     get_cg_attr_for_pool1(Pool, NextAttr, Attr)
	).

cg_var_get(X, What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_var_get(X, What, Val)]),
	    abort
	;
	    cg_var_get1(Pool, X, What, Val)
	).

cg_var_get1(Pool, X, node_val, Val) :- !,
        get_pool_handle(cg_prob with bfs_tree:BfsInstance, Pool),
        BfsInstance:var_get(X, node_val, Val).
cg_var_get1(Pool, X, reduced_cost, Val) :- !,
        get_pool_handle(cg_prob with bfs_tree:BfsInstance, Pool),
        BfsInstance:var_get(X, reduced_cost, Val).
cg_var_get1(Pool, X, mp_val, Val) :- !,
        cg_var_mp_val(Pool, X, Val).
cg_var_get1(Pool, X, cost, Val) :- !,
        cg_var_cost(Pool, X, Val).
cg_var_get1(Pool, X, coeffs, Val) :- !,
        cg_var_coeffs(Pool, X, Val).
cg_var_get1(Pool, X, aux, Val) :- !,
        cg_var_aux(Pool, X, Val).
cg_var_get1(Pool, X, What, Val) :-
	error(6, cg_var_get(Pool, X, What, Val)).

cg_var_mp_val(Pool, _{colgen:Attr0}, Sol) ?-
	get_cg_attr_for_pool(Pool, Attr0, Attr),
	Attr = colgen with mp_val:Sol.
cg_var_mp_val(_, X, Sol) :-
	integer(X),
	Sol is float(X).
cg_var_mp_val(_, X, Sol) :-
	real(X),
        X = Sol.

cg_var_cost(Pool, _{colgen:Attr0}, Cost) ?-
	get_cg_attr_for_pool(Pool, Attr0, Attr),
        Attr = colgen with cost:Cost.

cg_var_coeffs(Pool, _{colgen:Attr0}, Coeffs) ?-
	get_cg_attr_for_pool(Pool, Attr0, Attr),
        Attr = colgen with coeffs:Coeffs.

cg_var_aux(Pool, _{colgen:Attr0}, Aux) ?-
	get_cg_attr_for_pool(Pool, Attr0, Attr),
        Attr = colgen with aux:Aux.

cg_integers(Ints, Pool) :-
        get_pool_handle(Handle, Pool),
        ( Handle = cg_prob with bfs_tree:[] ->
              concat_string([Pool, '_bfs'], BfsInstanceName),
              atom_string(BfsInstance, BfsInstanceName),
              bfs_instance(BfsInstance),
              Handle = cg_prob with info_messages:OnOff,
              BfsInstance:solver_setup(min, bp_node(Pool),
                                       [info_messages:OnOff,
                                        separation(bp_separate(Pool))]),
              setarg(bfs_tree of cg_prob, Handle, BfsInstance)
        ; Handle = cg_prob with bfs_tree:BfsInstance ),
        ( var(Ints) ->
              get_cg_attr(Ints, Pool, Attr),
              setarg(type of colgen, Attr, integer)
        ;
              (
                  foreach(Int, Ints),
                  param(Pool)
              do
                  get_cg_attr(Int, Pool, Attr),
                  setarg(type of colgen, Attr, integer)
              )
        ),
        BfsInstance:integers(Ints).

% ----------------------------------------------------------------------
% The user-level constraints
% ----------------------------------------------------------------------

cg_range(X, Lo..Hi, Pool) :-
        get_cg_attr(X, Pool, _Attr), % make sure it is a var for Pool
        set_var_bounds(X, Lo, Hi).

cg_eq(X, Y, Pool) :- add_cg_pool_constraint(X=:=Y, _Id, Pool).
cg_ge(X, Y, Pool) :- add_cg_pool_constraint(X>=Y, _Id, Pool).
cg_le(X, Y, Pool) :- add_cg_pool_constraint(X=<Y, _Id, Pool).

add_cg_pool_constraint(Cstr, Ident, Pool) :-
	cg_normalise_cstr(Cstr, Norm0, CoeffVar),
	!,
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with idx_lookup:Lookup,
        ( nonvar(Ident) ->
              true
        ;
              Id = Ident
        ),
        suspend(hash_set(Lookup, Ident, Id), 1, Id->inst),
        ( nonvar(CoeffVar) ->
              % only involves existing MP vars
              try_propagate_bounds(Norm0, Norm),
              (var(Norm) ->
                   true     % constraint simplified away
              ;
                   
                   Handle = cg_prob with master_prob:MPHandle,
                   ( nonvar(MPHandle) ->
                         lp_add_indexed(MPHandle, [Norm], [], [Id])
                   ;
                         
                   post_typed_pool_constraint(Pool,
                                              mp_only of cg_constraint_type,
                                              Id:Norm)
                   
                   )
                   
              )
        ;
              % involves MP vars to be generated by SPs
              % cannot propagate bounds yet
              % give the CoeffVar of the Lambda vars to be generated
              % a dual_var attribute
              Norm0 = Sense:[Val*_One|_],
              Rhs is -Val,
              var_dual(CoeffVar, 0, Ident, Sense, Rhs, Pool),
              post_typed_pool_constraint(Pool,
                                         mp_sp of cg_constraint_type,
                                         [CoeffVar, Id]:Norm0)
	).
add_cg_pool_constraint(Cstr, _Id, Pool) :-
	error(5, Pool:Cstr).

try_propagate_bounds(NormIn, NormOut) :-
	NormIn = Sense:[Cst*_|Lhs],
	( Lhs = [] ->			% ground: check immediately
	    satisfied(Sense, Cst)
	; Lhs = [C*X] ->		% single var: update its bound
	    Bound is -Cst/C,
	    swap_sense(C, Sense, Sense1),
            ( Sense1 = (=<) -> set_var_bounds(X, -1.0Inf, Bound)
            ; Sense1 = (>=) -> set_var_bounds(X, Bound, 1.0Inf)
            ; set_var_bounds(X, Bound, Bound) ), % may bind X!
	    wake
	;				
	    NormIn = NormOut
	).

    satisfied((=<), C) :- C =< 0.
    satisfied((>=), C) :- C >= 0.
    satisfied((=:=), C) :- C =:= 0.

    swap_sense(C, (=<), (>=)) :- C < 0, !.
    swap_sense(C, (>=), (=<)) :- C < 0, !.
    swap_sense(_, S, S).


cg_sp_count(P, Pool) :-
        get_pool_handle(cg_prob with sp_count:P, Pool).

cg_sp_sol(SPSol, Pool) :-
        ( SPSol = [_|_] ->
            (
                foreach(Sol, SPSol),
                fromto(NewSolns, [Sol|Rest], Rest, Solns),
                param(Pool)
            do
                ( ground(Sol) -> true
                ; get_pool_handle(Handle, Pool),
                  term_variables(Sol, Vars),
                  cg_info_message(Handle, "%nSubproblem solution contains"
                                  " variables%n%w%n"
                                  "in %w:cg_subproblem_solution(%w)%n"
                                  "Corresponding variables in colgen attribute"
                                  " will be copies.%n",
                                  [Vars, Pool, Sol])
                )
            )
        ;
            ( ground(SPSol) -> true
            ; get_pool_handle(Handle, Pool),
              term_variables(SPSol, Vars),
              cg_info_message(Handle, "%nSubproblem solution contains"
                              " variables%n%w%n"
                              "in %w:cg_subproblem_solution(%w)%n"
                              "Corresponding variables in colgen attribute"
                              " will be copies.%n",
                              [Vars, Pool, SPSol])
            ),
            NewSolns = [SPSol|Solns]
        ),
        % store the subproblem solution
        getval(sp_solns(0), Solns)@Pool,
        setval(sp_solns(0), NewSolns)@ Pool.

cg_sp_rc_sum(SPRCSum, Pool) :-
        setval(sp_rc_sum,SPRCSum)@Pool. 

cg_branch(Score, Branch, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:branch(Score, Branch)]),
	    abort
	;
	    Handle = cg_prob with shelf:Shelf,
            shelf_get(Shelf, 1, Branches),
            shelf_set(Shelf, 1, [Score:Branch|Branches])
	).

cg_branch(Branch, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:branch(Branch)]),
	    abort
	;
	    Handle = cg_prob with shelf:Shelf,
            shelf_get(Shelf, 1, Branches),
            shelf_set(Shelf, 1, [0:Branch|Branches])
	).

cg_info_message(cg_prob with info_messages:OnOff, String, Vars) :-
        ( OnOff == on -> printf(String, Vars), flush(output) ; true ).

cg_get(What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_get(What, Val)]),
	    abort
	;
	    cg_get1(Pool, What, Val)
	).

cg_get1(Pool, obj_val, ObjVal) :- !,
        get_pool_handle(cg_prob with [
                                      master_prob:MPHandle,
                                      mp_vars:Vars,
                                      const_obj:Const*_One
                                     ],
                        Pool),
        (
            foreach(Var, Vars),
            fromto(Const, In, Out, ObjVal),
            param(Pool, MPHandle)
        do
            ( nonvar(Var) ->
                  Out = In
            ;
                  cg_var_cost(Pool, Var, Cost),
                  lp_var_get(MPHandle, Var, solution, VarVal),
                  Out is In + Cost*VarVal
            )
        ).
cg_get1(Pool, sp_obj(Idents), Val) :- !,
        get_pool_handle(cg_prob with [
                                      duals:DualArr,
                                      idx_lookup:Lookup,
                                      sp_obj_terms:TermArr
                                     ],
                        Pool),
        ( Idents == all ->
              TermArr =.. [[]|Terms],
              DualArr =.. [[]|Duals],
              (
                  fromto(Val, Out, In, []),
                  foreach(Dual, Duals),
                  foreach(Term, Terms)
              do
                  ( nonvar(Term), Term =:= 0 -> Out = In
                  ; Out = [Dual*Term|In] )
              )
        ; Idents = [_|_] ->
              (
                  foreach(Ident, Idents),
                  foreach(Dual*Term, Val),
                  param(Lookup, DualArr, TermArr)
              do
                  hash_get(Lookup, Ident, Id),
                  Id1 is Id + 1,
                  arg(Id1, DualArr, Dual),
                  arg(Id1, TermArr, Term)
              )
        ;
              hash_get(Lookup, Idents, Id),
              Id1 is Id + 1,
              arg(Id1, DualArr, Dual),
              arg(Id1, TermArr, Term),
              Val = Dual*Term
        ).
cg_get1(Pool, dual(Idents), Val) :- !,
        get_pool_handle(cg_prob with [
                                      duals:DualArr,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        ( Idents == all ->
              DualArr =.. [[]|Val]
        ; Idents = [_|_] ->
              (
                  foreach(Ident, Idents),
                  foreach(V, Val),
                  param(Lookup, DualArr)
              do
                  hash_get(Lookup, Ident, Id),
                  Id1 is Id + 1,
                  arg(Id1, DualArr, V)
              )
        ;
              hash_get(Lookup, Idents, Id),
              Id1 is Id + 1,
              arg(Id1, DualArr, Val)
        ).
cg_get1(Pool, column_count, Val) :- !,
        get_pool_handle(cg_prob with mp_cols_added:Val, Pool).
cg_get1(Pool, unsatisfiable_cstrs, Val) :- !,
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with sp_solution_call:SolveSubProblem,
        SolveSubProblem =.. [_, sp_prob with coeff_vars:DualVars|_],
        (
            foreach(DualVar, DualVars),
            fromto(Val, Out, In, []),
            param(Pool)
        do
            ( satisfiable_primal_cstr(DualVar, Pool) -> Out = In ; Out = [DualVar|In] )
        ).
cg_get1(Pool, satisfiable_cstrs, Val) :- !,
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with sp_solution_call:SolveSubProblem,
        SolveSubProblem =.. [_, sp_prob with coeff_vars:DualVars|_],
        (
            foreach(DualVar, DualVars),
            fromto(Val, Out, In, []),
            param(Pool)
        do
            ( satisfiable_primal_cstr(DualVar, Pool) -> Out = [DualVar|In] ; Out = In )
        ).
cg_get1(Pool, frac_vars, Val) :- !,
        get_pool_handle(cg_prob with bfs_tree:BfsInstance, Pool),
        BfsInstance:get(frac_vars, Val).
cg_get1(Pool, generated_non_zero_vars, Val) :- !,
        get_pool_handle(cg_prob with mp_vars:Vars, Pool),
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Pool)
        do
            ( nonvar(Var) ->
                  ( Var > 1e-05 -> Out = [Var|In] ; Out = In )
            ;
                  get_cg_attr(Var, Pool, colgen with [mp_val:Sol, coeffs:Coeffs]),
                  ( abs(Sol) =< 1e-05 -> Out = In
                  ; Coeffs = [] -> Out = In
                  ; Out = [Var|In] )
            )
        ).
cg_get1(Pool, non_zero_vars, Val) :- !,
        get_pool_handle(cg_prob with mp_vars:Vars, Pool),
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Pool)
        do
            ( nonvar(Var) ->
                  ( Var > 1e-05 -> Out = [Var|In] ; Out = In )
            ;
                  get_cg_attr(Var, Pool, colgen with mp_val:Sol),
                  ( abs(Sol) =< 1e-05 -> Out = In ; Out = [Var|In] )
            )
        ).
cg_get1(Pool, generated_vars, Val) :- !,
        get_pool_handle(cg_prob with mp_vars:Vars, Pool),
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Pool)
        do
            ( nonvar(Var) ->
                  Out = In
            ;
                  get_cg_attr(Var, Pool, colgen with coeffs:Coeffs),
                  ( Coeffs = [] -> Out = In
                  ; Out = [Var|In] )
            )
        ).
cg_get1(Pool, vars, Val) :- !,
        get_pool_handle(cg_prob with mp_vars:Val, Pool).
cg_get1(Pool, sep_goal, Val) :- !,
        get_pool_handle(cg_prob with sep_call:(call(Val)@_Module), Pool).
cg_get1(Pool, sp_solver, Val) :- !,
        get_pool_handle(cg_prob with sp_solution_call:Val, Pool).

cg_get1(Pool, stab_coeff_minus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        get_stab_coeff_minus(StabTerms, Id, Val).
cg_get1(Pool, stab_coeff_plus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        get_stab_coeff_plus(StabTerms, Id, Val).
cg_get1(Pool, stab_bound_minus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        get_stab_bound_minus(StabTerms, Id, Val).
cg_get1(Pool, stab_bound_plus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        get_stab_bound_plus(StabTerms, Id, Val).

cg_get1(Pool, What, Val) :-
	error(6, cg_get(Pool, What, Val)).

cg_set(What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_set(What, Val)]),
	    abort
	;
	    cg_set1(Pool, What, Val)
	).

cg_set1(Pool, disallow, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(disallow of cg_prob, Handle, Val).
cg_set1(Pool, int_tolerance, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(tolerance of cg_prob, Handle, Val).
cg_set1(Pool, basis_perturbation, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(basis_perturbation of cg_prob, Handle, Val).
cg_set1(Pool, info_messages, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(info_messages of cg_prob, Handle, Val),
        Handle = cg_prob with bfs_tree:BfsInstance,
        ( BfsInstance = [] -> true ; BfsInstance:set(info_messages, Val) ).

cg_set1(Pool, on_degeneracy, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(on_degeneracy of cg_prob, Handle, Val).
cg_set1(Pool, stabilisation, Val) :- !,
        get_pool_handle(Handle, Pool), 
	setarg(stabilisation of cg_prob, Handle, Val).
cg_set1(Pool, stab_coeff_minus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        set_stab_coeff_minus(StabTerms, Id, Val).
cg_set1(Pool, stab_coeff_plus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        set_stab_coeff_plus(StabTerms, Id, Val).
cg_set1(Pool, stab_bound_minus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      master_prob:MPHandle,
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        set_stab_bound_minus(StabTerms, Id, MPHandle, Val).
cg_set1(Pool, stab_bound_plus(Ident), Val) :- !,
        get_pool_handle(cg_prob with [
                                      master_prob:MPHandle,
                                      stab_terms:StabTerms,
                                      idx_lookup:Lookup
                                     ],
                        Pool),
        hash_get(Lookup, Ident, Id),
        set_stab_bound_plus(StabTerms, Id, MPHandle, Val).

cg_set1(Pool, What, Val) :-
	error(6, Pool:cg_set(What, Val)).

cg_statistics(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with bfs_tree:BfsInstance,
        ( BfsInstance == [] -> true ; BfsInstance:statistics ).

get_stab_coeff_plus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            arg(plus_coeff of stab_term, StabTerm, Val)
        ;
            get_stab_coeff_plus(StabTerms, Id, Val)
        ).

get_stab_coeff_minus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            arg(minus_coeff of stab_term, StabTerm, Val)
        ;
            get_stab_coeff_minus(StabTerms, Id, Val)
        ).

get_stab_bound_plus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            arg(plus_bound of stab_term, StabTerm, Val)
        ;
            get_stab_bound_plus(StabTerms, Id, Val)
        ).

get_stab_bound_minus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            arg(minus_bound of stab_term, StabTerm, Val)
        ;
            get_stab_bound_minus(StabTerms, Id, Val)
        ).

set_stab_coeff_plus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            setarg(plus_coeff of stab_term, StabTerm, Val)
        ;
            set_stab_coeff_plus(StabTerms, Id, Val)
        ).

set_stab_coeff_minus([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term with idx: Idx,
        ( Idx = Id ->
            setarg(minus_coeff of stab_term, StabTerm, Val)
        ;
            set_stab_coeff_minus(StabTerms, Id, Val)
        ).

set_stab_bound_plus([StabTerm|StabTerms], Id, MPHandle, Val) :-
        StabTerm = stab_term with [
                                   idx: Idx,
                                   plus_var: Yplus
                                  ],
        ( Idx = Id ->
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Val),
            setarg(plus_bound of stab_term, StabTerm, Val)
        ;
            set_stab_bound_plus(StabTerms, Id, MPHandle, Val)
        ).

set_stab_bound_minus([StabTerm|StabTerms], Id, MPHandle, Val) :-
        StabTerm = stab_term with [
                                   idx: Idx,
                                   minus_var: Yminus
                                  ],
        ( Idx = Id ->
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Val),
            setarg(minus_bound of stab_term, StabTerm, Val)
        ;
            set_stab_bound_minus(StabTerms, Id, MPHandle, Val)
        ).

% ----------------------------------------------------------------------
% The optimisation predicates
% ----------------------------------------------------------------------

cg_minimize(SolveSubProblem, Obj, ObjVal, Pool) :-
        cg_minimize(SolveSubProblem, Obj, [], ObjVal, Pool).

cg_minimize(SolveSubProblem, Obj, OptionList, ObjVal, Pool) :-
        % setup original MP problem
        cg_solver_setup(SolveSubProblem, Obj, OptionList, Pool, _),
        % solve the initial MP
        get_pool_handle(Handle, Pool),
        cg_masterproblem(Handle, Pool),
        % tidy up suspensions
        %kill_suspension(MPSusp),
        %schedule_suspensions(mp_susp of cg_prob, Handle),
        %kill_suspension(SPSusp),
        
        Handle = cg_prob with [
                               master_prob:MP,
                               upper_bound:ObjVal
                              ],
        lp_get(MP, vars, AllVarArr),
        AllVarArr =.. [_|AllVars],
        (
            foreach(Var, AllVars),
            param(MP, Pool)
        do
            ( nonvar(Var) ->
                  true
            ;
                  lp_var_get(MP, Var, solution, Val),
                  get_cg_attr(Var, Pool, Attr),
                  setarg(mp_val of colgen, Attr, Val)
            )
        ).

bp_solve(Obj, Pool) :-
        get_pool_handle(Handle, Pool),
        ( Handle = cg_prob with bfs_tree:[] ->
              concat_string([Pool, '_bfs'], BfsInstanceName),
              atom_string(BfsInstance, BfsInstanceName),
              bfs_instance(BfsInstance),
              Handle = cg_prob with info_messages:OnOff,
              BfsInstance:solver_setup(min, bp_node(Pool),
                                       [info_messages:OnOff,
                                        separation(bp_separate(Pool))]),
              setarg(bfs_tree of cg_prob, Handle, BfsInstance)
        ; Handle = cg_prob with bfs_tree:BfsInstance
        ),
        BfsInstance:solve(Obj),
        Handle = cg_prob with mp_vars:Vars,
        (
            foreach(Var, Vars),
            param(Pool, BfsInstance)
        do
            ( var(Var) ->
                BfsInstance:var_get(Var, optimal_val, Val),
                ( var(Val) -> Val = 0 ; true ),
                get_cg_attr(Var, Pool, Attr),
                setarg(mp_val of colgen, Attr, Val)
            ;
                true
            )
        ).

bp_node(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with [
                               master_prob:MP,
                               bfs_tree:BfsPool,
                               idx_lookup:Lookup,
                               shelf:Shelf
                              ],
        \+ \+ ( BfsPool:impose_node_state(other),
                ( cg_get(unsatisfiable_cstrs, [], Pool) -> D = -1 ; D = 0 ),
                setval(cost_dual, D)@Pool,
                cg_get(vars, OldVars, Pool),
                cg_masterproblem(Handle, Pool),
                cg_get(non_zero_vars, Vars, Pool),
                (
                    foreach(Var, OldVars),
                    foreach(Info, OldVals),
                    param(Pool, MP)
                do
                    Pool:var_get(Var, mp_val, Val),
                    lp_var_get(MP, Var, reduced_cost, RC),
                    get_var_bounds(Var, Lo, Hi),
                    Info = cg_var_info with [
                                             lo: Lo,
                                             hi: Hi,
                                             val: Val,
                                             reduced_cost: RC
                                            ]
                ),
                (
                    foreach(Var, Vars),
                    fromto(NewVals, Out, In, []),
                    param(OldVars, Pool, MP)
                do
                    % if it was added to the cg_prob before
                    % this call to cg_masterproblem/2 its
                    % solution info has been found above,
                    % otherwise it must have been generated
                    % in this call, so we have to save its
                    % colgen attr as well as the solution info
                    % to recreate the var after the \+ \+
                    ( var_member(Var, OldVars) ->
                          Out = In
                    ;
                          Pool:var_get(Var, mp_val, Val),
                          lp_var_get(MP, Var, reduced_cost, RC),
                          get_var_bounds(Var, Lo, Hi),
                          get_cg_attr(Var, Pool, Attr),
                          Attr = colgen with [
                                              mp_val:Val,
                                              type:Type,
                                              cost:Cost,
                                              coeffs:Coeffs,
                                              aux:Aux
                                             ],
                          Attr1 = colgen with [
                                               cost:Cost,
                                               coeffs:Coeffs,
                                               aux:Aux
                                              ],
                          Info = cg_var_info with [
                                                   lo: Lo,
                                                   hi: Hi,
                                                   val: Val,
                                                   reduced_cost: RC,
                                                   type: Type,
                                                   attr: Attr1
                                                  ],
                          Out = [Info|In]
                    )
                ),
                Handle = cg_prob with upper_bound:NodeCost,
                shelf_set(Shelf, 1, [NodeCost, OldVals, NewVals])
        ),
        shelf_get(Shelf, 1, [NodeCost, OldVals, NewVals]),
        shelf_set(Shelf, 1, []),
        cg_get(vars, OldVars, Pool),
        (
            foreach(Var, OldVars),
            foreach(Info, OldVals),
            param(BfsPool)
        do
            ( var(Var) ->
                Info = cg_var_info with [
                                         lo: Lo,
                                         hi: Hi,
                                         val: Val,
                                         reduced_cost: RC
                                        ],
                BfsPool:node_info(Var, Lo, Hi, Val, RC)
            ;
                true
            )
        ),
        (
            foreach(Info, NewVals),
            foreach(Var:ObjCol, VarCols),
            foreach(Var:Lo..Hi, NewBounds),
            fromto(NewVars, [Var|Vars], Vars, OldVars),
            param(BfsPool, Lookup, Pool)
        do
            Info = cg_var_info with [
                                     lo: Lo,
                                     hi: Hi,
                                     val: Val,
                                     reduced_cost: RC,
                                     type: Type,
                                     attr: Attr
                                    ],
            get_cg_attr(Var, Pool, Attr),
            Attr = colgen with [cost:Obj, coeffs:Coeffs],
            ( Obj =:= 0 -> ObjCol = Col ; ObjCol = [obj:Obj|Col] ),
            (
                foreach(Ident-V, Coeffs),
                foreach(Id:V, Col),
                param(Lookup)
            do
                hash_get(Lookup, Ident, Id)
            ),
            ( Type == integer ->
                BfsPool:integers(Var), cg_integers(Var, Pool)
            ;
                true
            ),
            BfsPool:node_info(Var, Lo, Hi, Val, RC)
        ),
        setarg(mp_vars of cg_prob, Handle, NewVars),
        lp_add_columns(MP, VarCols),
        (
            foreach(Var:Lo..Hi, NewBounds),
            param(MP)
        do
            lp_var_set_bounds(MP, Var, Lo, Hi)
        ),
        lp_get(MP, vars, VarArr),
        functor(VarArr, _, ColsAdded),
        setarg(mp_cols_added of cg_prob, Handle, ColsAdded),
        BfsPool:node_cost(NodeCost).


var_member(Var, [H|T]) :-
        ( Var == H -> true ; var_member(Var, T) ).

:- tool(cg_solver_setup/3, cg_solver_setup1/4).

cg_solver_setup1(Solver, Obj, Pool, Module) :-
        cg_solver_setup(Solver, Obj, [], Pool, Module).

:- tool(cg_solver_setup/4, cg_solver_setup/5).

cg_solver_setup(SolveSubProblem, Obj, OptionList, Pool, Module) :-
	get_pool_handle(Handle, Pool),
        linearize(Obj, [ConstTerm|LinObj], NonLinObj),
        ( NonLinObj == [] ->
              % the variables to be generated do not appear in the
              % objective, set OptVar in the SPs to 0
              NormObj = LinObj,
              OptVar = 0
        ;
              % the variables to be generated do appear in the
              % objective, but give them a 0 dual val for now
              % in case we need to use two phase
              NonLinObj = [AuxVar = implicit_sum(OptVar)],
              filter_auxvar(AuxVar, LinObj, NormObj),
              var_dual(OptVar, 0, obj, _, 0, Pool)
        ),
        % process option list and fill in defaults
        process_options(OptionList, Handle, SepGoal, EplexOptions),
        fill_in_defaults(Handle),
        SolveSubProblem =.. [_, SPHandle|_],
        SPHandle = sp_prob with [
                                 master_pool:Pool,
                                 cutoff:1e-05,
                                 cost:OptVar,
                                 disallow:[0, []],
                                 
                                 status: phase1,
                                 
                                 coeff_vars:DualVars
                                ],
        Handle = cg_prob with [
                               master_prob:MPHandle,
                               const_obj:ConstTerm,
                               sp_solution_call:SolveSubProblem,
                               phase1_obj:Phase1Obj,
                               upper_bound:1.0Inf,
                               lower_bound: -1.0Inf,
                               mp_vars:Vars,
                               
                               stab_terms:StabTerms,
                               stab_iter_counts:c(1, 1),
                               
                               sep_call:(call(SepGoal)@Module),
                               mp_cols_added:ColsAdded,
                               pool:Pool,
                               idx_lookup:Lookup,
                               shelf:Shelf
                              ],
        hash_create(Lookup),
        shelf_create(info(0), Shelf),
        % create the eplex handle and setup the fixed part of the obj
        ( NormObj = [] ->
              % need a dummy var in the obj fn to force
              % a CPLEX handle to be created properly
              % (or a dummy constraint)
              get_cg_attr(Dummy, Pool, DummyAttr),
              setarg(cost of colgen, DummyAttr, 0),
              setarg(coeffs of colgen, DummyAttr, []),
              setarg(aux of colgen, DummyAttr, artificial),
              MPObj=[ConstTerm, 1*Dummy]
        ;
              MPObj=[ConstTerm|NormObj]
        ),
        lp_setup([], min(sum(MPObj)),
                 EplexOptions,
                 MPHandle),
        lp_set(MPHandle, dual_solution, yes),
        lp_set(MPHandle, keep_basis, yes),
        lp_set(MPHandle, reduced_cost, yes),
        lp_set(MPHandle, slack, yes),
        % collect any constraints only involving known MP vars and add
        collect_typed_pool_constraints(Pool, mp_only of cg_constraint_type,
                                       MPIdxCstrs),
        (
            foreach(MPIdx:MPIdxCstr, MPIdxCstrs),
            foreach(MPIdxCstr1, MPIdxNormCstrs),
            fromto(StabTerms, [StabTerm|Rest],
                   Rest, StabTerms0),
            foreach(MPIdx, MPIdxs)
        do
            MPIdxCstr = Type:[ConstTerm|LinTerms],
            get_cg_attr(Yminus, Pool, AttrM),
            setarg(cost of colgen, AttrM, 0),
            setarg(coeffs of colgen, AttrM, [Idx - -1]),
            setarg(aux of colgen, AttrM, stabilisation),
            get_cg_attr(Yplus, Pool, AttrP),
            setarg(cost of colgen, AttrP, 0),
            setarg(coeffs of colgen, AttrP, [Idx - 1]),
            setarg(aux of colgen, AttrP, stabilisation),
            MPIdxCstr1 = Type:[ConstTerm, -1*Yminus, 1*Yplus|LinTerms],
            StabTerm = stab_term with [
                                       idx: MPIdx,
                                       plus_var: Yplus,
                                       plus_coeff: 0,
                                       plus_bound: 1.0e+3,
                                       minus_var: Yminus,
                                       minus_coeff: 0,
                                       minus_bound: 1.0e+3
                                      ]
        ),
        lp_add_indexed(MPHandle, MPIdxNormCstrs, [], MPIdxs),
        % now add the constraints which involve generated vars and
        % setup the dual_var attributes of the subproblem vars with
        % index of MP constraint
        % note: we add in artificial variables in case the first
        % restricted MP at any node is infeasible
        collect_typed_pool_constraints(Pool, mp_sp of cg_constraint_type,
                                       MPSPNormCstrs),
        (
            foreach([CoeffVar, Idx]:NormCstr, MPSPNormCstrs),
            foreach(StabTerm, StabTerms0),
            foreach(CoeffVar, CoeffVars),
            foreach(Cstr, Cstrs),
            fromto(ArtVars, AVOut, AVIn, []),
            foreach(Idx, Idxs),
            param(Pool)
        do
            StabTerm = stab_term with [
                                       idx:Idx,
                                       plus_var: Yplus,
                                       plus_coeff: 0,
                                       plus_bound: 1.0e+3,
                                       minus_var: Yminus,
                                       minus_coeff: 0,
                                       minus_bound: 1.0e+3
                                      ],
            NormCstr = Type:[ConstTerm|LinTerms],
            ( Type == (=<) ->
                  get_cg_attr(Art, Pool, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - -1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, -1*Art, -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art|AVIn]
            ; Type == (>=) ->
                  get_cg_attr(Art, Pool, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - 1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, 1*Art, -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art|AVIn]
            ; Type == (=:=) ->
                  get_cg_attr(Art1, Pool, Attr1),
                  setarg(cost of colgen, Attr1, 0),
                  setarg(coeffs of colgen, Attr1, [Idx - -1]),
                  setarg(aux of colgen, Attr1, artificial),
                  get_cg_attr(Art2, Pool, Attr2),
                  setarg(cost of colgen, Attr2, 0),
                  setarg(coeffs of colgen, Attr2, [Idx - 1]),
                  setarg(aux of colgen, Attr2, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, -1*Art1, 1*Art2,
                               -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art1, Art2|AVIn]
            )
        ),
        Phase1Obj = min(sum(ArtVars)),
        lp_add_indexed(MPHandle, Cstrs, [], Idxs),
        expand_sp_obj_terms(MPIdxs, Idxs, CoeffVars, Handle),
        % give the known MP vars a colgen attribute
        % and set their objective cost
        % this has to be done after both types of constraints
        % above are added in case there are known MP vars
        % appearing in the type 2 constraints that do not
        % appear in the type 1 constraints
        % however, now the artificial variables have been added
        % to the MP so we have to check if each var has an attribute
        % already and only add attribute/include in vars of cg_prob
        % if it does not
        lp_get(MPHandle, vars, VarArr),
        VarArr =.. [_|AllVars],
        (
            foreach(Var, AllVars),
            fromto(Vars, Out, In, []),
            param(MPHandle, Pool)
        do
            get_cg_attr(Var, Pool, Attr),
            Attr = colgen with [aux:Aux, lo:Lo, hi:Hi],
            % set initial mp eplex bounds
            lp_var_set_bounds(MPHandle, Var, Lo, Hi),
            ( Aux == artificial ->
                % artificial variable
                Out = In
            ; Aux == stabilisation ->
                % stabilisation variable
                Out = In
            ;
                % known MP variable
                Attr = colgen with [cost:0, coeffs:[], aux:[]],
                Out = [Var|In]
            )
        ),
        (
            foreach(Const*Var, NormObj),
            param(Pool)
        do
            get_cg_attr(Var, Pool, Attr),
            setarg(cost of colgen, Attr, Const)
        ),
        % finally add initial SP solution column set to MP
        cg_new_MP_columns(VarCols, Pool),
        ( VarCols == [] ->
              % no initial solution columns
              true
        ;
              % add initial solution columns to MP
              lp_add_columns(MPHandle, VarCols),
              (
                  foreach(Var:_, VarCols),
                  param(MPHandle)
              do
                  get_var_bounds(Var, Lo, Hi),
                  lp_var_set_bounds(MPHandle, Var, Lo, Hi)
              )
        ),
        % record the total number of columns now in the mp
        lp_get(MPHandle, vars, MPVarArr),
        functor(MPVarArr, _, ColsAdded),
        (
            foreach(Var, ArtVars)
        do
            set_var_bounds(Var, 0, 1.0Inf)
        ),
        (
            foreach(StabTerm, StabTerms),
            param(MPHandle)
        do
            StabTerm = stab_term with [
                                       plus_var: Yplus,
                                       plus_bound: Boundplus,
                                       minus_var: Yminus,
                                       minus_bound: Boundminus
                                      ],
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus),
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus)
        ),
        ( NormObj = [] -> Dummy = 0 ; true ),
        setval(cost_dual, 0)@Pool,
        /*
        lp_probe(MPHandle, Phase1Obj, Phase1ObjVal),
        ( Phase1ObjVal =:= 0 ->
              % feasible initial prob,
              % start phase 2
              setval(cost_dual, -1)@Pool
        ;
              % infeasible initial prob,
              % need to start phase 1
              setval(cost_dual, 0)@Pool
        ),
        lp_set(MPHandle, basis, []),
        */
        % make a suspension for the MP iterator
        % and insert it in the suspension list of Handle
        make_suspension(cg_masterproblem(Handle, Pool), 7, MPSusp),
        enter_suspension_list(mp_susp of cg_prob, Handle, MPSusp),
        % make a suspension for the SP iterator
        % and insert it in the suspension lists of the dual val vars
        make_suspension(solveSPs(Handle), 6, SPSusp),
        insert_suspension([OptVar|DualVars], SPSusp, susps of dual_var,
                          dual_var).

collect_new_solver_rowcols(Pool) :-
        % collect any new constraints
        collect_typed_pool_constraints(Pool, mp_only of cg_constraint_type,
                                       MPIdxCstrs),
        collect_typed_pool_constraints(Pool, mp_sp of cg_constraint_type,
                                       MPSPNormCstrs),
        ( MPIdxCstrs = [_|_] -> true
        ; MPSPNormCstrs = [_|_] ),
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with [
                               master_prob:MPHandle,
                               phase1_obj:Phase1Obj,
                               mp_vars:OldVars,
                               stab_terms:OldStabTerms,
                               mp_cols_added:ColsAdded,
                               pool:Pool
                              ],
        % add any constraints only involving known MP vars
        (
            foreach(MPIdx:MPIdxCstr, MPIdxCstrs),
            foreach(MPIdxCstr1, MPIdxNormCstrs),
            fromto(StabTerms, [StabTerm|Rest],
                   Rest, StabTerms0),
            fromto(NewStabTerms, [StabTerm|Rest0],
                   Rest0, StabTerms00),
            foreach(MPIdx, MPIdxs)
        do
            MPIdxCstr = Type:[ConstTerm|LinTerms],
            get_cg_attr(Yminus, Pool, AttrM),
            setarg(cost of colgen, AttrM, 0),
            setarg(coeffs of colgen, AttrM, [Idx - -1]),
            setarg(aux of colgen, AttrM, stabilisation),
            get_cg_attr(Yplus, Pool, AttrP),
            setarg(cost of colgen, AttrP, 0),
            setarg(coeffs of colgen, AttrP, [Idx - 1]),
            setarg(aux of colgen, AttrP, stabilisation),
            MPIdxCstr1 = Type:[ConstTerm, -1*Yminus, 1*Yplus|LinTerms],
            StabTerm = stab_term with [
                                       idx: MPIdx,
                                       plus_var: Yplus,
                                       plus_coeff: 0,
                                       plus_bound: 1.0e+3,
                                       minus_var: Yminus,
                                       minus_coeff: 0,
                                       minus_bound: 1.0e+3
                                      ]
        ),
        lp_add_indexed(MPHandle, MPIdxNormCstrs, [], MPIdxs),
        % now add the constraints which involve generated vars and
        % setup the dual_var attributes of the subproblem vars with
        % index of MP constraint
        % note: we add in artificial variables in case the first
        % restricted MP at any node is infeasible
        Phase1Obj = min(sum(OldArtVars)),
        (
            foreach([CoeffVar, Idx]:NormCstr, MPSPNormCstrs),
            fromto(StabTerms0, [StabTerm|Rest],
                   Rest, OldStabTerms),
            foreach(StabTerm, StabTerms00),
            foreach(CoeffVar, CoeffVars),
            foreach(Cstr, Cstrs),
            fromto(NewArtVars, NAVOut, NAVIn, []),
            fromto(ArtVars, AVOut, AVIn, OldArtVars),
            foreach(Idx, Idxs),
            param(Pool)
        do
            StabTerm = stab_term with [
                                       idx:Idx,
                                       plus_var: Yplus,
                                       plus_coeff: 0,
                                       plus_bound: 1.0e+3,
                                       minus_var: Yminus,
                                       minus_coeff: 0,
                                       minus_bound: 1.0e+3
                                      ],
            NormCstr = Type:[ConstTerm|LinTerms],
            ( Type == (=<) ->
                  get_cg_attr(Art, Pool, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - -1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, -1*Art, -1*Yminus, 1*Yplus|LinTerms],
                  NAVOut = [Art|NAVIn],
                  AVOut = [Art|AVIn]
            ; Type == (>=) ->
                  get_cg_attr(Art, Pool, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - 1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, 1*Art, -1*Yminus, 1*Yplus|LinTerms],
                  NAVOut = [Art|NAVIn],
                  AVOut = [Art|AVIn]
            ; Type == (=:=) ->
                  get_cg_attr(Art1, Pool, Attr1),
                  setarg(cost of colgen, Attr1, 0),
                  setarg(coeffs of colgen, Attr1, [Idx - -1]),
                  setarg(aux of colgen, Attr1, artificial),
                  get_cg_attr(Art2, Pool, Attr2),
                  setarg(cost of colgen, Attr2, 0),
                  setarg(coeffs of colgen, Attr2, [Idx - 1]),
                  setarg(aux of colgen, Attr2, artificial),
                  get_cg_attr(Yminus, Pool, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Pool, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  Cstr = Type:[ConstTerm, -1*Art1, 1*Art2,
                               -1*Yminus, 1*Yplus|LinTerms],
                  NAVOut = [Art1, Art2|NAVIn],
                  AVOut = [Art1, Art2|AVIn]
            )
        ),
        NewPhase1Obj = min(sum(ArtVars)),
        setarg(phase1_obj of cg_prob, Handle, NewPhase1Obj),
        setarg(stab_terms of cg_prob, Handle, StabTerms),
        lp_add_indexed(MPHandle, Cstrs, [], Idxs),
        expand_sp_obj_terms(MPIdxs, Idxs, CoeffVars, Handle),
        % give the known MP vars a colgen attribute
        % and set their objective cost
        % this has to be done after both types of constraints
        % above are added in case there are known MP vars
        % appearing in the type 2 constraints that do not
        % appear in the type 1 constraints
        % however, now the artificial variables have been added
        % to the MP so we have to check if each var has an attribute
        % already and only add attribute/include in vars of cg_prob
        % if it does not
        lp_get(MPHandle, vars, VarArr),
        VarArr =.. [_|AllVars],
        (
            foreach(Var, AllVars),
            fromto(Vars, Out, In, OldVars),
            param(MPHandle, Pool, OldVars)
        do
            ( var_member(Var, OldVars) ->
                Out = In
            ;
                get_cg_attr(Var, Pool, Attr),
                Attr = colgen with [aux:Aux, lo:Lo, hi:Hi],
                % set initial mp eplex bounds
                lp_var_set_bounds(MPHandle, Var, Lo, Hi),
                ( Aux == artificial ->
                    % artificial variable
                    Out = In
                ; Aux == stabilisation ->
                    % stabilisation variable
                    Out = In
                ; Attr = colgen with [cost:0, coeffs:[], aux:[]] ->
                    % new known MP variable
                    Out = [Var|In]
                ;
                    % extisting generated variable
                    Out = In
                )
            )
        ),
        setarg(mp_vars of cg_prob, Handle, Vars),
        % finally add initial SP solution column set to MP
        cg_new_MP_columns(VarCols, Pool),
        ( VarCols == [] ->
              % no initial solution columns
              true
        ;
              % add initial solution columns to MP
              lp_add_columns(MPHandle, VarCols),
              (
                  foreach(Var:_, VarCols),
                  param(MPHandle)
              do
                  get_var_bounds(Var, Lo, Hi),
                  lp_var_set_bounds(MPHandle, Var, Lo, Hi)
              )
        ),
        % record the total number of columns now in the mp
        lp_get(MPHandle, vars, MPVarArr),
        functor(MPVarArr, _, NewColsAdded),
        MPVarArr =.. [_|NewMPVars],
        setarg(mp_vars of cg_prob, Handle, NewMPVars),
        setarg(mp_cols_added of cg_prob, Handle, NewColsAdded),
        % extend the basis of the node lp (if present) for restart
        ( lp_get(MPHandle, basis, Basis) ->
            ExtraCols is NewColsAdded-ColsAdded,
            extend_base(Basis, ColsAdded, ExtraCols, NewBasis),
            lp_set(MPHandle, basis, NewBasis)
        ;
            true
        ),
        (
            foreach(Var, NewArtVars)
        do
            set_var_bounds(Var, 0, 1.0Inf)
        ),
        (
            foreach(StabTerm, NewStabTerms),
            param(MPHandle)
        do
            StabTerm = stab_term with [
                                       plus_var: Yplus,
                                       plus_bound: Boundplus,
                                       minus_var: Yminus,
                                       minus_bound: Boundminus
                                      ],
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus),
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus)
        ),
        % insert the SP iterator suspension in the suspension lists of
        % the dual val vars
        make_suspension(solveSPs(Handle), 6, SPSusp),
        insert_suspension(CoeffVars, SPSusp, susps of dual_var,
                          dual_var).        
collect_new_solver_rowcols(_Pool).

expand_sp_obj_terms([], [], [], _Handle) :- !.
expand_sp_obj_terms(MPIdxs, Idxs, CoeffVars, Handle) :-
        ( MPIdxs = [] ->
            sort(0, >, Idxs, [NCstrs|_])
        ; Idxs = [] ->
            sort(0, >, MPIdxs, [NCstrs|_])
        ;
            sort(0, >, MPIdxs, [MaxMPIdx|_]),
            sort(0, >, Idxs, [MaxIdx|_]),
            NCstrs is max(MaxMPIdx, MaxIdx)
        ),
        NCstrs1 is NCstrs + 1,
        dim(ExprArr, [NCstrs1]),
        Handle = cg_prob with sp_obj_terms:OldExprArr,
        ( var(OldExprArr) ->
            OldExprArr = ExprArr
        ;
            dim(OldExprArr, [OldNCstrs]),
            NCstrs1 > OldNCstrs,
            (
                for(Idx, 1, OldNCstrs),
                param(OldExprArr, ExprArr)
            do
                arg(Idx, OldExprArr, Arg),
                arg(Idx, ExprArr, Arg)
            ),
            setarg(sp_obj_terms of cg_prob, Handle, ExprArr)
        ),
        (
            foreach(CoeffVar, CoeffVars),
            foreach(Idx, Idxs),
            param(ExprArr)
        do
            Idx1 is Idx + 1,
            arg(Idx1, ExprArr, CoeffVar)
        ),
        (
            foreach(MPIdx, MPIdxs),
            param(ExprArr)
        do
            MPIdx1 is MPIdx + 1,
            arg(MPIdx1, ExprArr, 0)
        ).

process_options([], _, Separation, []) :- !,
        ( var(Separation) -> Separation = true ; true ).
process_options([O|Os], Handle, Separation, EplexOptions) :- !,
	process_option(O, Handle, Separation, EplexOptions, EplexOptions0),
	process_options(Os, Handle, Separation, EplexOptions0).
process_options(_NonList, Handle, _, _) :-
        Handle = cg_prob with pool:Pool,
        cg_info_message(Handle, "%w : options not proper list."
                        " Ignored.%n,", [Pool]).

process_option(Option, Handle, Separation, EplexOptions, EplexOptions0) :-
        Handle = cg_prob with pool:Pool,
        ( var(Option) ->
            error(4, process_option(Option, Handle, Separation,
                                    EplexOptions, EplexOptions0))
        ; Option = separate(Separation) ->
            ( var(Separation) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ;
                EplexOptions = EplexOptions0
            )
        ; Option = node_select(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ;
                ( Handle = cg_prob with bfs_tree:[] ->
                    concat_string([Pool, '_bfs'], BfsInstanceName),
                    atom_string(BfsInstance, BfsInstanceName),
                    bfs_instance(BfsInstance),
                    Handle = cg_prob with info_messages:OnOff,
                    BfsInstance:solver_setup(min, bp_node(Pool),
                                             [info_messages:OnOff,
                                              node_select(Val),
                                              separation(bp_separate(Pool))]),
                    setarg(bfs_tree of cg_prob, Handle, BfsInstance)
                ;
                    Handle = cg_prob with bfs_tree:BfsInstance,
                    BfsInstance:set(node_select, Val)
                ),
                EplexOptions = EplexOptions0
            )
        ; Option = int_tolerance(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; float(Val) ->
                ( 0 =< Val, Val =< 0.5 ->
                    cg_set1(Pool, int_tolerance, Val),
                    EplexOptions = EplexOptions0
                ; error(6, process_option(Option, Handle, Separation,
                                          EplexOptions, EplexOptions0))
                    %cg_info_message(Handle,
                    %                "Invalid int_tolerance (ignored): %w%n",
                    %                [Val]),
                    %EplexOptions = EplexOptions0
                )
            ; error(5, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; Option = disallow(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; atom(Val) ->
                ( valid_disallow_setting(Val) ->
                    cg_set1(Pool, disallow, Val),
                    EplexOptions = EplexOptions0
                ; error(6, process_option(Option, Handle, Separation,
                                          EplexOptions, EplexOptions0))

                )
            ; error(5, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; Option = info_messages(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; atom(Val) ->
                ( valid_setting(Val) ->
                    cg_set1(Pool, info_messages, Val),
                    EplexOptions = EplexOptions0
                ; error(6, process_option(Option, Handle, Separation,
                                          EplexOptions, EplexOptions0))

                )
            ; error(5, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; Option = on_degeneracy(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; atom(Val) ->
                ( valid_degeneracy_setting(Val) ->
                    cg_set1(Pool, on_degeneracy, Val),
                    EplexOptions = EplexOptions0
                ; error(6, process_option(Option, Handle, Separation,
                                          EplexOptions, EplexOptions0))
                )
            ; error(5, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; Option = stabilisation(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; valid_stabilisation_setting(Val) ->
                cg_set1(Pool, stabilisation, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; Option = basis_perturbation(Val) ->
            ( var(Val) ->
                error(4, process_option(Option, Handle, Separation,
                                        EplexOptions, EplexOptions0))
            ; atom(Val) ->
                ( valid_setting(Val) ->
                    cg_set1(Pool, basis_perturbation, Val),
                    EplexOptions = EplexOptions0
                ; error(6, process_option(Option, Handle, Separation,
                                          EplexOptions, EplexOptions0))
                )
            ; error(5, process_option(Option, Handle, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; % not a colgen option, pass to master problem eplex solver
            printf("Not a colgen option (passed to MP eplex solver): %w%n",
                   [Option]),
            EplexOptions = [Option|EplexOptions0]
        ).

valid_degeneracy_setting(continue).
valid_degeneracy_setting(stop).

valid_stabilisation_setting(OnTerm) :-
        OnTerm = on(BoundIter, BoundUpdate, CoeffIter, CoeffUpdate),
        integer(BoundIter),
        BoundIter >= 1,
        number(BoundUpdate),
        BoundUpdate > 0,
        integer(CoeffIter),
        CoeffIter >= 1,
        number(CoeffUpdate),
        CoeffUpdate > 0.
valid_stabilisation_setting(off).
valid_stabilisation_setting(stab_pred(_, _)).

valid_disallow_setting(clp).
valid_disallow_setting(lp).
valid_disallow_setting(off).

valid_setting(on).
valid_setting(off).

fill_in_defaults(Handle) :-
        Handle = cg_prob with [
                               disallow:DisOnOff,
                               tolerance:(Tol, BranchTol),
                               info_messages:MessageOnOff,
                               on_degeneracy:OnDegeneracy,
                               stabilisation:Stabilisation,
                               basis_perturbation:PerturbOnOff
                              ],
        ( var(DisOnOff) -> DisOnOff = off ; true ),
        ( var(Tol) -> Tol = 1e-05 ; true ),
        ( var(BranchTol) -> BranchTol = 0 ; true ),
        ( var(MessageOnOff) -> MessageOnOff = off ; true ),
        ( var(OnDegeneracy) -> OnDegeneracy = stop ; true ),
        ( var(Stabilisation) -> Stabilisation = off ; true ),
        ( var(PerturbOnOff) -> PerturbOnOff = off ; true ).

% ----------------------------------------------------------------------
% most general instantiation templates to disallow duplicate columns
% ----------------------------------------------------------------------

% repeatedly amlagamate a new coefficient list with
% the coefficient list of a member of a list of
% linearisation-coefficient list pairs, extract
% linearisation of new most general amalgamation
% and return new linearisation-coefficient list pair list
lin_amalgamate(N, List, Coeffs, Vars, NewN, [[N1, Cstrs]-NewCoeffs|NewList]) :-
    % try to amalgamate Coeffs with others in list
    % repeatedly until we have most general disallow template
    (
	fromto(no, _, Done, yes),
	fromto(Coeffs, CIn, COut, NewCoeffs),
	fromto(N, NIn, NOut, N0),
	fromto(List, LIn, LOut, NewList)
    do
	lin_amalgamate(LIn, CIn, NIn, LOut, COut, NOut, Done)
    ),
    % linearise new template
    linearize_template(Vars, NewCoeffs, 1, N1, LCstrs, Flags, Done),
    ( Done = yes -> NewN = N0, N1 = 0, Cstrs = []
    ; NewN is N0+N1, Cstrs = [(>=):[-1*1|Flags]|LCstrs] ).

linearize_template([], [], N, N, [], [], no).
linearize_template([Var|Vars], [Coeff|Coeffs], N0, N, Cstrs, Flags, Done) :-
    get_var_bounds(Var, Lo, Hi),
    ( Coeff = [_|_] -> CList = Coeff ; CList = [Coeff] ),
    lin_not_in(CList, Var, Lo, Hi, N0, N1, Cstrs, Cstrs1, Flags, Flags1, Done),
    ( Done == yes ->         % Var cannot match, no Cstrs needed
	true
    ;
	linearize_template(Vars, Coeffs, N1, N, Cstrs1, Flags1, Done)
    ).

lin_not_in([], _Var, _VLo, _VHi, _N, _N1, _Cstrs, _Cstrs1, _Flags, _Flags1, yes).
lin_not_in([H|T], Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1, Done) :-
    ( integer(H) -> Lo = H, Hi = H ; H = Lo..Hi ),
    ( VHi < Lo ->            % cannot be in this or later ranges
	Done = yes
    ; VLo < Lo, VHi =< Hi -> % may be in this range, cannot be in later
	% Var =< VHi-(VHi+1-Lo)*Flag
	Alpha is fix(VHi+1-Lo),
	Beta is fix(-1*VHi),
	N1 is N+1,
	Cstrs = [(=<):[Beta*1, 1*Var, Alpha*Flag]|Cstrs1],
	Flags = [1*Flag|Flags1],
	Flag::0..1
    ; VHi =< Hi ->           % is in this range, no cstr for this var
	N = N1,
	Cstrs = Cstrs1,
	Flags = Flags1
    ; VLo =< VHi ->          % may be in this or later but cannot be earlier
	lin_not_in1(T, Hi, Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1)
    ;                        % may be in later ranges but cannot be in this
	lin_not_in(T, Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1, Done)
    ).

lin_not_in1([], Hi, Var, VLo, _VHi, N, N1,
	    [(>=):[Delta*1,1*Var,Gamma*Flag]|Cstrs1], Cstrs1,
	    [1*Flag|Flags1], Flags1) :-
    % Var >= VLo+(Hi+1-VLo)*Flag
    N1 is N+1,
    Flag::0..1,
    Gamma is fix(VLo-1-Hi),
    Delta is fix(-1*VLo).
lin_not_in1([H|T], Hi, Var, VLo, VHi, N, N1,
	    [(>=):[Delta*1,1*Var,Gamma*Flag],
	     (=<):[Beta*1,1*Var,Alpha*Flag]|Cstrs], Cstrs1,
	    [1*Flag|Flags], Flags1) :-
    % Var >= VLo+(Hi+1-VLo)*Flag
    % Var =< VHi-(VHi+1-Lo1)*Flag
    ( integer(H) -> Lo1 = H, Hi1 = H ; H = Lo1..Hi1 ),
    N0 is N+2,
    Flag::0..1,
    Alpha is fix(VHi+1-Lo1),
    Beta is fix(-1*VHi),
    Gamma is fix(VLo-1-Hi),
    Delta is fix(-1*VLo),
    lin_not_in1(T, Hi1, Var, VLo, VHi, N0, N1, Cstrs, Cstrs1, Flags, Flags1).

% amalgamate a coefficient list with the coefficient
% list of a member of a list of linearisation-coefficient
% list pairs and return remaining list and amalgamated coefficients
lin_amalgamate([], Coeffs, N, [], Coeffs, N, yes).
lin_amalgamate([[L, Cstrs]-Coeffs|List], NewCoeffs, N,
	       NewList, NewCoeffs1, NewN, Done) :-
    ( amalg(NewCoeffs, Coeffs, 0, 0, NewCoeffs1) ->
	% new coeff list can be amalgamated with this one
	% to create a more general disallow template
	% return rest of list and new template
	NewList = List,
	NewN is N-L,
	Done = no
    ;
	% could not be amalgamated
	NewList = [[L, Cstrs]-Coeffs|NewList1],
	lin_amalgamate(List, NewCoeffs, N, NewList1, NewCoeffs1, NewN, Done)
    ).

% repeatedly amlagamate a new coefficient list with
% the coefficient list of a member of a list of
% suspension-coefficient list pairs killing any
% associated suspesnions, suspend disallow demons for
% the most general amalgamation and return new
% suspension-coefficient list pair list
amalgamate(N, List, Coeffs, Vars, NewN, [[N1, Susps]-NewCoeffs|NewList]) :-
    % try to amalgamate Coeffs with others in list
    % repeatedly until we have most general disallow template
    (
	fromto(no, _, Done, yes),
	fromto(Coeffs, CIn, COut, NewCoeffs),
	fromto(N, NIn, NOut, N0),
	fromto(List, LIn, LOut, NewList)
    do
	amalgamate(LIn, CIn, NIn, LOut, COut, NOut, Done)
    ),
    % suspend disallow demons for new template
    (
	foreach(Var, Vars),
	foreach(Coeff, NewCoeffs),
	foreach(Flag, AndFlags),
	foreach(Susp, AndSusps),
	count(_,2,NSusps),
	param(OrFlag)
    do
	( Coeff = [_|_] -> ListTerm = l(Coeff) ; ListTerm = l([Coeff]) ),
	suspend(not_among(Var, ListTerm, Flag, OrFlag, Susp), 4, [Var->constrained, [Flag, OrFlag]->inst], Susp),
	not_among(Var, ListTerm, Flag, OrFlag, Susp)
    ),
    ( AndFlags = [AndFlag] ->
	AndFlag = 1,
	NewN is N0+1,
	N1 = 1,
	Susps = AndSusps
    ;
	NewN is N0+NSusps,
	N1 = NSusps,
	Susps = [OrSusp|AndSusps],
	AndFlagTerm = l(AndFlags),
	suspend(or(OrFlag, AndFlagTerm, OrSusp), 3, [OrFlag|AndFlags]->inst, OrSusp),
	or(OrFlag, AndFlagTerm, OrSusp)
    ).

% amalgamate a coefficient list with the coefficient list of
% a member of a list of suspension-coefficient list pairs
% killing any associated suspesnions and return remaining
% list and amalgamated coefficients
amalgamate([], Coeffs, N, [], Coeffs, N, yes).
amalgamate([[L, Susps]-Coeffs|List], NewCoeffs, N,
	   NewList, NewCoeffs1, NewN, Done) :-
    ( amalg(NewCoeffs, Coeffs, 0, 0, NewCoeffs1) ->
	% new coeff list can be amalgamated with this one
	% to create a more general disallow template
	% kill the suspension for this disallow goal
	% return rest of list and new template
	(
	    foreach(Susp, Susps)
	do
	    kill_suspension(Susp)
	),
	NewList = List,
	NewN is N-L,
	Done = no
    ;
	% could not be amalgamated
	NewList = [[L, Susps]-Coeffs|NewList1],
	amalgamate(List, NewCoeffs, N, NewList1, NewCoeffs1, NewN, Done)
    ).

% amalgamate two coefficient lists to create a _most general amalgamation_
% succeed if they are pairwise disjoint at exactly one coeff and pairwise indentical at all others
% abort if they are equal/intersecting at all coeffs (since we have a duplicate coeff list which should have been disallowed)
% fail otherwise
amalg([], [], D, I, []) :-
    ( D = 0 -> printf("lists not disjoint in amalg/5%n", []),
               flush(output), abort
    ; D = 1, I = 0 ).
amalg([A|As], [B|Bs], D, I, Amalg) :-
    ( A = B ->
	% pairwise identical here
	D1 = D, I1 = I, Amalg = [A|Amalg1]
    ;
	% otherwise make both coeff templates into
	% disallowed domain lists
	% and check for disjointness/intersection
	( A = [_|_] -> List1 = A ; List1 = [A] ),
	( B = [_|_] -> List2 = B ; List2 = [B] ),
	amalg(List1, List2, D, D1, I, I1, List3), Amalg = [List3|Amalg1]
    ),
    amalg(As, Bs, D1, I1, Amalg1).

amalg(List1, List2, D, D1, I, I1, Amalg) :-
    amalg1(List1, List2, D, D1, I, I1, List3),
    ( List3 = [Val] -> Amalg = Val
    ; Amalg = List3 ).

amalg1([], List, 0, 1, 0, 0, List) :- !.
amalg1(List, [], 0, 1, 0, 0, List) :- !.
amalg1([H1|T1], [H2|T2], D, D1, I, I1, Amalg) :-
    ( integer(H1) -> Lo1 = H1, Hi1 = H1 ; H1 = Lo1..Hi1 ),
    ( integer(H2) -> Lo2 = H2, Hi2 = H2 ; H2 = Lo2..Hi2 ),
    ( overlapping_ranges(Lo1, Hi1, Lo2, Hi2) ->
	% lists intersect here,
	% fail immediately if already disjoint
	% otherwise succeed with
	% no need to continue amalgamating
	D = 0, D1 = 0, I1 = 1
    ;
	% no intersection between H1 and H2,
	% continue with T1 and T2,
	% amalgamating H1 and H2 if necessary
	( Lo1 is Hi2 + 1 -> T3 = [Lo2..Hi1|T1], T4 = T2, Amalg = Amalg1
	; Lo1 > Hi2 + 1 -> T3 = [H1|T1], T4 = T2, Amalg = [H2|Amalg1]
	; Lo2 is Hi1 + 1 -> T3 = T1, T4 = [Lo1..Hi2|T2], Amalg = Amalg1
	; T3 = T1, T4 = [H2|T2], Amalg = [H1|Amalg1] ),
	amalg1(T3, T4, D, D1, I, I1, Amalg1)
    ).

overlapping_ranges(Lo1, Hi1, Lo2, Hi2) :-
    (Lo1 >= Lo2, Lo1 =< Hi2) ; (Lo2 >= Lo1, Lo2 =< Hi1).

% suspend this on [OrFlag|AndFlags]->inst
:- demon or/3.
or(OrFlag, AndFlagTerm, Susp) :-
    ( OrFlag == 1 ->
	kill_suspension(Susp)
    ;
	% an AndFlag was set to 0
	AndFlagTerm =.. [_, AndFlags],
	(
	    foreach(AndFlag, AndFlags),
	    fromto(NewFlags, Out, In, [])
	do
	    ( AndFlag == 0 -> Out = In ; Out = [AndFlag|In] )
	),
	( NewFlags = [AndFlag] -> AndFlag = 1, kill_suspension(Susp)
	; setarg(1, AndFlagTerm, NewFlags) )
    ).

% suspend this on Var->any, [Flag, OrFlag]->inst
:- demon not_among/5.
not_among(Var, ListTerm, Flag, OrFlag, Susp) :-
    ( OrFlag == 1 ->
	% some other variable has been
	% instantiated to a non-matching
	% value, done
	kill_suspension(Susp)
    ;
	% change in domain of Var
	% or all other variables have been
	% instantiated to matching values,
	% Var cannot match ListTerm
	get_var_bounds(Var, Lo, Hi),
	ListTerm =.. [_, List],
	not_among_body(List, Lo, Hi, NewList, Flag),
	( Flag == 0 ->
	    % matches the list, done
	    kill_suspension(Susp)
	; NewList = [] ->
	    % does not match, done
	    OrFlag = 1, Flag = 1, kill_suspension(Susp)
	; Flag == 1 ->
	    % all other variables have been
	    % instantiated to matching values,
	    % Var cannot match ListTerm

	    % is there a way to remove intervals from ic var domains?

	    % for now

	    % remove the top and bottom overlaps
	    % if it can be done without creating holes
	    NewList = [H|Rest],
	    ( integer(H) -> Lo1 = H, Hi1 = H ; H = Lo1..Hi1 ),
	    ( Lo1 =< Lo -> NewLo is Hi1 + 1 ; NewLo = Lo ),
	    ( Rest = [] -> NewHi = Hi
	    ;
		( fromto(Rest, [_|Out], Out, [T]) do true ),
		( integer(T) -> Lo2 = T, Hi2 = T ; T = Lo2..Hi2 ),
		( Hi2 >= Hi -> NewHi is Lo2 - 1 ; NewHi = Hi )
	    ),
	    set_var_bounds(Var, NewLo, NewHi),
	    setarg(1, ListTerm, NewList)
	; setarg(1, ListTerm, NewList) )
    ).

% not_among_body(+RangeList, +VLo, +VHi, ?List, ?Flag)
% succeeds if VLo..VHi is a subset of a member of RangeList and Flag = 0
% or if List is the intersection of VLo..VHi and RangeList
not_among_body([], _VLo, _VHi, [], _Flag).
not_among_body([H|T], VLo, VHi, List, Flag) :-
    ( integer(H) -> Lo = H, Hi = H ; H = Lo..Hi ),
    ( VHi < Lo ->            % cannot be in this or later ranges
	List = []
    ; VLo < Lo, VHi =< Hi -> % may be in this range, cannot be in later
	List = [H]
    ; VHi =< Hi ->           % is in this range, set flag false
	Flag = 0
    ;                        % may be in this and/or later ranges
	( VLo =< Hi -> List = [H|List1] ; List = List1 ),
	not_among_body(T, VLo, VHi, List1, Flag)
    ).

cg_new_MP_columns(VarCols, Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with [
                               sp_solution_call:SolveSubProblem,
                               disallow:DisOnOff,
                               mp_vars:OldVars,
                               idx_lookup:Lookup
                              ],
        SolveSubProblem =.. [_, SPHandle|_],
        SPHandle = sp_prob with [
                                 coeff_vars:DualVars,
                                 disallow:[Count, Templates]
                                ],
        getval(sp_solns(0), Solns)@Pool,
        setval(sp_solns(0), [])@Pool,
        (
            foreach(Soln, Solns),
            fromto(VarCols, [Var:ObjCol|Rest], Rest, []),
            fromto(NewVars, [Var|Vars], Vars, OldVars),

            % most general inst templates for disallowing cols
            % for thesis results tables
            fromto(Count, CountIn, CountOut, Count0),
            fromto(Templates, TemplatesIn, TemplatesOut, NewTemplates),
            param(DisOnOff),
            
            param(Pool, DualVars, Lookup)
        do
            Soln = sp_sol with [
                                cost:Obj,
                                coeff_vars:Coeffs,
                                aux:Info,
                                lo:Lo,
                                hi:Hi,
                                type:Type
                               ],
            ( Obj =:= 0 -> ObjCol = Col ; ObjCol = [obj:Obj|Col] ),
            get_cg_attr(Var, Pool, Attr),
            Attr = colgen with [cost:Obj, coeffs:HashCol, aux:Info],
            ( Lo = 0 -> true
            ; Lo =:= 0 -> true
            ; setarg(lo of colgen, Attr, Lo) ),
            ( Hi = 1.0Inf -> true
            ; Hi =:= 1.0Inf -> true
            ; setarg(hi of colgen, Attr, Hi) ),
            ( Type == integer -> setarg(type of colgen, Attr, integer)
            ; true ),
            set_var_bounds(Var, Lo, Hi),
            ( Coeffs = [_Id-_V|_] ->

                  % most general inst templates for disallowing cols
                  % for thesis results tables
                  ( DisOnOff == off ->
                        CountOut = CountIn, TemplatesOut = TemplatesIn
                  ;
                        (
                            foreach(Var, DualVars),
                            foreach(TVal, TCoeffs),
                            param(Pool, Coeffs)
                        do
                            get_idx(Var, Id, Pool),
                            ( member(Id-TVal, Coeffs) -> true
                            ; TVal = 0 )
                        ),
                        ( DisOnOff == lp ->
                              lin_amalgamate(CountIn, TemplatesIn,
                                             TCoeffs, DualVars,
                                             CountOut, TemplatesOut)
                        ; DisOnOff == clp ->
                              amalgamate(CountIn, TemplatesIn,
                                         TCoeffs, DualVars,
                                         CountOut, TemplatesOut)
                        )
                  ),

                  (
                      foreach(Id-V, Coeffs),
                      foreach(I:V, Col1),
                      param(Lookup)
                  do
                      hash_get(Lookup, Id, I)
                  ),
                  keysort(Coeffs, HashCol)
            ;

                  % most general inst templates for disallowing cols
                  % for thesis results tables
                  ( DisOnOff == off ->
                        CountOut = CountIn, TemplatesOut = TemplatesIn
                  ;
                        ( DisOnOff == lp ->
                              lin_amalgamate(CountIn, TemplatesIn,
                                             Coeffs, DualVars,
                                             CountOut, TemplatesOut)
                        ; DisOnOff == clp ->
                              amalgamate(CountIn, TemplatesIn,
                                         Coeffs, DualVars,
                                         CountOut, TemplatesOut)
                        )
                  ),

                  (
                      foreach(Var, DualVars),
                      foreach(V, Coeffs),
                      fromto(HC, HCOut, HCIn, []),
                      fromto(Col1, Out, In, []),
                      param(Pool, Lookup)
                  do
                      (V =:= 0 ->
                           HCOut = HCIn,
                           Out = In
                      ;
                           get_idx(Var, Id, Pool),
                           HCOut = [Id-V|HCIn],
                           hash_get(Lookup, Id, I),
                           Out = [I:V|In]
                      )
                  ),
                  keysort(HC, HashCol)
            ),
            keysort(Col1, Col)
        ),

        % most general inst templates for disallowing cols
        % for thesis results tables
        NewCount is max(Count, Count0),
        setarg(disallow of sp_prob,SPHandle, [NewCount, NewTemplates]),
                                 
        setarg(mp_vars of cg_prob, Handle, NewVars).

:- demon cg_masterproblem/2.
cg_masterproblem(Handle, Pool) :-
        collect_new_solver_rowcols(Pool),
        % solve master problem and generate columns
        setval(sp_rc_sum, none)@Pool,
        Handle = cg_prob with [
                               master_prob:MP,
                               phase1_obj:Phase1Fn,
                               stabilisation:Stabilisation,
                               stab_terms:StabTerms,
                               sp_solution_call:SolveSubProblem,
                               mp_vars:Vars,
                               mp_cols_added:ColsAdded,
                               idx_lookup:Lookup,
                               tolerance:(Tolerance, _BranchTolerance),
                               lower_bound:LowerBound
                              ],
        cg_info_message(Handle, "%tSolving mp relaxation ... ", []),
        ( lp_get(MP, basis, StartBasis) -> true ; StartBasis = [] ),
        getval(cost_dual, CostDual)@Pool,
        % solve the new MP
        ( CostDual = 0 ->
              % in phase1 of two-phase method
              ( phase_change(Phase1Fn),
                lp_get(MP, objective, ObjExpr),
                ( Stabilisation == off ->
                    (
                        foreach(StabTerm, StabTerms)
                    do
                        StabTerm = stab_term with [
                                                   plus_var: 0.0,
                                                   minus_var: 0.0
                                                  ]
                    ),
                    StabExpr = ObjExpr
                ;
                    ObjExpr =.. [Sense, Expr],
                    (
                        foreach(StabTerm, StabTerms),
                        fromto(SExpr, Out, In, Expr)
                    do
                        StabTerm = stab_term with [
                                                   plus_var: Yplus,
                                                   plus_coeff: CoeffPlus,
                                                   minus_var: Yminus,
                                                   minus_coeff: CoeffMinus
                                                  ],
                        Out = CoeffPlus*Yplus - CoeffMinus*Yminus + In
                    ),
                    StabExpr =.. [Sense, SExpr]
                ),
                lp_probe(MP, StabExpr, ObjVal) ->
                  % phase_change
                  setval(cost_dual, -1)@Pool,
                  setarg(upper_bound of cg_prob, Handle, ObjVal),
                  % set optimal vals
                  ( 
                      foreach(Var, Vars),
                      param(Pool, MP)
                  do
                      ( nonvar(Var) -> true
                      ; lp_var_get(MP, Var, solution, Sol),
                        get_cg_attr(Var, Pool, Attr),
                        setarg(mp_val of colgen, Attr, Sol)
                      )
                  ),
                  cg_info_message(Handle, "done, z_mp = %w%n", [ObjVal]),
                  OptDual = -1  
              ;
                ( Stabilisation == off ->
                    (
                        foreach(StabTerm, StabTerms)
                    do
                        StabTerm = stab_term with [
                                                   plus_var: 0.0,
                                                   minus_var: 0.0
                                                  ]
                    ),
                    StabExpr = Phase1Fn
                ;
                    Phase1Fn =.. [Sense, Expr],
                    (
                        foreach(StabTerm, StabTerms),
                        fromto(SExpr, Out, In, Expr)
                    do
                        StabTerm = stab_term with [
                                                   plus_var: Yplus,
                                                   plus_coeff: CoeffPlus,
                                                   minus_var: Yminus,
                                                   minus_coeff: CoeffMinus
                                                  ],
                        Out = CoeffPlus*Yplus - CoeffMinus*Yminus + In
                    ),
                    StabExpr =.. [Sense, SExpr]
                ),
                lp_probe(MP, StabExpr, ObjVal) ->
                  % still infeasible
                  phase1_perturb_if_necessary(Handle, StartBasis),
                  cg_info_message(Handle, "infeasible%n", []),
                  OptDual = 0
              ;
                  % if a constraint involving no generated vars is
                  % infeasible it will always be infeasible, fail
                  % note all constraints involving generated vars
                  % have an artificial variable and _should_ always
                  % be satisfiable; really we should check which
                  % cstrs are unsatisfiable and abort if a generated
                  % var constraint was
                  cg_info_message(Handle, "inconsistent constraints posted", []),
                  fail
              )
        ;
              % not in a two-phase, solve normally
              Phase1Fn = min(sum(ArtVars)),
              (
                  foreach(0, ArtVars)
              do
                  true
              ),
              lp_get(MP, objective, ObjExpr),
              ( Stabilisation == off ->
                  (
                      foreach(StabTerm, StabTerms)
                  do
                      StabTerm = stab_term with [
                                                 plus_var: 0.0,
                                                 minus_var: 0.0
                                                ]
                  ),
                  StabExpr = ObjExpr
              ;
                  ObjExpr =.. [Sense, Expr],
                  (
                      foreach(StabTerm, StabTerms),
                      fromto(SExpr, Out, In, Expr)
                  do
                      StabTerm = stab_term with [
                                                 plus_var: Yplus,
                                                 plus_coeff: CoeffPlus,
                                                 minus_var: Yminus,
                                                 minus_coeff: CoeffMinus
                                                ],
                      Out = CoeffPlus*Yplus - CoeffMinus*Yminus + In
                  ),
                  StabExpr =.. [Sense, SExpr]
              ),
              lp_probe(MP, StabExpr, ObjVal),
              setarg(upper_bound of cg_prob, Handle, ObjVal),
              % set optimal vals
              ( 
                  foreach(Var, Vars),
                  param(Pool, MP)
              do
                  ( nonvar(Var) -> true
                  ; lp_var_get(MP, Var, solution, Sol),
                    get_cg_attr(Var, Pool, Attr),
                    setarg(mp_val of colgen, Attr, Sol)
                  )
              ),
              perturb_if_necessary(Handle, StartBasis),
              cg_info_message(Handle, "done, z_mp = %w%n", [ObjVal]),
              OptDual = -1
        ),
        lp_get(MP, basis, Basis),
        ( ( OptDual = -1, ObjVal - (Tolerance*ObjVal) =< LowerBound ) ->
            % cannot apparently improve by SP solution
            ( arg(on_degeneracy of cg_prob, Handle, continue),
              SolveSubProblem =.. [_, SPHandle|_],
              arg(status of sp_prob, SPHandle, degenerate) ->
                % get dual values and upate dual_var attributes
                % (triggers SP solution - the SP will try to deal with
                %  degeneracy)
                lp_get(MP, dual_solution, Duals),
                DualArr =.. [[]|Duals],
                setarg(duals of cg_prob, Handle, DualArr),
                call_priority(update_duals(Pool, SPHandle, OptDual, Lookup, DualArr),
                              1),
                cg_new_MP_columns(VarCols, Pool)
            ;
              % terminate colgen            
              VarCols = []
            )
        ; ( OptDual = -1, StartBasis = Basis ) ->
            % CPLEX doesnt always change basis and
            % duals if at opt?
            % check for identical basis to avoid duplicate
            % columns
            ( arg(on_degeneracy of cg_prob, Handle, stop) ->
              % may not be true optimal solution/
              % proof of infeasibility, but
              % report optimal if in phase 2
              cg_info_message(Handle, "%t... detected identical"
                              " external solver basis after mp"
                              " optimisation%n%t    terminating with"
                              " potentially suboptimal solution%n", []),
              VarCols = []
            ;
              % get dual values and upate dual_var attributes
              % (triggers SP solution - the SP will try to deal with
              %  degeneracy)
              SolveSubProblem =.. [_, SPHandle|_],
              setarg(status of sp_prob, SPHandle, degenerate),
              lp_get(MP, dual_solution, Duals),
              DualArr =.. [[]|Duals],
              setarg(duals of cg_prob, Handle, DualArr),
              call_priority(update_duals(Pool, SPHandle, OptDual, Lookup, DualArr),
                            1),
              cg_new_MP_columns(VarCols, Pool)
            )
            
        ; StartBasis = Basis ->
            % apparently infeasible if in phase1
            ( arg(on_degeneracy of cg_prob, Handle, stop) ->
              cg_info_message(Handle, "%t... detected identical"
                              " external solver basis after mp"
                              " optimisation%n%t    terminating with"
                              " no solution where one may exist%n", []),
              fail
            
            ;
              % get dual values and upate dual_var attributes
              % (triggers SP solution - the SP will try to deal with
              %  degeneracy)
              SolveSubProblem =.. [_, SPHandle|_],
              setarg(status of sp_prob, SPHandle, degenerate),
              lp_get(MP, dual_solution, Duals),
              DualArr =.. [[]|Duals],
              setarg(duals of cg_prob, Handle, DualArr),
              call_priority(update_duals(Pool, SPHandle, OptDual, Lookup, DualArr),
                            1),
              cg_new_MP_columns(VarCols, Pool)
            )
        ;
              % get dual values and upate dual_var attributes
              % (triggers SP solution)
              SolveSubProblem =.. [_, SPHandle|_],
              lp_get(MP, dual_solution, Duals),
              DualArr =.. [[]|Duals],
              setarg(duals of cg_prob, Handle, DualArr),
              call_priority(update_duals(Pool, SPHandle, OptDual, Lookup, DualArr),
                            1),
              cg_new_MP_columns(VarCols, Pool)
        ),
        ( stabilisation_stopping_criteria(Handle, OptDual, VarCols) ->
              % no new columns to add
              getval(cost_dual, -1)@Pool,
              setarg(upper_bound of cg_prob, Handle, ObjVal),
              setarg(lower_bound of cg_prob, Handle, ObjVal)
        ;
              update_stabilisation(Handle, OptDual, VarCols),
              getval(cost_dual, CD)@Pool,
              ( CD = 0 -> Status = phase1 ; Status = phase2 ),
              SolveSubProblem =.. [_, SPHandle|_],
              setarg(status of sp_prob, SPHandle, Status),
              % add new columns to MP
              lp_add_columns(MP, VarCols),
              (
                  foreach(Var:_, VarCols),
                  param(MP)
              do
                  get_var_bounds(Var, Lo, Hi),
                  lp_var_set_bounds(MP, Var, Lo, Hi)
              ),
              lp_get(MP, vars, MPVarArr),
              functor(MPVarArr, _, NewColsAdded),              
              MPVarArr =.. [_|NewMPVars],
              setarg(mp_vars of cg_prob, Handle, NewMPVars),
              setarg(mp_cols_added of cg_prob, Handle, NewColsAdded),
              % extend the basis of the node lp for restart
              ExtraCols is NewColsAdded-ColsAdded,
              extend_base(Basis, ColsAdded, ExtraCols, NewBasis),
              lp_set(MP, basis, NewBasis),
              getval(sp_rc_sum, RCSum)@Pool,
              % calculate the Lasdon bound
              ( RCSum == none ->
                    % SP solution didnt store the rcs for us
                    % schedule the next MP iteration
                    schedule_suspensions(mp_susp of cg_prob, Handle),
                    wake
              ; RCSum == 1.0Inf ->
                    % SP solution didnt store the rcs for us
                    % schedule the next MP iteration
                    schedule_suspensions(mp_susp of cg_prob, Handle),
                    wake
              ;
                    Handle = cg_prob with upper_bound:UpperBound,
                    LasdonBound is max(ObjVal - RCSum, LowerBound),
                    setarg(lower_bound of cg_prob, Handle, LasdonBound),
                    ( (UpperBound =\= 1.0Inf,
                       LasdonBound >= UpperBound - (Tolerance * UpperBound)) ->
                          % cannot improve solution, done
                          getval(cost_dual, -1)@Pool,
                          setarg(upper_bound of cg_prob, Handle,
                                 ObjVal)
                    ;
                          % schedule the next MP iteration
                          schedule_suspensions(mp_susp of cg_prob, Handle),
                          wake
                    )
              )
        ).

stabilisation_stopping_criteria(Handle, OptDual, VarCols) :-
        Handle = cg_prob with [
                               duals:Duals,
                               master_prob: MPHandle,
                               sp_solution_call: SolveSubProblem,
                               stabilisation: Stabilisation,
                               stab_terms: StabTerms,
                               tolerance: (Tolerance, _BranchTolerance)
                              ],
        ( Stabilisation = off ->
            VarCols = []
        ; Stabilisation = stab_pred(_, Goal) ->
            SolveSubProblem =.. [_, SPHandle|_],
            SPHandle = sp_prob with module:Module,
            call(Goal)@Module
        ;
            (
                foreach(_Var:ObjCol, VarCols),
                param(Duals, OptDual, Tolerance)
            do
                % check all cols have wrong reduced cost
                ( ObjCol = [obj:Obj|Col] -> true
                ; Obj = 0, ObjCol = Col ),
                Cost is OptDual*Obj,
                (
                    foreach(I:V, Col),
                    fromto(Cost, In, Out, RC),
                    param(Duals)
                do
                    I1 is I + 1,
                    arg(I1, Duals, Dual),
                    Out is In + Dual*V
                ),
                RC =< -Tolerance
            ),
            (
                foreach(StabTerm, StabTerms),
                param(MPHandle, Tolerance)
            do
                StabTerm = stab_term with [
                                           plus_var: Yplus,
                                           plus_bound: Boundplus,
                                           minus_var: Yminus,
                                           minus_bound: Boundminus
                                          ],
                lp_var_get(MPHandle, Yplus, solution, YplusVal),
                abs(YplusVal) =< Tolerance,
                Boundplus =< Tolerance,
                lp_var_get(MPHandle, Yminus, solution, YminusVal),
                abs(YminusVal) =< Tolerance,
                Boundminus =< Tolerance
            )
        ).
           
update_stabilisation(Handle, OptDual, VarCols) :-
        Handle = cg_prob with [
                               duals:Duals,
                               sp_solution_call: SolveSubProblem,
                               master_prob: MPHandle,
                               stabilisation: Stabilisation,
                               stab_terms: StabTerms,
                               stab_iter_counts: StabCounts,
                               tolerance: (Tolerance, _BranchTolerance)
                              ],
        ( Stabilisation = off ->
            true
        ; Stabilisation = stab_pred(Goal, _) ->
            SolveSubProblem =.. [_, SPHandle|_],
            SPHandle = sp_prob with module:Module,
            call(Goal)@Module
        ;
            Stabilisation = on(BoundIter, BoundUpdate, CoeffIter, CoeffUpdate),
            arg(1, StabCounts, BoundCount),
            ( BoundCount < BoundIter ->
                BoundCount1 is BoundCount + 1,
                setarg(1, StabCounts, BoundCount1)
            ;
                update_stab_bounds(StabTerms, MPHandle, BoundUpdate),
                setarg(1, StabCounts, 1)
            ),
            arg(2, StabCounts, CoeffCount),
            ( CoeffCount < CoeffIter ->
                CoeffCount1 is CoeffCount + 1,
                setarg(1, StabCounts, CoeffCount1)
            ;
                ( update_stab_coeffs(StabTerms, VarCols, OptDual,
                                     Duals, Tolerance,
                                     CoeffUpdate) ->
                    setarg(2, StabCounts, 1)
                ;
                    true
                )
            )
         ).

update_stab_bounds(StabTerms, MPHandle, BoundUpdate) :-
        (
            foreach(StabTerm, StabTerms),
            param(MPHandle, BoundUpdate)
        do
            StabTerm = stab_term with [
                                       plus_var: Yplus,
                                       plus_bound: Boundplus,
                                       minus_var: Yminus,
                                       minus_bound: Boundminus
                                      ],
            Boundplus1 is max(0, Boundplus - BoundUpdate),
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus1),
            setarg(plus_bound of stab_term, StabTerm, Boundplus1),
            Boundminus1 is max(0, Boundminus - BoundUpdate),
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus1),
            setarg(minus_bound of stab_term, StabTerm, Boundminus1)
        ).

update_stab_coeffs(StabTerms, VarCols, OptDual,
                   Duals, Tolerance,
                   CoeffUpdate) :-
        (
            foreach(_Var:ObjCol, VarCols),
            param(OptDual, Duals, Tolerance)
        do
            % check all cols have wrong reduced cost
            ( ObjCol = [obj:Obj|Col] -> true
            ; Obj = 0, ObjCol = Col ),
            Cost is OptDual*Obj,
            (
                foreach(I:V, Col),
                fromto(Cost, In, Out, RC),
                param(Duals)
            do
                I1 is I + 1,
                arg(I1, Duals, Dual),
                Out is In + Dual*V
            ),
            RC < Tolerance
        ),
        (
            foreach(StabTerm, StabTerms),
            param(Duals, CoeffUpdate)
        do
            StabTerm = stab_term with idx: Idx,
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            Coeffplus is Dual + CoeffUpdate,
            setarg(plus_coeff of stab_term, StabTerm, Coeffplus),
            Coeffminus is Dual - CoeffUpdate,
            setarg(minus_coeff of stab_term, StabTerm, Coeffminus)
        ).

phase_change(min(sum(ArtVars))) :-
        (
            foreach(0, ArtVars)
        do
            true
        ).

frac_sol([Var|Vars], MP) :-
        lp_var_get(MP, Var, solution, Sol),
        ( max(Sol-floor(Sol), ceiling(Sol)-Sol) > 1e-05 -> true
        ; frac_sol(Vars, MP) ).

perturb_if_necessary(cg_prob with basis_perturbation:off, _Basis) :- !.
perturb_if_necessary(Handle, Basis) :- !,
        Handle = cg_prob with master_prob:MPHandle,
        ( lp_get(MPHandle, basis, Basis) ->
            lp_get(optimizer, Optimizer),
            ( Optimizer == cplex ->
                lp_get(optimizer_param(perind), Ind),
                lp_get(optimizer_param(perlim), Lim),
                lp_set(optimizer_param(perind), 1),
                lp_set(optimizer_param(perlim), 1),
                lp_solve(MPHandle, _),
                lp_set(optimizer_param(perind), Ind),
                lp_set(optimizer_param(perlim), Lim)
            ;
                lp_get(optimizer_param(perturb), Val),
                lp_set(optimizer_param(perturb), 1.0),
                lp_solve(MPHandle, _),
                lp_set(optimizer_param(perturb), Val)
            ),
            lp_get(MPHandle, basis, NewBasis),
            ( Basis = NewBasis -> writeln(perturbation-failed) ; true )
        ;
            true
        ).

phase1_perturb_if_necessary(cg_prob with basis_perturbation:off, _Basis) :- !.
phase1_perturb_if_necessary(Handle, Basis) :- !,
        Handle = cg_prob with [
                               master_prob:MPHandle,
                               phase1_obj:Obj
                              ],
        ( lp_get(MPHandle, basis, Basis) ->
            lp_get(optimizer, Optimizer),
            ( Optimizer == cplex ->
                lp_get(optimizer_param(perind), Ind),
                lp_get(optimizer_param(perlim), Lim),
                lp_set(optimizer_param(perind), 1),
                lp_set(optimizer_param(perlim), 1),
                lp_probe(MPHandle, Obj, _),
                lp_set(optimizer_param(perind), Ind),
                lp_set(optimizer_param(perlim), Lim)
            ;
                lp_get(optimizer_param(perturb), Val),
                lp_set(optimizer_param(perturb), 1.0),
                lp_probe(MPHandle, Obj, _),
                lp_set(optimizer_param(perturb), Val)
            ),
            lp_get(MPHandle, basis, NewBasis),
            ( Basis = NewBasis -> writeln(perturbation-failed) ; true )
        ;
            true
        ).

update_duals(Pool, SPHandle, OptDual, Lookup, Duals) :-
        SPHandle = sp_prob with [
                                 cost:OptVar,
                                 coeff_vars:DualVars,
                                 cutoff:Cutoff
                                ],
        ( nonvar(OptVar) ->
              Cutoff1 is Cutoff + OptDual*OptVar,
              setarg(cutoff of sp_prob, SPHandle, Cutoff1)
        ;
              always_set_dual(OptVar, OptDual, Pool)
        ),
        (
            foreach(DualVar, DualVars),
            param(Pool, Lookup, Duals)
        do
            get_idx(DualVar, Ident, Pool),
            hash_get(Lookup, Ident, Idx),
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            always_set_dual(DualVar, Dual, Pool)
        ).
/*
update_duals(SPHandle, Mu, Nu, OptDual, Duals, ObjVal, ZInc) :-
        SPHandle = sp_prob with [
                                 cost:OptVar,
                                 coeff_vars:DualVars,
                                 gub_coeff_vars:Branches
                                ],
        set_dual(OptVar, OptDual),
        
        ( get_idx(Mu, MuIdx) ->
              % calculate Vanderbeck-Wolsey bound
              % Delta1 = Z - sum(PiB) - sum(MuK) - sum(NuL),
              % Delta2 = ZInc - sum(PiB) - sum(MuK) - sum(NuL),
              % Rho1 = max(Delta1/K0, Delta1/L0),
              % Rho2 = max(Delta2/K0, Delta2/L0),
              % Rho = min(Rho1, Rho2),
              % SPOpt > Mu0 + Nu0 - Rho
              % approximated by
              % SPOpt >= Mu0 + Nu0 - Rho + 1e-05
              (
                  foreach(DualVar, DualVars),
                  fromto(0, In, Out, PiB),
                  param(Duals)
              do
                  get_idx(DualVar, Idx),
                  get_rhs(DualVar, Rhs),
                  Idx1 is Idx + 1,
                  arg(Idx1, Duals, Dual),
                  Out is In + Dual*Rhs,            
                  set_dual(DualVar, Dual)
              ),
              (
                  foreach(Branch, Branches),
                  
                  % foreach(cg_gub with dual_var:GUBDualVar, Branches),
                  fromto(PiB, In, Out, ConstTerm),
                  param(Duals)
              do
                  
                  Branch = cg_gub with [
                                        type:Type,
                                        dual_var:GUBDualVar
                                       ],
                  
                  ( atom(Type) ->
                  
                  get_rhs(GUBDualVar, Rhs),

                  ( (Rhs = 0, Type \= (>=)) ->
                        Out = In
                  ;
                  
                  get_idx(GUBDualVar, Idx),
                  Idx1 is Idx + 1,
                  arg(Idx1, Duals, Dual),
                  Out is In + Dual*Rhs,
                  set_dual(GUBDualVar, Dual)
                  
                  )
              
                  ;
                  (
                      foreach(T, Type),
                      foreach(GUBDV, GUBDualVar),
                      fromto(In, I, O, Out),
                      param(Duals)
                  do
                      get_rhs(GUBDV, Rhs),
                      ( (Rhs = 0, T \= (>=)) ->
                            O = I,
                            % the constraint was not explicitly
                            % added to the lp because it just
                            % fixes vars to 0,
                            % no constraint, no dual val,
                            % no contribution to ConstTerm
                            % but we MUST still give it a big
                            % negative dual val for the sps
                            set_dual(GUBDV, -1000000000)
                      ;
                            get_idx(GUBDV, Idx),
                            ( var(Idx) ->
                                  % the constraint was not
                                  % added to the lp because
                                  % it contained no vars yet
                                  % is it safe to set Dual = 0?
                                  Dual = 0
                            ;
                                  
                            Idx1 is Idx + 1,
                            arg(Idx1, Duals, Dual),
                            
                            true
                            ),
                            
                            O is I + Dual*Rhs,
                            set_dual(GUBDV, Dual)
                      )
                  )
                  )
              
              ),
              % get_idx(Mu, MuIdx),
              get_rhs(Mu, K),
              MuIdx1 is MuIdx + 1,
              arg(MuIdx1, Duals, Mu0),
              set_dual(Mu, Mu0),
              get_idx(Nu, NuIdx),
              get_rhs(Nu, L),
              NuIdx1 is NuIdx + 1,
              arg(NuIdx1, Duals, Nu0),
              set_dual(Nu, Nu0),
              Delta1 is ObjVal - ConstTerm,
              Rho1 is max(Delta1/K, Delta1/L),
              Delta2 is ZInc - ConstTerm,
              Rho2 is max(Delta2/K, Delta2/L),
              ( Rho2 =< Rho1 ->
                    % should prune node if no SP solutions
                    Rho = Rho2
              ;
                    % should terminate CG in node if no SP solutiuons
                    Rho = Rho1
              ),
              % but since
              % SPOpt = max{(pi-e)X - fW + sum(muZ) + sum(nuZ) + mu0 + nu0}
              % we can just leave Mu0, Nu0 out?
              % CGCutoff is Mu0 + Nu0 - Rho + 1e-05,
              CGCutoff is 1e-05 - Rho,
              setarg(cutoff of sp_prob, SPHandle, CGCutoff)        
        ;
              
        (
            foreach(DualVar, DualVars),
            param(Duals)
        do
            get_idx(DualVar, Idx),
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            set_dual(DualVar, Dual)
        )
        
        ).
*/

bp_separate(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = cg_prob with [
                               bfs_tree:BfsPool,
                               sep_call:Goal,
                               shelf:Shelf
                              ],
        Goal = call(SepGoal)@Module,
        ( SepGoal = true ->
            true
        ;
            \+ \+ ( BfsPool:impose_node_state(other),
                    call(Goal)
                  ),
            shelf_get(Shelf, 1, Branches),
            shelf_set(Shelf, 1, []),
            (
                foreach(Score:Branch, Branches),
                param(BfsPool, Module)
            do
                BfsPool:bfs_branch([Score, call(Branch)@Module])
            )
        ).


fractional_vars(Vars, FracVars, Vals, Diffs, Fracs, L, U, Pool) :-
        Vars = [X|_],
        cg_var_mp_val(Pool, X, Sol),
        ( var(Sol) ->
              get_pool_handle(cg_prob with master_prob:MP, Pool),
              (
                  foreach(Var, Vars),
                  fromto(FracVars, VarsIn, VarsOut, []),
                  fromto(Vals, ValsIn, ValsOut, []),
                  fromto(Diffs, DiffsIn, DiffsOut, []),
                  fromto(Fracs, FracsIn, FracsOut, []),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U),
                  param(MP)
              do
                  ( nonvar(Var) ->
                        VarsIn = VarsOut,
                        ValsIn = ValsOut,
                        DiffsIn = DiffsOut,
                        FracsIn = FracsOut,
                        LIn = LOut,
                        UIn = UOut
                  ;
                        lp_var_get(MP, Var, solution, Val),
                        Diff is abs(round(Val) - Val),
                        ( Diff =< 1e-5 ->
                              VarsIn = VarsOut,
                              ValsIn = ValsOut,
                              DiffsIn = DiffsOut,
                              FracsIn = FracsOut,
                              LIn = LOut,
                              UIn = UOut
                        ;
                              Frac is Val - floor(Val),
                              ( Frac < 0.5 ->
                                    LOut is max(Frac, LIn),
                                    UOut = UIn
                              ;
                                    Frac > 0.5 ->
                                        LOut = LIn,
                                        UOut is min(Frac, UIn)
                              ;
                                        LOut = 0.5,
                                        UOut = 0.5
                              ),
                              VarsIn = [Var|VarsOut],
                              ValsIn = [Val|ValsOut],
                              DiffsIn = [Diff|DiffsOut],
                              FracsIn = [Frac|FracsOut]
                        )
                  )

              )
        ;
              (
                  foreach(Var, Vars),
                  fromto(FracVars, VarsIn, VarsOut, []),
                  fromto(Vals, ValsIn, ValsOut, []),
                  fromto(Diffs, DiffsIn, DiffsOut, []),
                  fromto(Fracs, FracsIn, FracsOut, []),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U),
                  param(Pool)
              do
                  ( nonvar(Var) ->
                        VarsIn = VarsOut,
                        ValsIn = ValsOut,
                        DiffsIn = DiffsOut,
                        FracsIn = FracsOut,
                        LIn = LOut,
                        UIn = UOut
                  ;
                        cg_var_mp_val(Pool, Var, Val),
                        Diff is abs(round(Val) - Val),
                        ( Diff =< 1e-5 ->
                              VarsIn = VarsOut,
                              ValsIn = ValsOut,
                              DiffsIn = DiffsOut,
                              FracsIn = FracsOut,
                              LIn = LOut,
                              UIn = UOut
                        ;
                              Frac is Val - floor(Val),
                              ( Frac < 0.5 ->
                                    LOut is max(Frac, LIn),
                                    UOut = UIn
                              ;
                                Frac > 0.5 ->
                                    LOut = LIn,
                                    UOut is min(Frac, UIn)
                              ;
                                    LOut = 0.5,
                                    UOut = 0.5
                              ),
                              VarsIn = [Var|VarsOut],
                              ValsIn = [Val|ValsOut],
                              DiffsIn = [Diff|DiffsOut],
                              FracsIn = [Frac|FracsOut]
                        )
                  )
              )
        ).

:- demon solveSPs/1.
solveSPs(Handle) :-
        Handle = cg_prob with sp_solution_call: SolveSubProblem,
        solveSP(SolveSubProblem).

solveSP(SolveSubProblem) :-
        SolveSubProblem =.. [_, SPHandle|_],
        SPHandle = sp_prob with module:Module,
        % note SolveSubProblem MUST post at least one positive
        % reduced cost solution to the MP pool with
        % cg_subproblem_solution if any exist for any subproblem
        call(SolveSubProblem)@Module,
        fail.
solveSP(_SolveSubProblem).

%-----------------------------------------------------------------------
% Pools
%-----------------------------------------------------------------------

:- local record(colgen_pools). % list of colgen pool names

create_colgen_pool(Pool) :-
	create_constraint_pool(Pool, property(arity) of cg_constraint_type,
                               [
                                var_dual/5 -> var_dual/6,
                                get_dual/2 -> get_dual/3,
                                get_idx/2 -> get_idx/3,
                                get_rhs/2 -> get_rhs/3,
                                
                                always_set_dual/2 -> always_set_dual/3,

                                set_dual/2 -> set_dual/3,
                                solve/1 -> bp_solve/2,
                                solver_setup/3 -> cg_solver_setup/4,
                                solver_setup/2 -> cg_solver_setup/3,
                                var_get/3 -> cg_var_get/4,
                                get/2 -> cg_get/3,
                                
                                set/2 -> cg_set/3,
                                statistics/0 -> cg_statistics/1,
                                
                                integers/1 -> cg_integers/2,
                                identified_constraint/2 -> add_cg_pool_constraint/3,
                                (::)/2 -> cg_range/3,
                                (=:=)/2 -> cg_eq/3,
                                (>=)/2 -> cg_ge/3,
                                (=<)/2 -> cg_le/3,
                                ($::)/2 -> cg_range/3,
                                ($=)/2 -> cg_eq/3,
                                ($>=)/2 -> cg_ge/3,
                                ($=<)/2 -> cg_le/3,
                                branch/1 -> cg_branch/2,

                                branch/2 -> cg_branch/3,
                                                  
                                cg_subproblem_count/1 -> cg_sp_count/2,
                                cg_subproblem_solution/1 -> cg_sp_sol/2,
                                cg_subproblem_rc_sum/1 -> cg_sp_rc_sum/2,
                                minimize/4 -> cg_minimize/5,
                                minimize/3 -> cg_minimize/4
                               ]).


colgen_instance(PoolName) :-
        ( is_constraint_pool(PoolName),
	  recorded(colgen_pools, PoolName) % is a colgen pool
	->
            % if pool exists, check if it is currently empty 
	    ( pool_is_empty(PoolName),
	      get_pool_item(PoolName, 0) % has no associated solver
	    ->
		true
	    ;
		printf(error, "Colgen instance still in use in %w%n", [colgen_instance(PoolName)]),
		abort
	    )
	;
%	    ( current_module(PoolName) ->
%		  error(6, colgen_instance(PoolName))
%	    ;
		  record(colgen_pools, PoolName),
		  create_colgen_pool(PoolName)
%	    )
	).

get_pool_handle(Handle, Pool) :-
	( get_pool_item(Pool, 0) ->
            Handle = cg_prob with [pool:Pool, bfs_tree:[]],
            init_suspension_list(mp_susp of cg_prob, Handle),
            set_pool_item(Pool, Handle),
            (local variable(sp_rc_sum))@Pool,
            (local variable(cost_dual))@Pool,
            (local variable(mp_solution_info))@Pool,
            (local array(sp_solns(1)))@Pool,
            setval(sp_solns(0), [])@Pool
        ;
            get_pool_item(Pool, Handle),
            Handle = cg_prob with []
        ).
        

% ----------------------------------------------------------------------
% Expression simplifier (using lib(linearize))
%
% A linear expression is normalised into a list (sum) of the form
%	[C0*1, C1*X1, C2*X2, ...]
% where Ci are numbers and Xi are distinct variables.
% The first (constant) term is always present, Ci (i>=1) are nonzero.
% The expression must be built from variables, numbers, +/2, */2, -/2, -/1.
% The simplifier fails if the expression is not linear.
%
% renormalise/2 renormalises a normal form expression after variable
% bindings, unifications.
%
% A normalised constraint has the form
%	Sense:NormExpr
% where Sense is one of the atoms =:=, >= or =< and NormExpr is a
% normalised expression as above. E.g. (>=):[-5*1,3*X] encodes
% the constraint  -5 + 3*X >= 0.
% ----------------------------------------------------------------------

cg_normalise_cstr(E1 =:= E2, (=:=):Norm, CoeffVar) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm)
        ).
cg_normalise_cstr(E1 >= E2, (>=):Norm, CoeffVar) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm)
        ).
cg_normalise_cstr(E1 =< E2, (=<):Norm, CoeffVar) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm)
        ).
cg_normalise_cstr(Cstr, _, _) :-
	writeln(error, "Error: Unknown constraint":Cstr),
	abort. 

    filter_auxvar(Var, [C*X|Terms], Norm) :-
        ( Var == X ->
              Norm = Terms
        ;
              Norm = [C*X|Norm0],
              filter_auxvar(Var, Terms, Norm0)
        ).
