:- lib(eplex).
:- lib(branch_and_bound).

:- eplex_instance(mip).

main6(Cost, Vars) :-
        % b. create the problem variables and set their range
        Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,D3], 
        mip: (Vars :: 0.0..1.0Inf),

        % c. post the constraints for the problem to the eplex instance
        mip: (A1 + A2 + A3 $= 21),
        mip: (B1 + B2 + B3 $= 40),
        mip: (C1 + C2 + C3 $= 34),
        mip: (D1 + D2 + D3 $= 10),

        mip: (A1 + B1 + C1 + D1 $=< 50),
        mip: (A2 + B2 + C2 + D2 $=< 30),
        mip: (A3 + B3 + C3 + D3 $=< 40),
        mip: (A1 $= A2),

        % j. post the objective function as a constraint 
        ObjFunc = 10*A1 + 7*A2 + 200*A3 + 
                   8*B1 + 5*B2 + 10*B3 +
                   5*C1 + 5*C2 +  8*C3 + 
                   9*D1 + 3*D2 +  7*D3,
        mip: (ObjFunc  $= Cost),

        % k. this is a more flexible method for setting up a solver.
        %    [deviating_bounds] specifies that the external solver should be
        %    invoked when any solution value is outside the variable bounds 
        mip: eplex_solver_setup(min(ObjFunc), Cost, [], [deviating_bounds]),

        % l. Use the branch_and_bound library to do the branch and bound
        bb_min(( branching(Vars), 
                 mip: eplex_get(cost, Cost),
                 (foreach(V, Vars) do mip: eplex_var_get(V,solution,V))
               ), Cost, _).

branching(IntVars) :-
        % Find a variable X which does not have an integer solution value
        (integer_violation(IntVars, X, XVal) ->
            % m. try the closer integer range first
            Split is round(XVal),
            (Split > XVal ->
                (mip: (X $>= Split) ;  mip: (X $=< Split - 1))
            ;
                (mip: (X $=< Split) ; mip:  (X $>= Split + 1))
            ),
            branching(IntVars)
        ;
            % cannot find any integer violations; found a solution
            true
        ).

% returns Var with solution value Val which violates the integer constraint
integer_violation([X|Xs], Var, Val) :-
        mip: eplex_var_get(X, solution, RelaxedSol),
        % m. we are dealing with floats here, so need some `margin' for a
        %    float value to be considered integer (1e-5 on either side)
        (abs( RelaxedSol - round(RelaxedSol) ) >= 1e-5 ->
            Var = X, Val = RelaxedSol
        ;
            integer_violation(Xs, Var, Val)
        ).

