:- lib(eplex).

:- eplex_instance(prob).  

main4(Cost, Vars) :-
        Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,D3], 
        prob: (Vars $:: 0.0..1.0Inf),
        prob: integers(Vars),  % h. impose the integrality constraint
        prob: (A1 + A2 + A3 $= 21),
        prob: (B1 + B2 + B3 $= 40),
        prob: (C1 + C2 + C3 $= 34),
        prob: (D1 + D2 + D3 $= 10),

        prob: (A1 + B1 + C1 + D1 $=< 50),
        prob: (A2 + B2 + C2 + D2 $=< 30),
        prob: (A3 + B3 + C3 + D3 $=< 40),

        prob: eplex_solver_setup(min(
                10*A1 + 7*A2 + 200*A3 + 
                 8*B1 + 5*B2 + 10*B3 +
                 5*C1 + 5*C2 +  8*C3 + 
                 9*D1 + 3*D2 +  7*D3)),

        prob: (A1 $= A2), % g. the new constraint, added after setup

        %------------------------------- End of Modelling code

        prob: eplex_solve(Cost),  
        (foreach(V, Vars) do
            prob: eplex_var_get(V, typed_solution, V) 
        ).
