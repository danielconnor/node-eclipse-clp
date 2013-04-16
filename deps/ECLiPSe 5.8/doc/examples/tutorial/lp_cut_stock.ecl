:- lib(eplex).

% eplex instance creation
:- eplex_instance(cut_stock).

lp_cut_stock(Lengths, Demands, StockLength, Vars, Cost) :-
        (
            foreach(Li, Lengths),
            foreach(Bi, Demands),
            foreach([], XijVars0),
            foreach(Maxi, Bounds),
            fromto(0, KIn, KOut, K0),
            param(StockLength)
        do
            KOut is KIn + fix(ceiling(Bi/floor(StockLength/Li))),
            Maxi is fix(floor(StockLength/Li))
        ),
        (
            for(J, 1, K0),
            foreach(Wj, Obj),
            foreach(Xj:Used, Vars),
            fromto(XijVars0, VIn, VOut, XijVars),
            param(Lengths, StockLength, Bounds)
        do
            cut_stock:integers([Xj,Wj]),
            % Xj variable bounds
            cut_stock:(Xj::0..1),
            % Wj variable bounds
            cut_stock:(Wj::0..StockLength),
            (
                foreach(Li, Lengths),
                foreach(Xij, Used),
                foreach(Li*Xij, Knapsack),
                foreach(XiVars, VIn),
                foreach([Xij|XiVars], VOut),
                foreach(Maxi, Bounds),
                param(Xj)
            do
                % Xij variable bounds
                cut_stock:integers(Xij),
                cut_stock:(Xij::0..Maxi)
            ),
            % cutting knapsack constraint
            cut_stock:(sum(Knapsack) + Wj =:= StockLength*Xj)
        ),
        (
            foreach(Bi, Demands),
            foreach(Xijs, XijVars)
        do
            % demand constraint
            cut_stock:(sum(Xijs) >= Bi)
        ),
        cut_stock:eplex_solver_setup(min(sum(Obj))),
        % optimization call
        cut_stock:eplex_solve(Cost).
