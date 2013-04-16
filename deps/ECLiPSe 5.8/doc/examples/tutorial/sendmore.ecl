
:- lib(ic).

sendmore(Digits) :-
        Digits = [S,E,N,D,M,O,R,Y],

        % Assign a finite domain with each letter - S, E, N, D, M, O, R, Y -
        % in the list Digits
        Digits :: [0..9],

        % Constraints
        alldifferent(Digits),
        S #\= 0,
        M #\= 0,
                     1000*S + 100*E + 10*N + D
                   + 1000*M + 100*O + 10*R + E
        #= 10000*M + 1000*O + 100*N + 10*E + Y,

        % Search
        labeling(Digits).

