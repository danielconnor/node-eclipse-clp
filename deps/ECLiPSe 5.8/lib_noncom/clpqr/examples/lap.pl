
laplace([_, _]).
laplace([H1, H2, H3|T]):-
        laplace_vec(H1, H2, H3),
        laplace([H2, H3|T]).

laplace_vec([_,_], [_,_], [_,_]).
laplace_vec([_,A,B|C], [D,E,F|G], [_,H,I|J]) :-
        { 4*E = F + D + A + H },
        laplace_vec([A,B|C], [E,F|G], [H,I|J]).

lap(X) :-
   X =  [
    [0,0,0,0,0,0,0,0,0,0,0],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,100,100,100,100,100,100,100,100,100,100]
    ],
  laplace( X),
  print_nl( X).

print_nl( []).
print_nl( [X|Xs]) :-
  print( X), nl,
  print_nl( Xs).
