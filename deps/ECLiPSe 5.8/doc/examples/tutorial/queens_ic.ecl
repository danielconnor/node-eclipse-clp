:- lib(ic).

queens(N, Board) :-
	length(Board, N),
	Board :: 1..N,
	( fromto(Board, [Q1|Cols], Cols, []) do
	    ( foreach(Q2, Cols), count(Dist,1,_), param(Q1) do
		noattack(Q1, Q2, Dist)
	    )
	).

noattack(Q1,Q2,Dist) :-
	Q2 #\= Q1,
	Q2 - Q1 #\= Dist,
	Q1 - Q2 #\= Dist.


