queen(Data, Out) :-
	qperm(Data, Out),
	safe(Out).

qperm([], []).
qperm([X|Y], [U|V]) :-
	qdelete(U, X, Y, Z),
	qperm(Z, V).

qdelete(A, A, L, L).
qdelete(X, A, [H|T], [A|R]) :-
	qdelete(X, H, T, R).

safe([]).
safe([N|L]) :-
	nodiag(L, N, 1),
	safe(L).

nodiag([], _, _).
nodiag([N|L], B, D) :-
	D =\= N - B,
	D =\= B - N,
	D1 is D + 1,
	nodiag(L, B, D1).

