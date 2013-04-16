% n-queens with finite domains
:- [domain]. % load in the finite domain ECH handler

:- setval(domain,number).

queen(N,L):- 
	length(L,N), 
	L::1..N,
	queen(L), chr_labeling.

queen([]).
queen([X|Xs]):- safe(X,Xs,1),queen(Xs).

safe(X,[],N).
safe(X,[H|T],N):- no_attack(X,H,N), M is N+1, safe(X,T,M).

no_attack(X,Y,N):- X ne Y, X ne Y-N, X ne Y+N, Y ne X-N, Y ne X+N.
