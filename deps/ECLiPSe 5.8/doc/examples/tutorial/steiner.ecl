:- lib(ic_sets).
:- lib(ic).

steiner(N, Sets) :-
	NB is N * (N-1) // 6,			% compute number of triplets
	intsets(Sets, NB, 1, N),		% initialise the set variables
	( foreach(S,Sets) do
	    #(S,3)				% constrain their cardinality
	),
	( fromto(Sets,[S1|Ss],Ss,[]) do
	    ( foreach(S2,Ss), param(S1) do
		#(S1 /\ S2, C),			% constrain the cardinality
		C #=< 1				% of pairwise intersections
	    )
	),
	label_sets(Sets).			% search

label_sets([]).
label_sets([S|Ss]) :-
	insetdomain(S,_,_,_),
	label_sets(Ss).

