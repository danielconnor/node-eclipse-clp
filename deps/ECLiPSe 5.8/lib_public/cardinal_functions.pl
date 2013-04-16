%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cardinal_functions.pl      By Francisco Azevedo    2000 - 2004
%
% Set optional functions of Cardinal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [cardinal_union,cardinal_minmax].


%--------
% cardinality_function(+Card, +Set)
%  Inferences for Set's cardinality function.
%--
cardinality_function(Card, Set):-
	domain(Set, [_:NIn,_:NMax]),
	(var(Card) -> Card::NIn..NMax,
		set_cardinality(Set, Card),
		check_cardinality(Card, Set)
	;CardVar::Card, CardVar::NIn..NMax,
	 set_cardinality(Set, CardVar),
	 check_cardinality(CardVar, Set)
	).

%--------
% check_cardinality(+Cardinality, ?Set)
%  Suspend until Cardinality of Set is ground. Then, if it is equal to
% the cardinality of one of its bounds, assign Set to that bound.
% Else suspend until Set further constrained.
%--
check_cardinality(C, S):- var(C), !, suspend(check_cardinality(C,S), 2, C->inst).
check_cardinality(C, S):-
	domain(S, C, [Glb:NIn,Poss:NMax]),
	(C==NIn -> S=Glb
	;C==NMax -> lub(Glb, Poss, Lub), S=Lub
	;suspend(check_cardinality(C,S), 2, S->cardinal:bounded)
	).


%--------
% check_ground_functions(+Functions, +Set)
%  Check if each of the optional Functions agrees with ground Set.
%--
check_ground_functions(Functions, Set):-
	(member_remove(Functions, cardinality:Card, Fs1) ->
		length(Set, C),
		(Card=C ; member(M,Card), (M=C ; M=A..B,number(A),number(B),C>=A,C=<B)), !
	;Fs1=Functions
	),
	(member_remove(Fs1, minimum:Min, Fs2) -> Set=[Min|_] ; Fs2=Fs1),
	(member_remove(Fs2, maximum:Max, Fs3) -> reverse(Set, [Max|_]) ; Fs3=Fs2),
	(member_remove(Fs3, union:Union, _) ->
		(var(Union) -> all_sets_union(Set, [],U), U=Union
		;Union=GlbU+PossU -> verify_inclusion(Set,GlbU,PossU,[])
		;all_sets_union(Set, [],Union)
		)
	;true
	).

%--------
% check_functions(+Functions, +SetVar)
%  Check if each optional function is in Functions to update SetVar's functions
% and domain attributes.
%--
check_functions(Functions, SetVar):-
	(member_remove(Functions,cardinality:Card,Fs1) -> true ; Fs1=Functions),
	cardinality_function(Card, SetVar),
	(member_remove(Fs1, minimum:Min, Fs2) -> minimum_function(Min,SetVar) ; Fs2=Fs1),
	(member_remove(Fs2, maximum:Max, Fs3) -> maximum_function(Max,SetVar) ; Fs3=Fs2),
	(member_remove(Fs3, union:Union, _) -> union_function(Union,SetVar) ; true).

%--------
% check_new_functions(+Functions, +SetVar)
%  Check if each optional function is in (new) Functions, to update SetVar's functions
% and domain attributes.
%--
check_new_functions(Functions, S):-
	(member_remove(Functions, cardinality:Card, Fs1) ->
		cardinality(S, C1),
		(ground(Card), is_list(Card) -> C1::Card ; C1#=Card)
	;Fs1=Functions
	),
	(member_remove(Fs1, minimum:Min, Fs2) ->
		minimum(S, MinS),
		(is_domain(MinS) -> (is_domain(Min) -> MinS=Min ; MinS::Min)
		;minimum_function(Min,S)
		)
	;Fs2=Fs1
	),
	(member_remove(Fs2, maximum:Max, Fs3) ->
		maximum(S, MaxS),
		(is_domain(MaxS) -> (is_domain(Min) -> MaxS=Max ; MaxS::Max)
		;maximum_function(Max,S)
		)
	;Fs3=Fs2
	),
	(member_remove(Fs3, union:Union, _) ->
		union_att(S, U),
		(var(U) -> union_function(Union, S)
		;var(Union) -> U=[Union|_]
		;Union=GlbUVar+PossUVar ->
			sort(GlbUVar,SGlbUVar), sort(PossUVar,SPossUVar),
			U=[SVar|_],
			contains(SVar, SGlbUVar),
			lub(SGlbUVar, SPossUVar, LubUVar),
			contains(LubUVar, SVar)
		;is_list(Union), sort(Union,SUnion), U=[SUnion|_]
		)
	;true
	).

