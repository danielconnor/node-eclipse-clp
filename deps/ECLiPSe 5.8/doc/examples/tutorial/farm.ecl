
:- lib(ic).

farm(F, A, B, C) :-
	[A, B, C] :: 0.0 .. 1.0Inf,	% The 3 sides of the lake
	triangle_area(A, B, C, 7),	% The lake area is 7

	[F, FA, FB, FC] :: 1 .. 1.0Inf,	% The square areas are integral
	square_area(A, FA),
	square_area(B, FB),
	square_area(C, FC),

	F #= FA+FB+FC,FA $>= FB, FB $>= FC. % Avoid symmetric solutions

triangle_area(A, B, C, Area) :-
	S $>= 0,
	S $= (A+B+C)/2,
	Area $= sqrt(S*(S-A)*(S-B)*(S-C)).

square_area(A, Area) :-
	Area $= sqr(A).


solve(F) :-
	farm(F, A, B, C),		% the model
	indomain(F),			% ensure that solution is minimal
	locate([A, B, C], 0.01).
