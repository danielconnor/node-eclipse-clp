%
% Shelf example
%

count_solutions(Goal, Total) :-
	shelf_create(count(0), Shelf),
	(
	    call(Goal),
	    shelf_get(Shelf, 1, Old),
	    New is Old + 1,
	    shelf_set(Shelf, 1, New),
	    fail
	;
	    shelf_get(Shelf, 1, Total)
	),
	shelf_abolish(Shelf).
