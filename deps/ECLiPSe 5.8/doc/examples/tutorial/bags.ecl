%
% Bag example
%

simple_findall(Goal, Solutions) :-
	bag_create(Bag),
	(
	    call(Goal),
	    bag_enter(Bag, Goal),
	    fail
	;
	    bag_dissolve(Bag, Solutions)
	).

