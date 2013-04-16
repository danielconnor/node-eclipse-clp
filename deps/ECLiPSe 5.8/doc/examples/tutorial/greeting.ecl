%
% A very small module
%

:- module(greeting).
:- export hello/0.

hello :-
	who(X),
	printf("Hello %w!%n", [X]).

who(world).
who(friend).
