ancestor(X,Y) :- parent(X,Y).			% clause 1
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).	% clause 2

parent(abe, homer).				% clause 3
parent(abe, herbert).				% clause 4
parent(homer, bart).				% clause 5
parent(marge, bart).				% clause 6

male(abe).
male(homer).
male(herbert).
male(bart).

