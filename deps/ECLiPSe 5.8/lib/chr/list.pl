
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(700, xfx, eqlist).
:- op(700, xfx, lenlist).
ground(_1206) :- not nonground(_1206).
chr_member(_1231, _1222) :- eqlist([_1223, [_1231], _1227], _1222).
chr_append(_1254, _1253, _1248) :- eqlist([_1254, _1253], _1248).
chr_last(_1272, _1279) :- eqlist([_1273, [_1279]], _1272).
palindrome([]).
palindrome([_1303]).
palindrome(_1326) :- lenlist(_1333, 1), eqlist([_1333, _1322, _1333], _1326), palindrome(_1322).
reverse([], []).
reverse(_1391, _1382) :- lenlist(_1391, _1402), lenlist(_1382, _1402), lenlist(_1387, 1), eqlist([_1387, _1378], _1391), eqlist([_1377, _1387], _1382), reverse(_1378, _1377).
permute([], []).
permute(_1475, _1464) :- lenlist(_1475, _1486), lenlist(_1464, _1486), lenlist(_1471, 1), eqlist([_1471, _1451], _1475), eqlist([_1460, _1471, _1459], _1464), eqlist([_1460, _1459], _1450), permute(_1451, _1450).
tree([_1508], [_1508], _1508) :- var(_1508) -> suspend(atomic(_1508), 3, _1508 -> inst) ; atomic(_1508).
tree(_1580, _1567, t(_1576, _1561, _1554)) :- eqlist([[_1576], _1563, _1556], _1580), eqlist([_1562, _1555, [_1576]], _1567), tree(_1563, _1562, _1561), tree(_1556, _1555, _1554).
transpose([], _1612) :- eqlist([_1612, [[]]], [[]|_1612]).
transpose([_1644|_1639], _1645) :- first_column(_1645, _1644, _1638), transpose(_1639, _1638).
first_column([], [], []).
first_column([[_1688|_1685]|_1678], [_1688|_1677], [_1685|_1676]) :- first_column(_1678, _1677, _1676).



%%% Callables for eqlist / 2

'CHRlabel_with'(eqlist(A, B), C, D) ?-
	coca(try_clause(D, eqlist(A, B), eqlist(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_eqlist(A, B)).



%%% Prolog clauses for eqlist / 2

clause_eqlist([A|B], C) :-
	(
	    var(C)
	->
	    length(C, D)
	;
	    true
	),
	(
	    A = [],
	    eqlist(B, C)
	;
	    C = [E|F],
	    A = [E|G],
	    eqlist([G|B], F)
	).
:- current_macro(clause_eqlist / 2, _1983, _1984, _1985) -> true ; define_macro(clause_eqlist / 2, tr_chr / 2, [write]).
eqlist(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, eqlist(A, B))),
	'CHReqlist_2'(eqlist(A, B), D, E, C).



%%% Rules handling for eqlist / 2

'CHReqlist_2'(eqlist(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHReqlist_2'(eqlist([], A), B, C, D) ?-
	coca(try_rule(D, eqlist([], A), anonymous("0"), eqlist([], E), replacement, true, E = [])),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("0"))),
	A = [].
'CHReqlist_2'(eqlist([A], B), C, D, E) ?-
	coca(try_rule(E, eqlist([A], B), anonymous("1"), eqlist([F], G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	A = B.
'CHReqlist_2'(eqlist([A|B], []), C, D, E) ?-
	coca(try_rule(E, eqlist([A|B], []), anonymous("2"), eqlist([F|G], []), replacement, true, (F = [], eqlist(G, [])))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	A = [],
	eqlist(B, []).
'CHReqlist_2'(eqlist([[A|B]|C], D), E, F, G) ?-
	coca(try_rule(G, eqlist([[A|B]|C], D), anonymous("3"), eqlist([[H|I]|J], K), replacement, true, (K = [H|L], eqlist([I|J], L)))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("3"))),
	D = [A|M],
	eqlist([B|C], M).
'CHReqlist_2'(eqlist(A, B), C, D, E) ?-
	coca(try_rule(E, eqlist(A, B), anonymous("4"), eqlist(F, G), replacement, (delete(H, F, I), H == []), eqlist(I, G))),
	no_delayed_goals((delete(J, A, K), J == [])),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	eqlist(K, B).
'CHReqlist_2'(eqlist(A, B), C, D, E) ?-
	coca(try_rule(E, eqlist(A, B), anonymous("5"), eqlist(F, G), replacement, (delete(H, F, I), H == G), eqlist(I, []))),
	no_delayed_goals((delete(J, A, K), J == B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	eqlist(K, []).
'CHReqlist_2'(eqlist(A, B), C, D, E) :-
	'CHReqlist_2__9'(eqlist(A, B), C, D, E).
:- set_flag('CHReqlist_2' / 4, leash, notrace).
:- current_macro('CHReqlist_2' / 4, _3312, _3313, _3314) -> true ; define_macro('CHReqlist_2' / 4, tr_chr / 2, [write]).
'CHReqlist_2__9'(A, B, C, D) :-
	'CHReqlist_2__10'(A, B, C, D).
:- set_flag('CHReqlist_2__9' / 4, leash, notrace).
'CHReqlist_2__10'(eqlist(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, eqlist(A, B)], 'CHReqlist_2'(eqlist(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHReqlist_2__10' / 4, leash, notrace).
lenlist(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, lenlist(A, B))),
	'CHRlenlist_2'(lenlist(A, B), D, E, C).



%%% Rules handling for lenlist / 2

'CHRlenlist_2'(lenlist(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRlenlist_2'(lenlist([], A), B, C, D) ?-
	coca(try_rule(D, lenlist([], A), anonymous("6"), lenlist([], E), replacement, true, var(E) -> E = 0 ; E =:= 0)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("6"))),
	(
	    var(A)
	->
	    A = 0
	;
	    A =:= 0
	).
'CHRlenlist_2'(lenlist([A|B], C), D, E, F) ?-
	coca(try_rule(F, lenlist([A|B], C), anonymous("7"), lenlist([G|H], I), replacement, true, (I > 0, plus(J, 1, I), lenlist(H, J)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	C > 0,
	plus(K, 1, C),
	lenlist(B, K).
'CHRlenlist_2'(lenlist(A, B), C, D, E) ?-
	coca(try_rule(E, lenlist(A, B), anonymous("8"), lenlist(F, G), replacement, ground(G), length(F, G))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("8"))),
	length(A, B).
'CHRlenlist_2'(lenlist(A, B), C, D, E) :-
	'CHRlenlist_2__11'(lenlist(A, B), C, D, E).
:- set_flag('CHRlenlist_2' / 4, leash, notrace).
:- current_macro('CHRlenlist_2' / 4, _4420, _4421, _4422) -> true ; define_macro('CHRlenlist_2' / 4, tr_chr / 2, [write]).
'CHRlenlist_2__11'(A, B, C, D) :-
	'CHRlenlist_2__12'(A, B, C, D).
:- set_flag('CHRlenlist_2__11' / 4, leash, notrace).
'CHRlenlist_2__12'(lenlist(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, lenlist(A, B)], 'CHRlenlist_2'(lenlist(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRlenlist_2__12' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
