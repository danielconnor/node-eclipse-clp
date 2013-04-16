
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(31, xfx, #).
constrained(_548) :- delayed_goals(_548, [_546|_547]).
delay ground(_563) if nonground(_563).
ground(_577).
geon(_598, _597, _596) :- geons(_598, [rect(_597, _596)]).
approx_walls([], []).
approx_walls([wall(#(_643, _667), #(_643, _663))|_636], [rect(#(_651, _667), #(_644, _663))|_635]) :- !, _651 is _643 - 1, _644 is _643 + 1, approx_walls(_636, _635).
approx_walls([wall(#(_733, _709), #(_729, _709))|_702], [rect(#(_733, _717), #(_729, _710))|_701]) :- !, _717 is _709 - 1, _710 is _709 + 1, approx_walls(_702, _701).
approx_walls([wall(#(_777, _776), #(_772, _771))|_759], [rect(#(_777, _776), #(_772, _771))|_758]) :- approx_walls(_759, _758).
intersect_geons(_1097, _1096, _1090) :- bagof(_1095, intersect_geon(_1097, _1096, _1095), _1090).
intersect_geon(_1191, _1178, rect(#(_1142, _1137), #(_1141, _1136))) :- member(rect(#(_1174, _1166), #(_1158, _1150)), _1191), member(rect(#(_1173, _1165), #(_1157, _1149)), _1178), _1142 is max(_1174, _1173), _1137 is max(_1166, _1165), _1141 is min(_1158, _1157), _1136 is min(_1150, _1149), _1142 =< _1141, _1137 =< _1136.



%%% Callables for geons / 2

'CHRlabel_with'(geons(A, B), C, D) ?-
	coca(try_clause(D, geons(A, B), geons(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_geons(A, B)).



%%% Prolog clauses for geons / 2

clause_geons(#(A, B), C) :-
	member(rect(#(D, E), #(F, G)), C),
	A is (D + F) / 2,
	B is (E + G) / 2.
:- current_macro(clause_geons / 2, _1477, _1478, _1479) -> true ; define_macro(clause_geons / 2, tr_chr / 2, [write]).
geons(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, geons(A, B))),
	'CHRgeons_2'(geons(A, B), D, E, C).



%%% Rules handling for geons / 2

'CHRgeons_2'(geons(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRgeons_2'(geons(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRgeons_2'(geons(A, B), C, D, E) :-
	'CHRgeons_2__1'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2' / 4, leash, notrace).
:- current_macro('CHRgeons_2' / 4, _1823, _1824, _1825) -> true ; define_macro('CHRgeons_2' / 4, tr_chr / 2, [write]).
'CHRgeons_2__1'(A, B, C, D) :-
	'CHRgeons_2__2'(A, B, C, D).
:- set_flag('CHRgeons_2__1' / 4, leash, notrace).
'CHRgeons_2__2'(geons(#(A, B), C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRgeons_2__2__3'(G, D, geons(#(A, B), C), E, F).
'CHRgeons_2__2'(geons(A, B), C, D, E) :-
	'CHRgeons_2__2__4'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2__2' / 4, leash, notrace).
'CHRgeons_2__2__3'(['CHRgeons_2'(geons(#(A, B), C), D, E, F)|G], H, geons(#(A, B), I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("0")), H, D, J, E),
	coca(try_double(K, geons(#(A, B), I), F, geons(#(A, B), C), geons(#(L, M), N), geons(#(L, M), O), augmentation, (true, 'CHRkeep_heads_checking'(geons(#(L, M), N), P, geons(#(L, M), O), Q, geons(#(L, M), R), S)), (intersect_geons(N, O, R), R \== [], ('CHRhead_not_kept'(S) -> geons(#(L, M), R) ; true)), anonymous("0"))),
	no_global_bindings((true, 'CHRkeep_heads_checking'(geons(#(A, B), I), H, geons(#(A, B), C), D, geons(#(A, B), T), U)), (geons(#(A, B), I), geons(#(A, B), C))),
	!,
	coca(fired_rule(anonymous("0"))),
	'CHRgeons_2__2__3'(G, H, geons(#(A, B), I), J, K),
	intersect_geons(I, C, T),
	T \== [],
	(
	    'CHRhead_not_kept'(U)
	->
	    geons(#(A, B), T)
	;
	    true
	).
'CHRgeons_2__2__3'([A|B], C, D, E, F) :-
	'CHRgeons_2__2__3'(B, C, D, E, F).
'CHRgeons_2__2__3'([], A, B, C, D) :-
	'CHRgeons_2__2__4'(B, A, C, D).
:- set_flag('CHRgeons_2__2__3' / 5, leash, notrace).
'CHRgeons_2__2__4'(geons(#(A, B), C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRgeons_2__2__4__5'(G, D, geons(#(A, B), C), E, F).
'CHRgeons_2__2__4'(geons(A, B), C, D, E) :-
	'CHRgeons_2__2__4__6'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2__2__4' / 4, leash, notrace).
'CHRgeons_2__2__4__5'(['CHRgeons_2'(geons(#(A, B), C), D, E, F)|G], H, geons(#(A, B), I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("0")), H, D, J, E),
	coca(try_double(K, geons(#(A, B), I), F, geons(#(A, B), C), geons(#(L, M), N), geons(#(L, M), O), augmentation, (true, 'CHRkeep_heads_checking'(geons(#(L, M), O), P, geons(#(L, M), N), Q, geons(#(L, M), R), S)), (intersect_geons(O, N, R), R \== [], ('CHRhead_not_kept'(S) -> geons(#(L, M), R) ; true)), anonymous("0"))),
	no_global_bindings((true, 'CHRkeep_heads_checking'(geons(#(A, B), C), D, geons(#(A, B), I), H, geons(#(A, B), T), U)), (geons(#(A, B), I), geons(#(A, B), C))),
	!,
	coca(fired_rule(anonymous("0"))),
	'CHRgeons_2__2__4__5'(G, H, geons(#(A, B), I), J, K),
	intersect_geons(C, I, T),
	T \== [],
	(
	    'CHRhead_not_kept'(U)
	->
	    geons(#(A, B), T)
	;
	    true
	).
'CHRgeons_2__2__4__5'([A|B], C, D, E, F) :-
	'CHRgeons_2__2__4__5'(B, C, D, E, F).
'CHRgeons_2__2__4__5'([], A, B, C, D) :-
	'CHRgeons_2__2__4__6'(B, A, C, D).
:- set_flag('CHRgeons_2__2__4__5' / 5, leash, notrace).
'CHRgeons_2__2__4__6'(geons(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, geons(A, B)], 'CHRgeons_2'(geons(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRgeons_2__2__4__6' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
