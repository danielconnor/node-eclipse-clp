% bin_info.pl
% 
% Copyright 1995   IC-Parc, Imperial College:  Mark Wallace 
%

:- module(bin_info).

:- export get_bin_info/2.
:- export add_bin_info/2.

%:- meta_attribute(bin_info, [unify:unify_bin_info/2]).
:- meta_attribute(bin_info, [unify:unify_bin_info/2,print:print_bin_info/2]).

add_bin_info(Var,Bin_info) :-
	call(add_attribute(Var,Bin_info),bin_info).

unify_bin_info(_,Attr) :-
	var(Attr).
unify_bin_info(Term, Attr) :-
	nonvar(Attr),
	unify_term_bin_info(Term,Attr).

unify_term_bin_info(Value,_Attr) :-
	nonvar(Value).
unify_term_bin_info(Y{AttrY},AttrX) :-
	-?->
	unify_bin_info_bin_info(Y,AttrX,AttrY).

unify_bin_info_bin_info(_Y,AttrX,AttrY) :-
	var(AttrY), AttrX=AttrY.
unify_bin_info_bin_info(_Y,_AttrX,AttrY) :-
	nonvar(AttrY).

%print_bin_info(Bin_info,Bin_info).
print_bin_info(_{bin_info:Bin_info},OutBin_info) :-
	-?->
	OutBin_info=Bin_info.
/*
print_bin_info(Bin_info,OutBin_info) :-
	-?->
	OutBin_info=Bin_info.
*/
%get_bin_info(Val,Bin_info) :-
%	nonvar(Val), !,
%	Bin_info=Val.
get_bin_info(_Var{Bin_info},OutBin_info) :-
	-?->
	nonvar(Bin_info),
	Bin_info=OutBin_info.

