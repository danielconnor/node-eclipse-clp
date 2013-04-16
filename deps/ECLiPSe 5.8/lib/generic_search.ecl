% ----------------------------------------------------------------------
% 
% Generic search routine and search utilities for fd/ic problems
%
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	(C) Parc Technologies Ltd 2000
% Author/s:	Helmut Simonis, Parc Technologies Ltd
%               Joachim Schimpf, IC-Parc
%               Kish Shen, IC-Parc
% Version:	$Id: generic_search.ecl,v 1.10 2004/11/17 20:13:57 wh Exp $
%
% ----------------------------------------------------------------------

% TO-DO: generise to floats for IC, other solvers (e.g. fd_sets)

:-export search/6.


% Declare the daVinci predicates that are called from fd_search.
% Don't load the daVinci library yet because it may not be available.
% It is loaded at runtime, just before daVinci_begin/0 is called.
:- import
	daVinci_begin/0,
	daVinci_end/0,
	daVinci_node/2,
	daVinci_edge/3,
	daVinci_node_attribute/3
    from daVinci.

:-local variable(backtrack).
:-local variable(backtrack_limit).
:-local variable(one_level).
:-local variable(nodes).
:-local variable(node_limit).


/***********************************************************************

top level entry

***********************************************************************/

% search(+List:list,
%        ++Arg:integer,
%	++Select:atom,
%	+Choice:atom,
%	++Method:term,
%	?Option:list of options
%	++Module)
% search/6
% most predicates have a Module argument at the end, in order to pass the 
% caller module name to the meta-call predicates
%
:-tool(search/6,search_body/7).
search_body(L,Arg,Select,Choice,Method,Option,Module):-
	is_list(L),
	integer(Arg),
	callable(Select),
	callable(Choice),
	is_search_method(Method),
	is_list(Option),
	!,
	reset_backtrack_count(Option),
	in_out(Choice,In,Out),
	(option_for_daVinci(Option) ->
	    use_module(library(daVinci)),
	    daVinci_begin % only called for the right option
	;
	    true
	),
	tree_option(Option,Node_option,Root_node,Module),
	% top-level block to handle the limited number of nodes
	block(search1(L,Arg,Select,Choice,Method,In,Out,Node_option,Root_node,Module),nodes,search_nodes_failed),
	(option_for_daVinci(Option) ->
	    daVinci_end % only called for the right option
	;
	    true
	),
	get_backtrack_count(Option).
search_body(L,Arg,Select,Choice,Method,Option,Module):-
	error(5, search(L,Arg,Select,Choice,Method,Option), Module).

% called when the number of search nodes is exceeded
search_nodes_failed:-
	writeln(search_nodes_exceeded),
	fail.

% branch one the different search methods
search1(L,Arg,Select,Choice,complete,In,Out,Node_option,Root_node,Module):-
	labeling(L,Arg,Select,Choice,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,sbds,In,Out,Node_option,Root_node,Module):-
	sbds(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,gap_sbds,In,Out,Node_option,Root_node,Module):-
	gap_sbds(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,gap_sbdd,In,Out,Node_option,Root_node,Module):-
	gap_sbdd(L,Arg,Select,Choice,In,Out,Node_option,Root_node,Module).
search1(L,Arg,Select,Choice,bbs(Steps),In,Out,Node_option,Root_node,Module):-
	bbs(L,Arg,Select,Choice,Steps,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,credit(Credit,Steps),In,Out,Node_option,Root_node,Module):-
	credit(L,Arg,Select,Choice,Credit,Steps,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,lds(Disc),In,Out,Node_option,Root_node,Module):-
	lds(L,Arg,Select,Choice,Disc,In,Out,Node_option, Root_node,Module).
search1(L,Arg,Select,Choice,dbs(Level,Steps),In,Out,Node_option,Root_node,Module):-
	dbs(L,Arg,Select,Choice,Level,Steps,In,Out,Node_option, Root_node,Module).

is_search_method(complete) :- !.
is_search_method(sbds) :- !.
is_search_method(gap_sbds) :- !.
is_search_method(gap_sbdd) :- !.
is_search_method(bbs(N)) :- integer(N), !.
is_search_method(credit(N,M)) :- integer(N), integer(M), !.
is_search_method(credit(N,bbs(M))) :- integer(N), integer(M), !.
is_search_method(credit(N,lds(M))) :- integer(N), integer(M), !.
is_search_method(lds(N)) :- integer(N), !.
is_search_method(dbs(N,M)) :- integer(N), integer(M), !.
is_search_method(dbs(N,bbs(M))) :- integer(N), integer(M), !.
is_search_method(dbs(N,lds(M))) :- integer(N), integer(M), !.

is_list([]) ?- true.
is_list([_|_]) ?- true.

callable(C) :- atom(C).
callable(C) :- compound(C).


/***********************************************************************

different search methods

***********************************************************************/


% labeling(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode labeling(+,++,++,+,?,?,?,++,++).
labeling(L,Arg,Select,Choice,In,Out,Node_option, Node,Module):-
	labeling1(L,Arg,Select,Choice,In,Out,Node_option, Node,Module).


:-mode labeling1(+,++,++,+,?,?,?,++,++).
labeling1([],_,_,_,In,In,_Tree,_Node,_Module).
labeling1([H|T],Arg,Select,Choice,In,Out,Node_option, Node,Module):-
	delete(X,[H|T],R,Arg,Select,Module),
	tree_fixed(X,Node_option,Arg,Choice,Node,complete),
	choose(X,Arg,Choice,In,In1,Module),
	inc_backtrack_count,
	tree_node(X,Node_option, Node,Node1,Module),
	labeling1(R,Arg,Select,Choice,In1,Out,Node_option, Node1,Module).

% sbds(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode sbds(+,++,++,+,?,?,?,++,++).
sbds(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    sbds_var_list(L,Arg),
	    ( sbds_indomain_choice(Choice) ->
		SBDSChoice = sbds(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbds_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDSChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDSChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,sbds,[]), Module)
	).

    % Check the search list only contains SBDS variables (or non-variables).
sbds_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; sbds_module:is_sbds_var(Var)))
	).

% gap_sbds(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode gap_sbds(+,++,++,+,?,?,?,++,++).
gap_sbds(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    gap_sbds_var_list(L,Arg),
	    ( gap_sbds_indomain_choice(Choice) ->
		SBDSChoice = gap_sbds(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbds_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDSChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDSChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,gap_sbds,[]), Module)
	).

    % Check the search list only contains SBDS variables (or non-variables).
gap_sbds_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; ic_gap_sbds:is_sbds_var(Var)))
	).

% gap_sbdd(+List:list,
%           ++Arg:integer,
%	   ++Select:atom,
%	   +Choice:atom or p/2,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	   ++Module:atom)
%
:-mode gap_sbdd(+,++,++,+,?,?,?,++,++).
gap_sbdd(L,Arg,Select,Choice,In,Out,Node_option,Node,Module):-
	(
	    gap_sbdd_var_list(L,Arg),
	    ( gap_sbdd_indomain_choice(Choice) ->
		SBDDChoice = gap_sbdd(Choice)
	    ;
		% If it's not an explicitly supported built-in method, it
		% must be a user-supplied predicate (with the assumption
		% that the user has used sbdd_try/2 for labelling, as
		% required).
		\+ translate_indomain_atom(Choice, _),
		SBDDChoice = Choice
	    )
	->
	    labeling(L,Arg,Select,SBDDChoice,In,Out,Node_option,Node,Module)
	;
	    error(5, search(L,Arg,Select,Choice,gap_sbdd,[]), Module)
	).

    % Check the search list only contains SBDD variables (or non-variables).
gap_sbdd_var_list(L,Arg) :-
	(
	    foreach(X,L),
	    param(Arg)
	do
	    access(X,Arg,Var),
	    once((nonvar(Var) ; ic_gap_sbdd:is_sbdd_var(Var)))
	).


% bbs(+List:list,
%        ++Arg:integer,
%	++Select:atom,
%	+Choice:atom or p/2,
%	++Steps:integer,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%	++Module:atom)
% same as labeling, but stops after Steps backtracking steps
%
:-mode bbs(+,++,++,+,++,?,?,?,++,++).
bbs(L,Arg,Select,Choice,Steps,In,Out,Node_option, Node,Module):-
	getval(backtrack, CurrentBacktracks),
	BacktrackLimit is CurrentBacktracks+Steps,
	setval(backtrack_limit,BacktrackLimit),
	block(bbs1(L,Arg,Select,Choice,In,Out,Node_option, Node,Module),bbs,fail).

:-mode bbs1(+,++,++,+,?,?,?,++,++).
bbs1([],_,_,_,In,In,_Tree,_Node,_Module).
bbs1([H|T],Arg,Select,Choice,In,Out,Node_option, Node,Module):-
	delete(X,[H|T],R,Arg,Select,Module),
	tree_fixed(X,Node_option,Arg,Choice,Node,bbs),
	choose(X,Arg,Choice,In,In1,Module),
	inc_backtrack_count_check,
	tree_node(X,Node_option, Node,Node1,Module),
	bbs1(R,Arg,Select,Choice,In1,Out,Node_option, Node1,Module).


% credit(+List:list,++Arg:integer,++Select:atom,+Choice:atom or p/2,
%	 ++Credit:integer,
%	 ++Extra:integer or bbs(integer) or lds(integer),
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++ Module)
% same as labeling, but uses credit to control search
% always give half the credit to the first child,
% half of the remaining credit to the next child, etc

:-mode credit(+,++,++,+,++,++,?,?,?,++,++).
credit(L,Arg,Select,Choice,Credit,Extra,In,Out,Node_option, Node,Module):-
	length(L,N),
	shelf_create(credit/N,0,Shelf),
	credit1(L,Arg,Select,Choice,Credit,Extra,1,Shelf,In,Out,Node_option, Node,Module).

:-mode credit1(+,++,++,+,++,++,++,++,?,?,?,++,++).
credit1([],_,_,_,_,_,_,_,In,In,_Tree,_Node,_Module).
credit1([H|T],Arg,Select,Choice,1,Extra,_Level,_Shelf,In,Out,Node_option, Node,Module):-
	integer(Extra),
	!,
	bbs([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1([H|T],Arg,Select,Choice,1,bbs(Extra),_Level,_Shelf,In,Out,Node_option, Node,Module):-
	!,
	bbs([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1([H|T],Arg,Select,Choice,1,lds(Extra),_Level,_Shelf,In,Out,Node_option, Node,Module):-
	!,
	lds([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
credit1([H|T],Arg,Select,Choice,Credit,Extra,Level,Shelf,In,Out,Node_option, Node,Module):-
	Credit > 1,
	delete(X,[H|T],R,Arg,Select,Module),
	set_credit(Shelf,Level,Credit),
	tree_fixed(X,Node_option,Arg,Choice,Node,credit),
	choose(X,Arg,Choice,In,In1,Module),
	inc_backtrack_count,
	tree_node(X,Node_option, Node,Node1,Module),
	distribute_credit(Shelf,Level,Credit_child,Rest),
	(Rest = 0 ->
	    !  % cut away remaining choices in choose
	;
            true
	),
	Level1 is Level+1,
	credit1(R,Arg,Select,Choice,Credit_child,Extra,Level1,Shelf,In1,Out,Node_option, Node1,Module).

:-mode set_credit(++,++,++).
set_credit(Shelf,Level,Credit):-
	shelf_set(Shelf,Level,Credit).

% the credit distribution
% always give (a bit more than) half the credit to the next child
% keep the rest of the credit for the other children
% do not use up credit yourself
% if credit remains, and there are no more children, the credit is lost
% if children do not use their credit, it is lost
:-mode distribute_credit(++,++,++,-).
distribute_credit(Shelf,Level,Credit,Rest):-
	shelf_get(Shelf,Level,Old),
	Credit is (Old+1)//2,
	Rest is Old-Credit,
	shelf_set(Shelf,Level,Rest).


% lds(+List:list,++Arg:integer,++Select:atom,++Choice:atom,
%	 ++LDS:integer,
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++Module:atom)
% same as labeling, but only allows max LDS discrepancies against heuristic
% solution
% first tries 0, then 1, then 2, up to LDS discrepancies
%

:-mode lds(+,++,++,+,++,?,?,?,++,++).
lds(L,Arg,Select,Choice,Lds,In,Out,Node_option, Node,Module):-
	length(L,N),
	shelf_create(lds/N,0,Shelf),
	Disc :: 0..Lds,
	indomain(Disc),
	lds1(L,Arg,Select,Choice,Disc,1,Shelf,In,Out,Node_option, Node,Module).

:-mode lds1(+,++,++,+,++,++,++,?,?,?,++,++).
lds1([],_,_,_,0,_,_,In,In,_Tree,_Node,_Module). % do not allow to use less than given discrepancies
lds1([H|T],Arg,Select,Choice,0,Level,Shelf,In,Out,Node_option, Node,Module):-
	!,
	delete(X,[H|T],R,Arg,Select,Module),
	tree_fixed(X,Node_option,Arg,Choice,Node,lds_0),
	once(choose(X,Arg,Choice,In,In1,Module)), % allows only shallow backtracking
	update_nodes_counter, % create new node name
	tree_node(X,Node_option, Node,Node1,Module),
	Level1 is Level+1,
	lds1(R,Arg,Select,Choice,0,Level1,Shelf,In1,Out,Node_option, Node1,Module).
lds1([H|T],Arg,Select,Choice,Disc,Level,Shelf,In,Out,Node_option, Node,Module):-
	delete(X,[H|T],R,Arg,Select,Module),
	set_discrepancy(Shelf,Level,Disc),
	tree_fixed(X,Node_option,Arg,Choice,Node,lds),
	choose(X,Arg,Choice,In,In1,Module),
	inc_backtrack_count,
	tree_node(X,Node_option, Node,Node1,Module),
	inc_discrepancy(Shelf,Level),
	(test_discrepancy(Shelf,Level,Disc1) ->
	    true
	;
	    !,  % cut away remaining choices in choose
	    Disc1 = 0
	),
	Level1 is Level+1,
	lds1(R,Arg,Select,Choice,Disc1,Level1,Shelf,In1,Out,Node_option, Node1,Module).

:-mode set_discrepancy(++,++,++).
set_discrepancy(Shelf,Level,Disc):-
	shelf_set(Shelf,Level,Disc).


:-mode test_discrepancy(++,++,++).
test_discrepancy(Shelf,Level,Disc):-
	shelf_get(Shelf,Level,Disc),
	Disc > 0.

:-mode inc_discrepancy(++,++).
inc_discrepancy(_Shelf,_Level).
inc_discrepancy(Shelf,Level):-
	shelf_get(Shelf,Level,Old),
	New is Old -1,
	shelf_set(Shelf,Level,New),
	fail.

% dbs(+List:list,++Arg:integer,++Select:atom,+Choice:atom,
%	 ++Level:integer,
%	 ++Extra:integer or bbs(integer) or lds(integer),
%	   ?In,
%	   ?Out,
%	   ?Node_option,
%	   ++Node:integer,
%        ++ Module:atom)
% same as labeling, but uses depth bounded search to control search
% explore all choice points in the first Level variables

:-mode dbs(+,++,++,+,++,++,?,?,?,++,++).
dbs(L,Arg,Select,Choice,Level,Extra,In,Out,Node_option, Node,Module):-
	dbs1(L,Arg,Select,Choice,Level,Extra,In,Out,Node_option, Node,Module).

:-mode dbs1(+,++,++,+,++,++,?,?,?,++,++).
dbs1([],_,_,_,_,_,In,In,_Tree,_Node,_Module).
dbs1([H|T],Arg,Select,Choice,0,Extra,In,Out,Node_option, Node,Module):-
	integer(Extra),
	!,
	bbs([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs1([H|T],Arg,Select,Choice,0,bbs(Extra),In,Out,Node_option, Node,Module):-
	!,
	bbs([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs1([H|T],Arg,Select,Choice,0,lds(Extra),In,Out,Node_option, Node,Module):-
	!,
	lds([H|T],Arg,Select,Choice,Extra,In,Out,Node_option, Node,Module).
dbs1([H|T],Arg,Select,Choice,Level,Extra,In,Out,Node_option, Node,Module):-
	Level >= 1,
	delete(X,[H|T],R,Arg,Select,Module),
	tree_fixed(X,Node_option,Arg,Choice,Node,dbs),
	choose(X,Arg,Choice,In,In1,Module),
	inc_backtrack_count,
	tree_node(X,Node_option, Node,Node1,Module),
	Level1 is Level-1,
	dbs1(R,Arg,Select,Choice,Level1,Extra,In1,Out,Node_option, Node1,Module).

/***********************************************************************

value choice

***********************************************************************/

% choose(?X,++Arg:integer,++Method:atom,?In,?Out,++Module:atom)
% this predicate chooses a value for the selected term
% this choice is non-deterministic
% for the user defined case, the whole selected term is passed so that
% the user-predicate can assign more than one argument inside
%
:-mode choose(?,++,++,?,?,++).
choose(X,N,indomain,_In, _Out, _Module):-
	!,
	access(X,N,Var),
	indomain(Var).
choose(X,N,Type,_In, _Out, _Module):-
	translate_indomain_atom(Type, IndomainType),
	!,
	access(X,N,Var),
	indomain(Var,IndomainType).
choose(X,_Arg,Method,_In,_Out,Module):- % this is called for a user-defined method
	atom(Method),
	!,
	Call =.. [Method,X],
	call(Call)@Module. % may be non-deterministic
choose(X,_Arg,Method,In,Out,Module):- % this is called for a user-defined method
	functor(Method,F,2),
	Call =.. [F,X,In,Out],
	call(Call)@Module. % may be non-deterministic


/************************************************************

utilities

************************************************************/

% Translate search/6's indomain choice atoms to those used by indomain/2
translate_indomain_atom(indomain, min).		% Well, kind of
translate_indomain_atom(indomain_min, min).
translate_indomain_atom(indomain_max, max).
translate_indomain_atom(indomain_middle, middle).
translate_indomain_atom(indomain_median, median).
translate_indomain_atom(indomain_split, split).
translate_indomain_atom(indomain_interval, interval).
translate_indomain_atom(indomain_random, random).
translate_indomain_atom(sbds(Choice), Atom) :-
	translate_indomain_atom_sbds(Choice, Atom).
translate_indomain_atom(gap_sbds(Choice), Atom) :-
	translate_indomain_atom_gap_sbds(Choice, Atom).
translate_indomain_atom(gap_sbdd(Choice), Atom) :-
	translate_indomain_atom_gap_sbdd(Choice, Atom).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is SBDS.
translate_indomain_atom_sbds(indomain_min, sbds_min).
translate_indomain_atom_sbds(indomain_max, sbds_max).
translate_indomain_atom_sbds(indomain_middle, sbds_middle).
translate_indomain_atom_sbds(indomain_median, sbds_median).
translate_indomain_atom_sbds(indomain_random, sbds_random).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is GAP SBDS.
translate_indomain_atom_gap_sbds(indomain_min, gap_sbds_min).
translate_indomain_atom_gap_sbds(indomain_max, gap_sbds_max).
translate_indomain_atom_gap_sbds(indomain_middle, gap_sbds_middle).
translate_indomain_atom_gap_sbds(indomain_median, gap_sbds_median).
translate_indomain_atom_gap_sbds(indomain_random, gap_sbds_random).

% Translate search/6's indomain choice atoms to those used by indomain/2
% when the search method is GAP SBDD.
translate_indomain_atom_gap_sbdd(indomain_min, gap_sbdd_min).
translate_indomain_atom_gap_sbdd(indomain_max, gap_sbdd_max).
translate_indomain_atom_gap_sbdd(indomain_middle, gap_sbdd_middle).
translate_indomain_atom_gap_sbdd(indomain_median, gap_sbdd_median).
translate_indomain_atom_gap_sbdd(indomain_random, gap_sbdd_random).

% Defines the allowable value selection methods to be used with SBDS
sbds_indomain_choice(Method) :-
	translate_indomain_atom_sbds(Method, _).

% Defines the allowable value selection methods to be used with GAP SBDS
gap_sbds_indomain_choice(Method) :-
	translate_indomain_atom_gap_sbds(Method, _).

% Defines the allowable value selection methods to be used with GAP SBDD
gap_sbdd_indomain_choice(Method) :-
	translate_indomain_atom_gap_sbdd(Method, _).

% access argument N of term X, if N=0, X is returned
:-mode access(?,++,-).
access(X,0,X) :- !.
access(X,N,Var):-
	N > 0,
	arg(N,X,Var).

% Initialize the accumulator variable for the search choice
% this is only used if Choose is a functor of arity 2
:-mode in_out(?,-,-).
in_out(T,In,Out):-
	functor(T,_,2),
	!,
	arg(1,T,In),
	arg(2,T,Out).
in_out(_T,-,-).

reset_backtrack_count(Option):-
	nr_nodes(Option,N),
	setval(node_limit,N),
	setval(nodes,0),
	setval(backtrack,0).

nr_nodes(Option,N):-
	member(nodes(N),Option),
	!.
nr_nodes(Option,2000):-
	member(node(_),Option),
	!.
nr_nodes(_Option,none).

	
:-mode get_backtrack_count(?).
get_backtrack_count(L):-
	memberchk(backtrack(N),L), !,
	getval(backtrack,N).
get_backtrack_count(_).

inc_backtrack_count:-
	update_nodes_counter,
	setval(one_level,true).
inc_backtrack_count:-
	getval(one_level,true),
	setval(one_level,false),
	incval(backtrack),
	fail.

inc_backtrack_count_check :-
	update_nodes_counter,
	setval(one_level,true).
inc_backtrack_count_check :-
	getval(one_level,true),
	setval(one_level,false),
	incval(backtrack),
	( getval(backtrack) =< getval(backtrack_limit) ->
	    fail
	;
	    exit_block(bbs)
	).

update_nodes_counter:-
	incval(nodes),
	getval(node_limit, Max),
	( integer(Max), getval(nodes) >= Max ->
%	    writeln(jumping(nodes)),
	    exit_block(nodes)
	;
	    true
	).


/*
value_range([H|T],Arg,Range):-
	access(H,Arg,H1),
	value_range(T,H1,Msg),
	dom(Msg,Range).

value_range([],X,X).
value_range([H|T],Old,End):-
	access(H,Arg,H1),
	dvar_msg(H1,Old,New),
	value_range(T,New,End).

*/

/*

this checks the option list to find the node(_) option
it also creates the root node with parent node -1
and the initial node number is returned

if no node option is used, then it uses none for call and node number
*/

:-mode tree_option(?,-,-,++).
tree_option(Option,none,none,_Module):-
	var(Option),
	!,
	writeln(option_should_be_nonvar),
	abort.
tree_option(Option,Call,Node,Module):-
	member(node(Call),Option),
	!,
	tree_node(root,Call,-1,Node,Module).
tree_option(_Option,none,none,_Module).

/*

this predicate is called after the choice point
depending on the type of node action, different routines are called

returns the newly generated node name
*/

:-mode tree_node(?,+,++,-,++).
tree_node(_X,none,none,none,_Module):- % this case if no node option is used
	!.
tree_node(X,daVinci,Parent,Node,_Module):- % display the complete term
	integer(Parent),
	!,
	getval(nodes,Node),
	daVinci_node(Node,X),
	(Parent \= -1 ->
	    daVinci_edge(Node,Parent,Node)
	;
	    true
	).
tree_node(X,daVinci(Info_pred),Parent,Node,Module):- % display only Info returned
	integer(Parent),
	!,
	getval(nodes,Node),
	Goal =.. [Info_pred,X,Info],
	call(Goal)@Module,
	daVinci_node(Node,Info),
	(Parent \= -1 ->
	    daVinci_edge(Node,Parent,Node)
	;
	    true
	).
tree_node(X,Call,Parent,Node,Module):- % custom routine
	integer(Parent),
	!,
	getval(nodes,Node),
	make_call(Call,daVinci,X,Parent,Node,Goal),
	call(Goal)@Module.
tree_node(X,Call,Parent,Node,Module):- % catch error in calling pattern
	writeln(wrong_tree_node(X,Call,Parent,Node,Module)),
	abort.

/*
make a meta-call by appending some attributes to the given term
only handles up to 3 arguments
*/
:-mode make_call(+,++,?,++,++,-).
make_call(Call,S,X,Parent,Node,Goal):-
	atom(Call),
	!,
	Goal =.. [Call,S,X,Parent,Node].
make_call(Call,S,X,Parent,Node,Goal):-
	Call =.. [Pred,Arg1],
	!,
	Goal =.. [Pred,Arg1,S,X,Parent,Node].
make_call(Call,S,X,Parent,Node,Goal):-
	Call =.. [Pred,Arg1,Arg2],
	!,
	Goal =.. [Pred,Arg1,Arg2,S,X,Parent,Node].
make_call(Call,S,X,Parent,Node,Goal):-
	Call =.. [Pred,Arg1,Arg2,Arg3],
	!,
	Goal =.. [Pred,Arg1,Arg2,Arg3,S,X,Parent,Node].

/*

this predicate is called before the value choice part
it decides on some attributes of the current node

*/
:-mode tree_fixed(?,+,++,+,++,++).
tree_fixed(_X,none,_Arg,_Choice,_Node,_Type):-
	!.
tree_fixed(X,_Node_option,Arg,Choice,Node,Type):-
	translate_indomain_atom(Choice, _),	% Test for predefined choice
	!,
	access(X,Arg,Var),
	fixed_style(Node,Var,Type).
tree_fixed(X,_Node_option,Arg,_Choice,Node,_Type):-
	access(X,Arg,Var),
	fixed_style(Node,Var,custom).

fixed_style(Node,Var,Type):-
	type_color(Type,Color),
	daVinci_node_attribute(Node,'COLOR',Color),
	(integer(Var) ->
	    daVinci_node_attribute(Node,'FONTSTYLE',italic)
	;
	    true
	).
fixed_style(Node,_Var,_Type):-
	daVinci_node_attribute(Node,'BORDER','double'),
	fail.
/*

data for the mapping of search type to color

custom means that it is a custom assignment routine, not a custom search routine

*/

:-mode type_color(++,-).
type_color(complete,white).
type_color(credit,goldenrod).
type_color(lds,lightgray).
type_color(lds_0,gray). % this is a lds branch were all discrepancies have been used
type_color(bbs,white).
type_color(dbs,pink).
type_color(custom,orchid).

/*

test if we are in a daVinci based operation

*/

option_for_daVinci(Option):-
	member(node(daVinci),Option),
	!.
option_for_daVinci(Option):-
	member(node(daVinci(_)),Option),
	!.



/***********************************************************************

variable selection 

***********************************************************************/

:-export(delete/5).
:-tool(delete/5, delete/6).

% delete(-X,+List:non_empty_list,-R:list,++Arg:integer,++Select:atom,
%            ++Module:atom)
% choose one entry in the list based on a heuristic
% this is a deterministic selection
% a special case for input order to speed up the selection in that case
%
:-mode delete(-,+,-,++,++,++).
delete(H,[H|T],T,_Arg,input_order,_Module):-
	!.
delete(X,[H|T],R,Arg,Select,Module):-
	find_criteria(H,Arg,Select,Crit,Module),
	find_best_and_rest(T,H,Crit,T,X,Rest,Arg,Select,Module), % scan list once
	copy_until([H|T],Rest,R). % scan list again

% find_best_and_rest(+List:list,?Old,
%		     +Old_value:integer or crit(integer,integer),
%		     +Rest_old:list,-Best,-Rest_best:list,
%		     ++Arg:integer,++Select:atom,
%                    ++Module:atom)
%
:-mode find_best_and_rest(+,?,++,+,-,-,++,++,++).
find_best_and_rest([],Old,_Crit_old,Rest_old,Old,Rest_old,_Arg,_,_Module).
find_best_and_rest([H|T],_Old,Crit_old,_Rest_old,X,R,Arg,Select,Module):-
	find_criteria(H,Arg,Select,Crit_h,Module),
	Crit_h @< Crit_old, % sometimes crit values are terms
	!,
	find_best_and_rest(T,H,Crit_h,T,X,R,Arg,Select,Module).
find_best_and_rest([_H|T],Old,Crit_old,Rest_old,X,R,Arg,Select,Module):-
	find_best_and_rest(T,Old,Crit_old,Rest_old,X,R,Arg,Select,Module).

% find_criteria(?Term,++Arg:integer,++Select:atom,
%		-Crit:integer or crit(integer,integer),
%               ++Module:atom)
%
% find a heuristic value from a term
:-mode find_criteria(?,++,++,-,++).
find_criteria(Term,0,Select,Crit,Module):-
	!,
	find_value(Term,Select,Crit,Module).
find_criteria(Term,Arg,Select,Crit,Module):-
	arg(Arg,Term,X),
	find_value(X,Select,Crit,Module).

% find_value(?X:dvarint,++Select:atom,
%	     -Crit:integer or crit(integer,integer),
%            ++Module:atom)
%
% find a heuristic value from a domain variable
% the smaller the value, the better
:-mode find_value(?,++,-,++).
find_value(X,first_fail,Size,_Module):-
	!,
	get_size(X,Size).
find_value(X,anti_first_fail,Number,_Module):-
	!,
	get_size(X,Size),
	Number is -Size.
find_value(X,smallest,Size,_Module):-
	!,
	get_lwb(X,Size).
find_value(X,largest,Size,_Module):-
	!,
	get_upb(X,Max),
	Size is - Max.
find_value(X,occurence,Number,Module):-	% mis-spelt in first version
	!,
	find_value(X,occurrence,Number,Module).
find_value(X,occurrence,Number,_Module):-
	!,
	get_constraints_number(X,Nr), % this is very heavy
	Number is -Nr.
find_value(X,max_regret,0,_Module):-
	integer(X),
        !.
find_value(X,max_regret,Number,_Module):- % you could argue for -inf
	!,
	get_compact_domain_rep(X,L),
	nth_value(L,2,V),
	get_lwb(X,Min),
	Number is -(V-Min).
find_value(X,most_constrained,crit(Size,Number),Module):-
	!,
	find_value(X,first_fail,Size,Module),
	find_value(X,occurrence,Number,Module).
find_value(X,User_method,Value,Module):-
	Call =..[User_method,X,Value],
	once(Call)@Module.	% do not allow backtracking in user routine

% copy_until(+A:non_empty_list,+B:list,-C:list)
% C is equal to the list A with one element removed
% B is the tail of A (C)
% the value removed is the one just before B
:-mode copy_until(+,+,-).
copy_until([_H|T],Rest,T):-
	T == Rest, % this only scans the whole Rest once, when it succeeds
	!.
copy_until([H|T],Rest,[H|S]):-
	copy_until(T,Rest,S).





/****************************************************

some indomain variants

****************************************************/

:-export(indomain/2).

% indomain(?X:dvarint,++Type:atomic)
% Type is either one of min, max, middle or an integer
% these indomain versions remove the previous value on backtracking
:-mode indomain(?,++).
indomain(X,Type):-
	( is_integer_type(X) ->
	    indomain1(X,Type)
	;
	    error(5, indomain(X, Type))
	).

:-mode indomain1(?,++).
indomain1(X,min):-
	get_lwb(X,Min),
	indomain_min(X,Min).
indomain1(X,sbds_min):-
	sbds_indomain_min(X).
indomain1(X,gap_sbds_min):-
	gap_sbds_indomain_min(X).
indomain1(X,gap_sbdd_min):-
	gap_sbdd_indomain_min(X).
indomain1(X,max):-
	get_upb(X,Max),
	indomain_max(X,Max).
indomain1(X,sbds_max):-
	sbds_indomain_max(X).
indomain1(X,gap_sbds_max):-
	gap_sbds_indomain_max(X).
indomain1(X,gap_sbdd_max):-
	gap_sbdd_indomain_max(X).
indomain1(X,middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,Value).
indomain1(X,sbds_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,sbds(Value)).
indomain1(X,gap_sbds_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,gap_sbds(Value)).
indomain1(X,gap_sbdd_middle):-
	select_initial_value_middle(X,Value),
	indomain1(X,gap_sbdd(Value)).
indomain1(X,median):-
	select_initial_value_median(X,Value),
	indomain1(X,Value).
indomain1(X,sbds_median):-
	select_initial_value_median(X,Value),
	indomain1(X,sbds(Value)).
indomain1(X,gap_sbds_median):-
	select_initial_value_median(X,Value),
	indomain1(X,gap_sbds(Value)).
indomain1(X,gap_sbdd_median):-
	select_initial_value_median(X,Value),
	indomain1(X,gap_sbdd(Value)).
indomain1(X,split):-
	indomain_split(X).
indomain1(X,interval):-
	indomain_interval(X).
indomain1(X,random):-
	indomain_random(X).
indomain1(X,sbds_random):-
	sbds_indomain_random(X).
indomain1(X,gap_sbds_random):-
	gap_sbds_indomain_random(X).
indomain1(X,gap_sbdd_random):-
	gap_sbdd_indomain_random(X).
indomain1(X,Value):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    indomain_min(X,Min)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    indomain_max(X,Max)
	;
	    % enumerate from a starting value inside the domain
	    % is this enough in all cases ??
	    Range is 2*max(Max-Value,Value-Min)+1,
	    indomain_from(X,Value,1,Range)
	).
indomain1(X,sbds(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    sbds_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    sbds_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    sbds_indomain_from(X,Value,0)
	).
indomain1(X,gap_sbds(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    gap_sbds_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    gap_sbds_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    gap_sbds_indomain_from(X,Value,0)
	).
indomain1(X,gap_sbdd(Value)):-
	integer(Value),
	get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    gap_sbdd_indomain_min(X)
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    gap_sbdd_indomain_max(X)
	;
	    % enumerate from a starting value inside the domain
	    gap_sbdd_indomain_from(X,Value,0)
	).

    % translate middle into a starting value
select_initial_value_middle(X,Value) :-
	get_bounds(X,Min,Max),
	Value is (Min+Max)//2. % HS: remember to use integer division

    % translate median into a starting value
select_initial_value_median(X,Value) :-
	get_size(X,Size),
	Index is 1+Size//2,
	get_compact_domain_rep(X, L),
	nth_value(L,Index,Value).

% indomain_from(?X:dvar, ++Value:integer, ++Inc:integer, ++Range:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
% the next value is always the old value plus the increment
% the next increment is one bigger than the previous, but of opposite sign
% 1, -2, 3, -4, 5, -6, 7 ...
% if the increment becomes too large, you can stop
:-mode indomain_from(?,++,++,++).
indomain_from(X,X,_,_).
indomain_from(X,Value,Inc,Range):-
	X #\= Value,
	Value1 is Value+Inc,
	Inc1 is -sgn(Inc)*(abs(Inc)+1),
	Range >= abs(Inc1),
	indomain_from(X,Value1,Inc1,Range).

:-mode sbds_indomain_from(?,++,++).
sbds_indomain_from(X,_,_):-
	nonvar(X).
sbds_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    sbds_module:sbds_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	sbds_indomain_from(X,Value,Inc).

:-mode gap_sbds_indomain_from(?,++,++).
gap_sbds_indomain_from(X,_,_):-
	nonvar(X).
gap_sbds_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    ic_gap_sbds:sbds_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	gap_sbds_indomain_from(X,Value,Inc).

:-mode gap_sbdd_indomain_from(?,++,++).
gap_sbdd_indomain_from(X,_,_):-
	nonvar(X).
gap_sbdd_indomain_from(X,Value0,Inc0):-
	var(X),
	Value is Value0+(2*(Inc0 mod 2)-1)*Inc0,
	( check_in(Value, X) ->
	    % Don't want to call this if Value not in X's domain
	    ic_gap_sbdd:sbdd_try(X,Value)
	;
	    true
	),
	Inc is Inc0+1,
	gap_sbdd_indomain_from(X,Value,Inc).

% indomain_min(?X:dvar, ++Value:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
:-mode indomain_min(?,++).
indomain_min(X,X).
indomain_min(X,Min):-
	X #> Min,
	get_lwb(X,New),
	indomain_min(X,New).

sbds_indomain_min(X):-
	nonvar(X).
sbds_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	sbds_module:sbds_try(X,Min),
	sbds_indomain_min(X).

gap_sbds_indomain_min(X):-
	nonvar(X).
gap_sbds_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	ic_gap_sbds:sbds_try(X,Min),
	gap_sbds_indomain_min(X).

gap_sbdd_indomain_min(X):-
	nonvar(X).
gap_sbdd_indomain_min(X):-
	var(X),
	get_lwb(X,Min),
	ic_gap_sbdd:sbdd_try(X,Min),
	gap_sbdd_indomain_min(X).

% indomain_max(?X:dvar, ++Value:integer)
% the choice consists in either taking the proposed value or in excluding it
% and choosing another one
:-mode indomain_max(?,++).
indomain_max(X,X).
indomain_max(X,Max):-
	X #< Max,
	get_upb(X,New),
	indomain_max(X,New).

sbds_indomain_max(X):-
	nonvar(X).
sbds_indomain_max(X):-
	var(X),
	get_lwb(X,Max),
	sbds_module:sbds_try(X,Max),
	sbds_indomain_max(X).

gap_sbds_indomain_max(X):-
	nonvar(X).
gap_sbds_indomain_max(X):-
	var(X),
	get_lwb(X,Max),
	ic_gap_sbds:sbds_try(X,Max),
	gap_sbds_indomain_max(X).

gap_sbdd_indomain_max(X):-
	nonvar(X).
gap_sbdd_indomain_max(X):-
	var(X),
	get_lwb(X,Max),
	ic_gap_sbdd:sbdd_try(X,Max),
	gap_sbdd_indomain_max(X).

% split the domain into intervals until only an integer value is left
:-mode indomain_split(?).
indomain_split(X):-
	integer(X),
	!.
indomain_split(X):-
	get_bounds(X,Min,Max),
	Middle is (Min+Max)//2, % HS: remember to use integer division
	(
	    X #=< Middle
	;
	    X #> Middle
	),
	indomain_split(X).

% assign values by first choosing one interval from the domain and
% then assigning values from the middle of that domain
:-mode indomain_interval(?).
indomain_interval(X):-
	get_compact_domain_as_list(X,L),
	fix_interval(X,L).

:-mode fix_interval(?,++).
fix_interval(X,[A|_R]):-
	integer(A),
	X #= A.
fix_interval(X,[A|R]):-
	integer(A),
	X #\= A,
	fix_interval(X,R).
fix_interval(X,[_A..B|_R]):-
	X #=< B,
	indomain(X,split).  % there are many alternatives here
fix_interval(X,[_A..B|R]):-
	X #> B,
	fix_interval(X,R).

% choose values from the domain at random; on backtracking, the previous value
% is removed, so that it can be used for a complete enumeration
:-mode indomain_random(?).
indomain_random(X):-
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	indomain_random(X,Try).

:-mode indomain_random(?,++).
indomain_random(X,X).
indomain_random(X,Try):-
	X #\= Try,
	indomain_random(X).

sbds_indomain_random(X):-
	nonvar(X).
sbds_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	sbds_module:sbds_try(X,Try),
	sbds_indomain_random(X).

gap_sbds_indomain_random(X):-
	nonvar(X).
gap_sbds_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	ic_gap_sbds:sbds_try(X,Try),
	gap_sbds_indomain_random(X).

gap_sbdd_indomain_random(X):-
	nonvar(X).
gap_sbdd_indomain_random(X):-
	var(X),
	get_size(X,Size),
	random(V),
	Index is 1+ (V mod Size),
	get_compact_domain_rep(X,L),
	nth_value(L,Index,Try),
	ic_gap_sbdd:sbdd_try(X,Try),
	gap_sbdd_indomain_random(X).


/****************************************************

other useful stuff

****************************************************/

:-export(nth_value/3).

nth_value(V, 1, V) :-
	integer(V).
nth_value(I, N, V) :-
	I = _.._,
	nth_value1(I, [], N, V).
nth_value([I | R], N, V) :-
	nth_value1(I, R, N, V).

nth_value1(A..B, R, N, V) :-
	A1 is A + N - 1,
	N1 is A1 - B,
	( N1 > 0 ->
	    nth_value(R, N1, V)
	;
	    A1 >= A,
	    V = A1
	).
nth_value1(A, R, N, V) :-
	atomic(A),
	nth_value2(A, R, N, V).

nth_value2(A, _R, 1, V) :-
	!,
	V = A.
nth_value2(_A, R, N, V) :-
	N1 is N - 1,
	nth_value(R, N1, V).

