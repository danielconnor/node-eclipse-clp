%
% Version of the GAP-based SBDS library specialised for IC.
%
% See generic_gap_sbds.ecl for details.
%

:- module(ic_gap_sbds).

:- lib(ic).
:- lib(sym_expr).

:- lib(ic_generic_interface).
:- import get_bounds/3 from ic.

trans_var_to_dom_list(var_to_dom_list(Var, Dom), get_domain_as_list(Var, Dom)).
:- local macro(var_to_dom_list/2, trans_var_to_dom_list/2, []).

trans_module(set_module, ic_sets).
:- local macro(set_module/0, trans_module/2, []).

:- include(generic_gap_sbds).
 
