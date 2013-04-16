% ----------------------------------------------------------------------
%
% Description:	ECLiPSe column generation library
%
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	(C) IC-Parc, Imperial College London 1995-1999
% Author/s:	Andrew Eremin, IC-Parc
% Version:      $Id: colgen.ecl,v 1.6 2003/11/11 18:23:27 ae5 Exp $
%
% ----------------------------------------------------------------------



:- module(colgen_).
% ----------------------------------------------------------------------

:- include(colgen_).

% ----------------------------------------------------------------------
:- module(colgen).
% ----------------------------------------------------------------------

:- comment(include, colgen_comments).

:- reexport colgen_ except
   
        var_dual/6,
        get_dual/3,
        get_idx/3,
        get_rhs/3,
        set_dual/3,
   
        bp_solve/2,
        cg_solver_setup/3,
        cg_solver_setup/4,
        cg_integers/2,
        add_cg_pool_constraint/3,
	cg_eq/3,
	cg_ge/3,
	cg_le/3,
        cg_sp_count/2,
        cg_sp_sol/2,
        cg_sp_rc_sum/2,
        cg_minimize/4,
        cg_minimize/5,
        cg_var_get/4,
        cg_get/3.

%------------------------------------------------------------------------
