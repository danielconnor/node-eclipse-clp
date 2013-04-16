/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe Application Example
 *
 * $Id: eg_c_mip.c,v 1.1 1997/06/26 19:05:09 js10 Exp $
 *
 * IDENTIFICATION:	eg_sendmore.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * DESCRIPTION:
 *	Example of solving a constraint problem from C
 */

#include	"eclipse.h"


#define NCOLS	3
#define NROWS	2

double req[NROWS][NCOLS] = {
    2.2, 1.8, 1.9,
    2.4, 2.0, 2.1
};

double pc[NCOLS] = {
    24.7, 22.4, 19.7
};




static void bounds(vars, lb, ub)
pword vars;
double lb, ub;
{
    ec_post_goal(ec_term(
	ec_did("::",2),
	    vars,
	    ec_term(ec_did("..",2), ec_double(lb), ec_double(ub))));
}

static void eq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$=",2), lhs, rhs));
}

static void geq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$>=",2), lhs, rhs));
}

static void leq(lhs, rhs)
pword lhs, rhs;
{
    ec_post_goal(ec_term(ec_did("$=<",2), lhs, rhs));
}

static void maximize(obj, objval)
pword obj, objval;
{
    ec_post_goal( ec_term(ec_did("optimize",2),
	ec_term(ec_did("max",1), obj), objval));
}

static pword plus(lhs, rhs)
pword lhs, rhs;
{
    return ec_term(ec_did("+",2), lhs, rhs);
}

static pword times(lhs, rhs)
pword lhs, rhs;
{
    return ec_term(ec_did("*",2), lhs, rhs);
}


main()
{
    ec_refs	Vars;
    ec_ref	Profit;
    pword	varlist;
    
    ec_init();

    ec_post_string("lib(eplex)");

    Vars = ec_refs_create_newvars(NCOLS);
    Profit = ec_ref_create_newvar();
    varlist = ec_listofrefs(Vars);

    bounds(varlist, 0.0, 1e20);

    eq(  times(varlist, ec_listofdouble(NCOLS,pc)),	ec_ref_get(Profit));
    leq( times(varlist, ec_listofdouble(NCOLS,req[0])),	ec_double(8.0));
    leq( times(varlist, ec_listofdouble(NCOLS,req[1])),	ec_double(10.0));

    maximize( times(varlist, ec_listofdouble(NCOLS,pc)), ec_ref_get(Profit));

    if (ec_resume1(0) == PSUCCEED)		/* solve */
    {
	double d;
	int i;

	if (ec_get_double(ec_ref_get(Profit), &d) == PSUCCEED)
	    printf("Profit is %f\n", d);
	else
	    printf("Profit is ?\n");

	for (i=0; i<NCOLS; i++)
	{
	    if (ec_get_double(ec_refs_get(Vars,i), &d) == PSUCCEED)
		printf("X%d = %f\n", i, d);
	    else
		printf("X%d = ?\n");
	}
    }
    else printf("No solution\n");

    ec_refs_destroy(Vars);
    ec_ref_destroy(Profit);
    ec_cleanup();
}

