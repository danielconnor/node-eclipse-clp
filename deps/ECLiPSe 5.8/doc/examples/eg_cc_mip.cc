/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe Application Example
 *
 * $Id: eg_cc_mip.cc,v 1.1 1997/06/26 19:05:09 js10 Exp $
 *
 * IDENTIFICATION:	eg_sendmore.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * DESCRIPTION:
 *	Example of solving a constraint problem from C
 */

#include	"eclipseclass.h"
#include	<iostream.h>


#define NCOLS	3
#define NROWS	2

double req[NROWS][NCOLS] = {
    2.2, 1.8, 1.9,
    2.4, 2.0, 2.1
};

double pc[NCOLS] = {
    24.7, 22.4, 19.7
};




static void bounds(EC_word vars, double lb, double ub)
{
    post_goal(term(EC_functor("::",2),
	    vars,
	    term(EC_functor("..",2), lb, ub)));
}

static void eq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$=",2), lhs, rhs));
}

static void geq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$>=",2), lhs, rhs));
}

static void leq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$=<",2), lhs, rhs));
}

static void maximize(EC_word obj, EC_word objval)
{
    post_goal(term(EC_functor("optimize",2),
	term(EC_functor("max",1), obj), objval));
}


main()
{
    ec_init();
    {

	EC_ref	Profit;
	EC_refs	Vars(NCOLS);
	EC_ref	VarList;

	post_goal("lib(eplex)");

	VarList = list(Vars);

	bounds(VarList, 0.0, 1e20);

	leq( VarList * list(NCOLS,req[0]),	8.0);
	leq( VarList * list(NCOLS,req[1]),	10.0);

	maximize(VarList * list(NCOLS,pc), Profit);

	if (EC_resume() == EC_succeed)		/* solve */
	{
	    double d;
	    int i;

	    if (EC_word(Profit).is_double(&d) == EC_succeed)
		cout << "Profit is " << d << "\n";
	    else
		cout << "Profit is ?\n";

	    for (i=0; i<NCOLS; i++)
	    {
		if (Vars[i].is_double(&d) == EC_succeed)
		    cout << "X" << i << " = " << d << "\n";
		else
		    cout << "X" << i << " = ?\n";
	    }
	}
	else cout << "No solution\n";
    }
    ec_cleanup();
}

