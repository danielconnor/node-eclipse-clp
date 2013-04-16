/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe Application Example
 *
 * $Id: eg_c_sendmore.c,v 1.2 1998/02/25 18:31:43 js10 Exp $
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

#define NVARS	8

main()
{
    dident	plus,times;
    ec_refs	Vars;
    pword	varlist;
    int		i, res;
    
    ec_init();
    plus = ec_did("+",2);
    times = ec_did("*",2);
    ec_exec_string("lib(fd)",0);

    Vars = ec_refs_create_newvars(NVARS);
    varlist = ec_listofrefs(Vars);
    ec_post_goal(
	ec_term(ec_did("::",2),
	    varlist, ec_term(ec_did("..",2), ec_long(0), ec_long(9)))
    );
    ec_post_goal(
	ec_term(ec_did("alldistinct",1), varlist)
    );
    ec_post_goal(
	ec_term(ec_did("##",2), ec_refs_get(Vars,0), ec_long(0))
    );
    ec_post_goal(
	ec_term(ec_did("##",2), ec_refs_get(Vars,4), ec_long(0))
    );
    ec_post_goal(
	ec_term(ec_did("#=",2),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,0)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,1)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,2)),
	    ec_term(plus,
		ec_refs_get(Vars,3),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,4)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,5)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,6)),
		ec_refs_get(Vars,1)
	    ))))))),
	    ec_term(plus,
		ec_term(times, ec_long(10000), ec_refs_get(Vars,4)),
	    ec_term(plus,
		ec_term(times, ec_long(1000), ec_refs_get(Vars,5)),
	    ec_term(plus,
		ec_term(times, ec_long(100), ec_refs_get(Vars,2)),
	    ec_term(plus,
		ec_term(times, ec_long(10), ec_refs_get(Vars,1)),
		ec_refs_get(Vars,7)
	    ))))
	)
    );
    ec_post_goal(
	ec_term(ec_did("labeling",1), varlist)
    );

    res = ec_resume1(0);		/* solve */

    if (res == PSUCCEED)		/* print solution */
    {
	for (i=0; i<NVARS; i++)
	{
	    long sol;
	    res = ec_get_long(ec_refs_get(Vars,i), &sol);
	    if (res == PSUCCEED)
		printf("X%d = %d\n", i, sol);
	    else
		printf("X%d = ?\n");
	}
    }
    else printf("No solution\n");

    ec_refs_destroy(Vars);
    ec_cleanup();
}

