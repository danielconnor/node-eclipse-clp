/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_c_fail_loop.c,v 1.3 2004/05/05 16:16:52 js10 Exp $
 *
 *
 * IDENTIFICATION:	eg1.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipse.h"


main(int argc, char **argv)
{
    dident	p_1,fail;
    ec_ref X,Start;
    pword call;
    long num;
    int res;

    ec_init();

    /* make a set of facts */
    ec_exec_string("compile_term([p(1),p(2),p(3),p(2),p(1)])",0);

    /* make atoms and functors */
    p_1 = ec_did("p",1);
    fail = ec_did("fail",0);

    /* we will call p(X) and get an instantiation */
    X = ec_ref_create_newvar();

    /* Start will contain choice point before executing p(X) */
    Start = ec_ref_create(ec_nil());

    /* Fail loop */
    ec_post_goal(ec_term(p_1,ec_ref_get(X)));
    while(PSUCCEED == ec_resume1(Start))
    {
	/* on each iteration X is instantiated to a different number */
    	if (PSUCCEED == ec_get_long(ec_ref_get(X),&num))
	{
	    printf("p(%d)\n",num);

	    /* at 3 we want to exit the loop, cutting away other choices */
	    if(num == 3)
		ec_cut_to_chp(Start);
	}

	ec_post_goal(ec_atom(fail));
    }

    ec_ref_destroy(X);
    ec_ref_destroy(Start);

    ec_cleanup();
    exit(0);
}

