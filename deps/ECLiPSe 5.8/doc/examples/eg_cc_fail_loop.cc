/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_fail_loop.cc,v 1.1 1997/06/26 13:35:51 js10 Exp $
 *
 *
 * IDENTIFICATION:	fail_loop.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipseclass.h"
#include	<iostream.h>


main(int argc,char ** argv)
{
    ec_init();
    EC_atom fail = EC_atom("fail");
    EC_ref X;
    EC_ref Start;
    long num;


    /* p(X) has infinite solutions, each solution gives a different
     * value of X
     */
    ec_exec_string("compile_term([p(1),(p(N) :- N is p+1)])",0);

    /* Fail loop */
    post_goal(term(EC_functor("p",1),X));
    while(EC_succeed == EC_resume(Start))
    {
	/* on each iteration X is instantiated to a different number */
    	if (EC_succeed == ((EC_word)X).is_long(&num))
	{
	    cout << "p(" << num << ")\n";

	    /* at 10 we want to exit the loop, cutting away other choices */
	    if(num == 10)
		Start.cut_to();
	}

	post_goal(fail);
    }

    ec_cleanup(0);
    exit(0);
}

