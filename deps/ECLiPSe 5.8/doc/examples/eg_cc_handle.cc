/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_handle.cc,v 1.2 1998/02/26 17:25:22 js10 Exp $
 *
 *
 * IDENTIFICATION:	minimain.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of using handles
 */

#include	"eclipseclass.h"


double my_array[5] = {1.1, 2.2, 3.3, 4.4, 5.5};

main()
{
    ec_init();

    EC_ref X;
    post_goal(
	term(EC_functor(",",2),
	    term(EC_functor("xget",3),
		handle(&ec_xt_double_arr, my_array),
	    	3,
		X),
	    term(EC_functor("writeln",1), X)
	)
    );

    EC_resume();

    ec_cleanup(0);
}

