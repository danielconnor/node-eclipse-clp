/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_c_handle.c,v 1.3 1998/02/26 17:25:21 js10 Exp $
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

#include	"eclipse.h"


double my_array[5] = {1.1, 2.2, 3.3, 4.4, 5.5};

main(argc, argv)
int             argc;
char          **argv;
{
    pword	X;

    ec_init();

    X = ec_newvar();
    ec_post_goal(
	ec_term(ec_did(",",2),
	    ec_term(ec_did("xget",3),
		ec_handle(&ec_xt_double_arr, my_array),
		ec_long(3),
		X),
	    ec_term(ec_did("writeln",1), X)
	)
    );

    ec_resume1(0);

    ec_cleanup();
    exit(0);
}

