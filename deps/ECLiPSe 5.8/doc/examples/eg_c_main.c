/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_c_main.c,v 1.3 2004/05/05 16:16:49 js10 Exp $
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
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipse.h"


main(int argc, char **argv)
{
    char 	*s;
    dident	writeln,read,garbage_collect;
    ec_ref Vars,X;
    pword ans;
    
    ec_init();

    Vars = ec_ref_create(ec_nil());
    ec_exec_string("writeln(\"hello world: \"),read(X)",Vars);
    if (PSUCCEED == ec_var_lookup(Vars,"X",&ans) &&
    	PSUCCEED == ec_get_string(ans,&s))
	printf("Answer was %s\n",s);

    ec_ref_destroy(Vars);

    writeln = ec_did("writeln",1);
    read = ec_did("read",1);
    garbage_collect = ec_did("garbage_collect",0);

    X = ec_ref_create_newvar();

    /* writeln("hello again: ") */
    ec_post_goal(ec_term(writeln, ec_string("hello again: ")));
    /* read(X) */
    ec_post_goal(ec_term(read, ec_ref_get(X)));
    /* garbage_collect */
    ec_post_goal(ec_atom(garbage_collect));

    ec_resume1(0); /* pwords not in ec_ref are lost here */

    /* writeln(X) */
    ec_post_goal(ec_term(writeln, ec_ref_get(X)));
    ec_resume1(0);

    ec_ref_destroy(X);

    ec_cleanup();
    exit(0);
}

