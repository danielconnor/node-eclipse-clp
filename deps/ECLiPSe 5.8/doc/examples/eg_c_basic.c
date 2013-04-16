/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_c_basic.c,v 1.1 1998/02/25 18:31:37 js10 Exp $
 *
 *
 * IDENTIFICATION:	eg_c_basic.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main using string-based embed interface.
 */

#include	"eclipse.h"


main(argc, argv)
int             argc;
char          **argv;
{
    char	buf[1024];
    int		n;

    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
    ec_init();

    ec_post_string("write(hello)");
    ec_resume();

    n = ec_queue_read(1, buf, 1024);
    buf[n] = 0;
    printf("eclipse returned: %s.\n", buf);

    n = ec_queue_read(2, buf, 1024);
    buf[n] = 0;
    printf("eclipse error returned: %s.\n", buf);

    ec_cleanup();
    exit(0);
}

