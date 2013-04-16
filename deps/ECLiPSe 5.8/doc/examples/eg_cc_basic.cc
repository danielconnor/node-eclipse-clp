/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_basic.cc,v 1.2 1998/02/26 17:25:22 js10 Exp $
 *
 *
 * IDENTIFICATION:	eg_cc_basic.cc
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main using string-based embed interface.
 */

#include	"eclipseclass.h"


main()
{
    char	buf[1024];
    int		n;

    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
    ec_init();

    ec_post_string("write(hello)");
    ec_resume();

    n = ec_queue_read(1, buf, 1024);
    buf[n] = 0;
    cout << "eclipse returned: " << buf << ".\n":

    n = ec_queue_read(2, buf, 1024);
    buf[n] = 0;
    cout << "eclipse error returned: " << buf << ".\n";

    ec_cleanup();
    exit(0);
}

