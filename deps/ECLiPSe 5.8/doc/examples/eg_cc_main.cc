
/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_main.cc,v 1.2 1997/06/26 18:32:00 js10 Exp $
 *
 *
 * IDENTIFICATION:	eg.cc
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 */

#include	"eclipseclass.h"


double my_array[5] = {1.1, 2.2, 3.3, 4.4, 5.5};

main()
{
	ec_init();

	EC_ref R;
	EC_functor writeln_1 = EC_functor("writeln",1);
	EC_functor read_1 = EC_functor("read",1);
	EC_word w;

	post_goal(term(writeln_1,"hello world"));
	R= newvar();
	post_goal(term(read_1,R));
	post_goal(term(writeln_1,"hi again"));
	post_goal(term(writeln_1,R));
	w = list(0,list(1,3.0));
	post_goal(term(writeln_1,w));
	post_goal(term(writeln_1,R));
	post_goal(term(writeln_1,w*3+4*R));
	EC_resume();
}


