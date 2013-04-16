
/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_sendmore.cc,v 1.2 1998/12/04 10:41:31 js10 Exp $
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

enum {S,E,N,D,M,O,R,Y};
main()
{
	ec_init();
	post_goal("lib(fd)");

	EC_refs X(8);
	EC_ref DigitList;
	DigitList = list(X);

	post_goal(term(EC_functor("::",2),DigitList,
		term( EC_functor("..",2),0,9)));

	post_goal(term(EC_functor("alldistinct",1),DigitList));
	post_goal(term(EC_functor("##",2),0,X[S]));
	post_goal(term(EC_functor("##",2),0,X[M]));
	post_goal(term(EC_functor("#=",2),
			    1000 * X[S] + 100 * X[E] + 10 * X[N] + X[D]
			  + 1000 * X[M] + 100 * X[O] + 10 * X[R] + X[E] ,
	     10000 * X[M] + 1000 * X[O] + 100 * X[N] + 10 * X[E] + X[Y] ));
	post_goal(term(EC_functor("labeling",1),DigitList));
	post_goal(term(EC_functor("writeln",1),DigitList));
	EC_resume();
}


