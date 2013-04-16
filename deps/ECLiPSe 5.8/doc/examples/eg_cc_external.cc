/*
 * Examples for ECLiPSe C++ externals, from the User Manual
 *
 * $Id: eg_cc_external.cc,v 1.3 2003/01/13 11:02:38 js10 Exp $
 *
 */

#include "eclipseclass.h"

extern "C" int
p_isvar()
{
    return EC_arg(1).is_var();
}

extern "C" int
p_comp()
{
    return unify(EC_arg(1),
	/*
    	compare(EC_arg(2),EC_arg(3)) < 0 ? EC_atom("<") :
    	compare(EC_arg(2),EC_arg(3)) > 0 ? EC_atom(">") :
	EC_atom("="));
	*/
    	EC_arg(2) == EC_arg(3) ? EC_atom("==") :
	EC_atom("\\="));
}

extern "C" int
p_eq()
{
    return EC_arg(1) == EC_arg(2) ? EC_succeed : EC_fail;
}


extern "C" int
p_string_to_list()
{
    EC_word  the_string(EC_arg(1));
    EC_word  the_list(nil());
    char *s;
    long len;
    int res;

    res = the_string.is_string( &s );
    if (res != EC_succeed) return res;
    len = strlen(s);

    /* the list is built backwards */
    while (len--)
    {
	the_list = list(EC_word(s[len]), the_list);
    }
    return unify(EC_word(EC_arg(2)), the_list);
}


extern "C" int
p_sumlist()
{
    int res;
    long x, sum = 0;
    EC_word list(EC_arg(1));
    EC_word car,cdr;

    for ( ; list.is_list(car,cdr) == EC_succeed; list = cdr)
    {
	res = car.is_long( &x);
	if (res != EC_succeed) return res;
	sum += x;
    }
    res = list.is_nil();
    if (res != EC_succeed) return res;
    return unify(EC_arg(2), EC_word(sum));
}

