/*
 * Examples for ECLiPSe C externals, from the User Manual
 *
 * $Id: eg_c_external.c,v 1.2 2003/01/13 11:02:38 js10 Exp $
 *
 */

#include "eclipse.h"

int
p_isvar()
{
    return ec_is_var(ec_arg(1));
}

int
p_comp()
{
    return ec_unify_arg(1,
    	ec_compare(ec_arg(2),ec_arg(3)) < 0 ? ec_atom(ec_did("<",0)) :
    	ec_compare(ec_arg(2),ec_arg(3)) > 0 ? ec_atom(ec_did(">",0)) :
	ec_atom(ec_did("=",0)));
}

int
p_string_to_list()
{
    pword  list;
    char *s;
    long len;
    int res;

    res = ec_get_string_length(ec_arg(1), &s, &len);
    if (res != PSUCCEED) return res;

    list = ec_nil();	/* the list is built backwards */
    while (len--)
    {
	list = ec_list(ec_long(s[len]), list);
    }
    return ec_unify_arg(2, list);
}


int
p_sumlist()
{
    int res;
    long x, sum = 0;
    pword list, car, cdr;

    for (list = ec_arg(1); ec_get_list(list,&car,&cdr) == PSUCCEED; list = cdr)
    {
	res = ec_get_long(car, &x);
	if (res != PSUCCEED) return res;
	sum += x;
    }
    res = ec_get_nil(list);
    if (res != PSUCCEED) return res;
    return ec_unify_arg(2, ec_long(sum));
}

