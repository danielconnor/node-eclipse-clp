
/*
 * SEPIA INCLUDE FILE
 *
 * SCCS CONTROL BLOCK
 *
 *	SccsId   = "%W%	%E%";
 *	SccsCr   = "%Z%   Copyright 1989 ECRC GmbH and ICL";
 */

/*
 * IDENTIFICATION	external.h
 *
 * DESCRIPTION   	Contains Macros for externals 
 *       
 * CONTENTS:
 *			Current_Error 
 *			Current_Input  
 *			Current_Output 
 *			Debug_Input
 *			Debug_Output 
 *			Delay     
 *			Dereference(x) 
 *			Did(string, arity)  
 *			Error(code)  
 *			Fail  
 *			Fprintf  
 *			Get_Array_Address(adid, address)
 *			Get_Visible_Array_Address(adid, module,mod_tag,address)
 *			Get_Array_Header(adid, address)  
 *			Get_Visible_Array_Header(adid, module, mod_tag,address)
 *			Get_Stream(stream_val, stream_tag, type, stream)
 *			Mark_Suspending_Variable(var)
 *			Mark_Suspending_Variable_Inst(var)
 *			Prolog_Call(goal_val, goal_tag, mod_val, mod_tag)
 *			Prolog_Call_Nobind(goal_val, goal_tag, mod_val, mod_tag)
 *			Succeed  
 *			Toplevel_Input  
 *			Toplevel_Output  
 *			User 
 *			Write(val, tag, stream)  
 *			Writeq(val, tag, stream) 
 *
 *
 */

#define EC_EXTERNAL

#ifdef _WIN32
#include <windows.h>
#define Winapi WINAPI
#else
#define Winapi
#endif

/*
 * INCLUDES
 */
#include "config.h"
#include "sepia.h"
#include "types.h"
#include "error.h"
#include "embed.h"

/*
 * DEFINES:
 */
#define	Succeed			Succeed_
#define Fail			Fail_
#define Error(code)		Bip_Error(code)
#define Delay			return PDELAY;
#define Dereference(x)		Dereference_(x)

#define Fprintf			p_fprintf
#define Write(val, tag, stream)					\
	{							\
		int	res;					\
		res = ec_pwrite(2, stream, val, tag, 1200, PrintDepth,	\
			d_.default_module, tdict, 0);		\
		if (res != PSUCCEED)				\
		    { Bip_Error(res);}				\
	}

#define Writeq(val, tag, stream)		\
	{							\
		int	res;					\
		res = ec_pwrite(3, stream, val, tag, 1200, PrintDepth,	\
			d_.default_module, tdict, 0);		\
		if (res != PSUCCEED)				\
		    { Bip_Error(res);}				\
	}


#define Get_Array_Address(adid, address)				\
	Get_Array_Header(adid, address)					\
	if (DidArity(adid) != 0)					\
	{								\
	    address = address->val.ptr;					\
	    address = (pword *) ((uword *) address + 1 + DidArity(adid));\
	}

#define Get_Visible_Array_Address(adid, module, mod_tag, address)	\
	Get_Visible_Array_Header(adid, module, mod_tag, address)	\
	if (DidArity(adid) != 0)					\
	{								\
	    address = address->val.ptr;					\
	    address = (pword *) ((uword *) address + 1 + DidArity(adid));\
	}

#define Get_Array_Header(adid, address)					\
	address = get_array_header(adid);				\
	if (address == 0)						\
	{								\
		Error(NOGLOBAL);					\
	}

#define Get_Visible_Array_Header(adid, module, mod_tag, address)	\
	{								\
	    int	res;							\
	    address = get_visible_array_header(adid, module, mod_tag, &res);\
	    if (address == 0)						\
	    {								\
		Error(res);						\
	    }								\
	}

#define Mark_Suspending_Variable(vptr) {        \
        register pword *pw = TG;                \
        TG += 2;                                \
        Check_Gc;                               \
        pw[0].val.ptr = vptr;                   \
        pw[0].tag.kernel = TREF;                \
        if (SV) {                               \
            pw[1].val.ptr = SV;                 \
            pw[1].tag.kernel = TLIST;           \
        } else                                  \
            pw[1].tag.kernel = TNIL;            \
        SV = pw;                                \
    }

#define Mark_Suspending_Variable_Inst(var)	\
	Mark_Suspending_Variable(var)

#define Check_Gc \
	if (TG >= TG_LIM) global_ov();

#define Prolog_Call(goal_val, goal_tag, mod_val, mod_tag) \
	sub_emulc(goal_val, goal_tag, mod_val, mod_tag)

#define Prolog_Call_Nobind(goal_val, goal_tag, mod_val, mod_tag) \
	query_emulc(goal_val, goal_tag, mod_val, mod_tag)


/*
 * EXTERNAL FUNCTION DECLARATIONS:
 */

Extern dident	bitfield_did(Dots);
Extern pword	*get_array_header(Dots),
		*get_visible_array_header(Dots);
Extern int	ec_pwrite(Dots),
		sub_emulc(Dots),
		query_emulc(Dots);

Extern stream_id	get_stream_id();

#define INPUT		0x0001
#define OUTPUT		0x0002
#define Get_Stream(vs, ts, typ, nst)				\
	{							\
	    int			res;				\
	    nst = get_stream_id(vs, ts, typ, &res);		\
	    if (nst == 0)					\
		{ Error(res); }					\
	}
