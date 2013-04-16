/*
 * ECLiPSe INCLUDE FILE
 *
 * $Id: eclipse.h,v 1.2 1998/06/04 16:52:37 js10 Exp $
 *
 * DESCRIPTION
 *		Included by C programs that use embedding interface.
 */

#define EC_EXTERNAL
#define EC_EMBED

#ifdef _WIN32
#include <windows.h>
#define Winapi WINAPI
#else
#define Winapi
#endif

#include "config.h"
#include "ec_public.h"
#include "types.h"
#include "embed.h"
