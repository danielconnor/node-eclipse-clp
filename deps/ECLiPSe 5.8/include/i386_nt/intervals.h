
/* Codes for the various ria operations. */
/* If you change these, be sure to update ria.pl as well. */

#define	RIA_UN_SQR		0
#define	RIA_UN_SQRT		1
#define	RIA_UN_SIN		2
#define	RIA_UN_COS		3
#define	RIA_UN_EXP		4
#define	RIA_UN_LN		5
#define	RIA_UN_ATAN		6
#define	RIA_UN_PI		7
#define	RIA_UN_ABS		8
#define	RIA_UN_ROUNDOUT		10
#define	RIA_UN_NEG		11
#define	RIA_UN_WIDTH		12

#define	RIA_BIN_ADD		0
#define	RIA_BIN_SUB		1
#define	RIA_BIN_MULT		2
#define	RIA_BIN_DIV		3
#define	RIA_BIN_RSQR		4
#define	RIA_BIN_POW_INT		5
#define	RIA_BIN_RPOW_ODD	6
#define	RIA_BIN_RELAX		8
#define	RIA_BIN_MIN		9
#define	RIA_BIN_MAX		10
#define	RIA_BIN_LOGSPLIT	11
#define	RIA_BIN_PLUSMINUS	12
#define	RIA_BIN_MIN_DELTA	13
#define	RIA_BIN_LINSPLIT	14
#define	RIA_BIN_LINSPLIT_UPPER	15
#define	RIA_BIN_LOGSPLIT_UPPER	16

#define	RIA_TERN_RPOW_EVEN	0
#define	RIA_TERN_UNION		1
#define	RIA_TERN_DIV		2


/* Miscellaneous stuff useful for when working with intervals. */

#include <math.h>	/* for ieee_flags(), infinity(), nextafter() */
#if defined(_WIN32) || defined(__APPLE__)
#include <float.h>
#define MINDOUBLE DBL_MIN
#define MAXDOUBLE DBL_MAX
#else
#include <values.h>	/* for MINDOUBLE/MAXDOUBLE */
#endif

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif

/* #define HAVE_SINCOS */
/* #define HAVE_NEXTAFTER */
/* #define RIA_DEBUG */

#ifndef HAVE_SINCOS
#define sincos(x,s,c)	{ *(s) = sin(x); *(c) = cos(x); }
#endif
