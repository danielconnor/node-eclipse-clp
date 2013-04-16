/*
 *							Copyright 1997 IC-Parc
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: embed.h,v 1.41 2004/11/27 13:20:01 amc4 Exp $
 *
 *
 * IDENTIFICATION:	embed.h
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *			External embedding interface and safe variables
 */

#ifndef EC_EXTERNAL
#ifdef _WIN32
/* For building Eclipse itself: avoid to include windows.h everywhere */
/* Define Winapi for the compiler we use (Microsoft C) */
#define Winapi __stdcall
#else
#define Winapi
#endif
#else
/* If this file is used to build user's C code, Winapi is defined in
 * eclipse.h or eclipseclass.h specific to the user's C/C++ compiler */
#endif

enum {
	EC_OPTION_MAPFILE		=0,
	EC_OPTION_PARALLEL_WORKER	=1,
	EC_OPTION_ARGC			=2,
	EC_OPTION_ARGV			=3,
	EC_OPTION_LOCALSIZE		=4,
	EC_OPTION_GLOBALSIZE		=5,
	EC_OPTION_PRIVATESIZE		=6,
	EC_OPTION_SHAREDSIZE		=7,
	EC_OPTION_PANIC			=8,
	EC_OPTION_ALLOCATION		=9,
	EC_OPTION_DEFAULT_MODULE	=10,
	EC_OPTION_ECLIPSEDIR		=11,
	EC_OPTION_IO			=12,
	EC_OPTION_INIT			=13,
	EC_OPTION_DEBUG_LEVEL		=14
};

/*
 * Data
 */

#if defined(EC_EXTERNAL) && defined(_WIN32)

Extern t_eclipse_data	*_imp__ec_;
#define ec_		(*_imp__ec_)

Extern t_ext_type		*_imp__ec_xt_double_arr;
#define ec_xt_double_arr (*_imp__ec_xt_double_arr)

Extern t_ext_type		*_imp__ec_xt_long_arr;
#define ec_xt_long_arr (*_imp__ec_xt_long_arr)

Extern t_ext_type		*_imp__ec_xt_char_arr;
#define ec_xt_char_arr (*_imp__ec_xt_char_arr)

#else

Extern t_eclipse_data		ec_;
Extern t_ext_type		ec_xt_double_arr;
Extern t_ext_type		ec_xt_long_arr;
Extern t_ext_type		ec_xt_char_arr;

#endif


/*
 * Initialisation options
 */
Extern int	Winapi	ec_set_option_int ARGS((int, int));
Extern int	Winapi	ec_set_option_ptr ARGS((int, void *));

/*
 * Create and destroy an eclipse engine
 */
Extern int	Winapi	ec_init ARGS((void));
Extern int	Winapi	ec_cleanup ARGS((void));

/*
 * Restart an eclipse engine that has yielded
 */
Extern void	Winapi	ec_post_goal ARGS((const pword));
Extern void	Winapi	ec_post_string ARGS((const char *));
Extern void	Winapi	ec_post_exdr ARGS((int, const char *));
Extern int	Winapi	ec_resume ARGS((void));
Extern int	Winapi	ec_resume1 ARGS((ec_ref));
Extern int	Winapi	ec_resume2 ARGS((const pword,ec_ref));
Extern int	Winapi	ec_resume_long ARGS((long *));
Extern int	Winapi	ec_resume_async ARGS((void));
Extern int	Winapi	ec_running ARGS((void));
Extern int	Winapi	ec_resume_status ARGS((void));
Extern int	Winapi	ec_resume_status_long ARGS((long *));
Extern int	Winapi	ec_wait_resume_status_long ARGS((long *, int));

/*
 * Send events to running engine and handle them
 * (Note that events can also be raised by queues)
 */
Extern int	Winapi	ec_post_event ARGS((pword));
Extern int	Winapi	ec_post_event_string ARGS((const char *));
Extern int	Winapi	ec_post_event_int ARGS((int));
Extern int	Winapi	ec_handle_events ARGS((long *));

/*
 * Choicepoints
 */
Extern void	Winapi	ec_cut_to_chp ARGS((ec_ref));

/*
 * construct eclipse terms
 */
Extern pword	Winapi	ec_string ARGS((const char*));
Extern pword	Winapi	ec_length_string ARGS((int, const char*));
Extern pword	Winapi	ec_atom ARGS((const dident));
Extern pword	Winapi	ec_long ARGS((const long));
Extern pword	Winapi	ec_double ARGS((const double));
#ifdef STDC_HEADERS
Extern pword		ec_term ARGS((dident, ... /*pwords*/));
			/* can't use Winapi with varargs! */
#else
Extern pword		ec_term();
#endif
Extern pword	Winapi	ec_term_array ARGS((const dident,const pword[]));
Extern pword	Winapi	ec_list ARGS((const pword,const pword));
Extern pword	Winapi	ec_listofrefs ARGS((ec_refs));
Extern pword	Winapi	ec_listofdouble ARGS((int, const double*));
Extern pword	Winapi	ec_listoflong ARGS((int, const long*));
Extern pword	Winapi	ec_listofchar ARGS((int, const char*));
Extern pword	Winapi	ec_arrayofdouble ARGS((int, const double*));
Extern pword	Winapi	ec_matrixofdouble ARGS((int, int, const double*));
Extern pword	Winapi	ec_handle ARGS((const t_ext_type*,const t_ext_ptr));
Extern pword	Winapi	ec_newvar ARGS((void));
Extern pword	Winapi	ec_nil ARGS((void));
Extern dident	Winapi	ec_did ARGS((const char *,const int));

/*
 * inspect eclipse terms
 */
Extern int	Winapi	ec_get_string ARGS((const pword,char**));
Extern int	Winapi	ec_get_string_length ARGS((const pword,char**,long*));
Extern int	Winapi	ec_get_atom ARGS((const pword,dident*));
Extern int	Winapi	ec_get_long ARGS((const pword,long*));
Extern int	Winapi	ec_get_double ARGS((const pword,double*));
Extern int	Winapi	ec_get_nil ARGS((const pword));
Extern int	Winapi	ec_get_list ARGS((const pword,pword*,pword*));
Extern int	Winapi	ec_get_functor ARGS((const pword,dident*));
Extern int	Winapi	ec_arity ARGS((const pword));
Extern int	Winapi	ec_get_arg ARGS((const int,pword,pword*));
Extern int	Winapi	ec_get_handle ARGS((const pword,const t_ext_type*,t_ext_ptr*));
Extern int	Winapi	ec_is_var ARGS((const pword));

#define DidName(d)	((char *)(((dident)(d))->string + 1))
#define DidArity(d)	(((dident)(d))->arity)

/*
 * eclipse refs hold registered references to eclipse terms
 * which survive while the engine is running
 */
Extern ec_refs	Winapi	ec_refs_create ARGS((int,const pword));
Extern ec_refs	Winapi	ec_refs_create_newvars ARGS((int));
Extern void	Winapi	ec_refs_destroy ARGS((ec_refs));
Extern void	Winapi	ec_refs_set ARGS((ec_refs,int,const pword));
Extern pword	Winapi	ec_refs_get ARGS((const ec_refs,int));
Extern int	Winapi	ec_refs_size ARGS((const ec_refs));

Extern ec_ref	Winapi	ec_ref_create ARGS((pword));
Extern ec_ref	Winapi	ec_ref_create_newvar ARGS((void));
Extern void	Winapi	ec_ref_destroy ARGS((ec_ref));
Extern void	Winapi	ec_ref_set ARGS((ec_ref,const pword));
Extern pword	Winapi	ec_ref_get ARGS((const ec_ref));


/*
 * String-based interface
 */

Extern int	Winapi	ec_exec_string ARGS((char*,ec_ref));
Extern int	Winapi	ec_var_lookup ARGS((ec_ref,char*,pword*));

/*
 * External function interface
 */

Extern pword	Winapi	ec_arg ARGS((int));
Extern int	Winapi	ec_unify ARGS((pword,pword));
Extern int	Winapi	ec_unify_arg ARGS((int,pword));
Extern int	Winapi	ec_compare ARGS((pword,pword));
Extern int	Winapi	ec_visible_procedure ARGS((dident,pword,void**));
Extern int	Winapi	ec_make_suspension ARGS((pword,int,void*,pword*));
Extern int	Winapi	ec_schedule_suspensions ARGS((pword,int));
Extern int	Winapi	ec_free_handle ARGS((const pword, const t_ext_type*));

/*
 * Stream I/O
 */

Extern int	Winapi	ec_stream_nr ARGS((char *name));
Extern int	Winapi	ec_queue_write ARGS((int stream, char *data, int size));
Extern int	Winapi	ec_queue_read ARGS((int stream, char *data, int size));
Extern int	Winapi	ec_queue_avail ARGS((int stream));
Extern void	Winapi	ec_double_xdr ARGS((double * d, char * dest));
Extern void	Winapi	ec_int32_xdr ARGS((int32 * l, char * dest));
Extern void	Winapi	ec_xdr_int32 ARGS((char * buf , int32 * l));
Extern void	Winapi	ec_xdr_double ARGS((char * buf , double * d));

/*
 * Error handling
 */

Extern char *	Winapi	ec_error_string ARGS((int));
Extern void		ec_panic ARGS((const char* what, const char* where)); /* no Winapi */



/*
 * The following is NOT part of the embedding interface!
 */

#ifndef EC_EMBED

Extern t_eclipse_options	ec_options;
Extern stream_id	current_input_, current_output_, current_err_;
Extern stream_id	log_output_, warning_output_, null_;


Extern void		trail_undo ARGS((pword*, void (*)(pword*)));
Extern void		ec_trail_undo ARGS((void (*)(pword*,word*,int,int), pword*, pword*, word*, int, int));
Extern void		ec_trail_pwords ARGS((pword*, int, int));
Extern int		ec_unify_ ARGS((value,type,value,type,pword**));
Extern int		ec_remember ARGS((int,value,type));
Extern void		cut_external ARGS((void));
Extern int		ec_external ARGS((dident,int (*) (Dots),dident));

Extern stream_id Winapi	ec_stream_id ARGS((int));
Extern	int		ec_outf ARGS((stream_id, const char*, int));
Extern	int		ec_outfc ARGS((stream_id, int));
Extern	int		ec_outfs ARGS((stream_id, const char*));
Extern	int		ec_flush ARGS((stream_id));
#ifdef STDC_HEADERS
Extern	int		p_fprintf ARGS((stream_id nst, char *fmt, ...));
#else
Extern	int		p_fprintf();
#endif
Extern	int		ec_newline ARGS((stream_id));

Extern void		delayed_break ARGS((void));
Extern int		final_overflow ARGS((void));
Extern void		global_ov ARGS((void));
Extern void		trail_ov ARGS((void));

Extern volatile int	it_disabled_, delayed_it_;

Extern int		p_handle_free ARGS((value v_handle, type t_handle));
Extern int		p_handle_free_on_cut ARGS((value v_handle, type t_handle));

#endif /* !EC_EMBED */

