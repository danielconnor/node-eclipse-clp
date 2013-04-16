.\"	$Id: eclipse.l,v 1.3 2003/05/01 14:58:30 js10 Exp $
.TH ECLIPSE L "January 2003"
.UC 4
.SH NAME
eclipse \- ECLiPSe Constraint Logic Programming System (console interface)
.SH SYNOPSIS
.B eclipse [ options ...]
.SH DESCRIPTION
.I ECLiPSe
is a Constraint Logic Programming system, initially developed at the
European Computer-Industry Research Centre (ECRC) in Munich, and
since 1995 at the Centre for Planning and Resource Control (IC-Parc)
at Imperial College London.

.I ECLiPSe
can be used simply as a Prolog system, and provides compatibility
libraries for a variety of other Prolog dialects and the ISO Prolog standard.
As a Prolog system,
.I ECLiPSe
implements a number of nonstandard language features, including
an advanced module system,
attributed variables,
an elaborate implementation of data-driven goal execution,
array syntax,
structures with named fields,
string data type,
arithmetic with big integers, rational numbers and intervals,
iteration, matching,
synchronous and asynchronous event handling,
nonlogical stores and an
external language interface.

The main purpose of
.I ECLiPSe
however is to be used as a constraint programming language. In particular,
it is aimed at problem solving with a variety of classical methods
(e.g. constraint propagation, mathematical programming, and local search),
and the combination of these basic methods into new hybrid methods.
The system's constraint solving facilities are largely based on the primitives
for data-driven execution, and are generally provided in the form
of constraint solver libraries.

.SH OPTIONS
The following options are currently recognized by
.B eclipse:
.TP 13
.B \-b bootfile
compile the file
.I bootfile
before starting the session.
.TP
.B \-e goal
Instead of starting an interactive toplevel, the system will
execute the goal
.I goal.
This goal is given in normal Prolog syntax,
and has to be quoted if it contains any characters that would normally
be interpreted by the shell.  The -e option can be used together with
the -b option and is executed afterwards.  The exit status of the
ECLiPSe process reflects success or failure of the executed Prolog
goal (0 for success, 1 for failure, 2 for abort). 
.br
When you only have a runtime installation of eclipse, the -e option
is compulsory because a runtime system does not have an interactive
toplevel.
.TP
.B \-g size
This option specifies to which limit the memory consumption of the
ECLiPSe global/trail stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).  The default is 128M, ie. 128
Megabytes.
The amount required for this stack depends on the program's data
structures and may need to be increased for very large applications.
.TP
.B \-l size
This option specifies to which limit the memory consumption of the
ECLiPSe local/control stack can grow.  The size is specified in
kilobytes (followed by an optional K), in megabytes (followed by M) or
in gigabytes (followed by G).  The default is 128M, ie. 128
Megabytes.
The local/control stack is unlikely to require more than this default.
If it does, it is probably caused by a programming error.
.TP
.B \-h size
This option is largely obsolete and currently effective only on SunOS 4.
It specifies to which limit the memory consumption of the
ECLiPSe private heap can grow.  The size is specified in kilobytes
(followed by an optional K), in megabytes (followed by M) or in
gigabytes (followed by G).  The default is 32M, ie. 32 Megabytes. 
.TP
.B \-s size
This option is largely obsolete and currently effective only on SunOS 4.
It specifies to which limit the memory consumption of the
ECLiPSe shared heap can grow.  The size is specified in kilobytes
(followed by an optional K), in megabytes (followed by M) or in
gigabytes (followed by G).  The default is 64M, ie. 64 Megabytes. 
.TP
.B \-D directory
This options allows to explicitly specify the ECLiPSe installation
directory, i.e. the directory under which the system tries to find the
ECLiPSe runtime system and libraries.  This option overrides (and
renders unnecessary) any setting of the ECLIPSEDIR environment
variable (Unix) or, respectively, an ECLIPSEDIR registry entry
(Windows) that may be in effect. 
.TP
.B \-\-
The ECLiPSe system will ignore this argument and everything that
follows on the commmand line.  The Prolog program will only see the
part of the command line that follows this argument. 


.SH ENVIRONMENT VARIABLES
The following environment variable settings are recognised by ECLiPSe,
but none of them needs to be set in order to run eclipse:
.TP 20
.B ECLIPSEDIR
the ECLiPSe installation directory. The default is fixed during
ECLiPSe installation.
.TP
.B ECLIPSETMP
the directory in which ECLiPSe may store temporary files, by default /tmp.
.TP
.B ECLIPSEINIT
the name of an initialization file which is compiled just before
starting the interactive toplevel.
If ECLIPSEINIT is unset, the default is '.eclipserc' or ~/.eclipserc.
If ECLIPSEINIT is set to the empty string, no initialization is made.
.TP
.B ECLIPSELIBRARYPATH
a colon-separated sequence of directory names, which get prepended to
ECLiPSe's list of library directories.


.SH FILES
.TP 20
.B $ECLIPSEDIR
usually /usr/local/eclipse, the ECLiPSe installation directory
.TP
.B $ECLIPSEDIR/bin/`arch`/eclipse
an executable eclipse (console interface)
.TP
.B $ECLIPSEDIR/bin/`arch`/tkeclipse
an executable eclipse (graphical user interface)
.TP
.B $ECLIPSEDIR/bin/`arch`/tktools
standalone ECLiPSe GUI debugging tools that can be attached to a
separate ECLiPSe session.
.TP
.B $ECLIPSEDIR/doc
online documentation and manuals
.TP
.B $ECLIPSEDIR/include
contains the .h files necessary to compile code using ECLiPSe's
C/C++ interfaces.
.TP
.B $ECLIPSEDIR/{lib,lib_public}
contains the ECLiPSe libraries
.TP
.B $ECLIPSETMP/eclipse.XXX.<pid>
temporary file created by ECLiPSe, e.g. for the profiler.


.SH SEE ALSO
.nf
.IR tkeclipse (1), tktools (1)
.sp
.I "ECLiPSe Web Site"
http://www.icparc.ic.ac.uk/eclipse
.sp
.I "Online Documentation"
In $ECLIPSEDIR/doc/index.html, containing the following:
.sp
.IR "ECLiPSe, An Introduction" ,
Andrew M. Cheadle et al, IC-PARC.
.sp
.IR "ECLiPSe User Manual" ,
Abderrahamane Aggoun et al, IC-PARC.
.sp
.IR "ECLiPSe Constraint Library Manual" ,
Pascal Brisset et al, IC-PARC.
.sp
.I "ECLiPSe Embedding and Interfacing Manual"
Stefan Novello, Joachim Schimpf, Kish Shen and Josh Singer, IC-PARC.
.sp
.IR "ECLiPSe Visualisation Manual" ,
Kish Shen, Josh Singer and Andrew Sadler, IC-PARC.


.SH SIGNAL HANDLING
The eclipse runtime system (eclipse -e) modifies the handler for SIGALRM
in order to implement ECLiPSe's timer facilities.
The interactive development system additionally defines handlers for SIGPROF
and SIGINT to implement profiling and user interrupt.
The development system also tries to catch all other signals and handle them
using appropriate defaults.


.SH BUGS
Bugs should be reported via the web site or by email
.IP
http://www.icparc.ic.ac.uk/eclipse/bugs.html
mailto:eclipse-bugs@icparc.ic.ac.uk
.br
.LP
