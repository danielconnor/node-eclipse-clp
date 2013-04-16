.\"	$Id: tkeclipse.l,v 1.2 2003/05/01 14:58:30 js10 Exp $
.TH TKECLIPSE L "January 2003"
.UC 4
.SH NAME
tkeclipse \- ECLiPSe Constraint Logic Programming System (graphical user interface)
.SH SYNOPSIS
.B tkeclipse
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
None. Use the tkeclipse preference editor to modify the defaults.


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
.IR eclipse (1), tktools (1)
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


.SH BUGS
Bugs should be reported via the web site or by email
.IP
http://www.icparc.ic.ac.uk/eclipse/bugs.html
mailto:eclipse-bugs@icparc.ic.ac.uk
.br
.LP
