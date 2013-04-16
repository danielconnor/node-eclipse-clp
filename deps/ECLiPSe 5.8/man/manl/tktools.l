.\"	$Id: tktools.l,v 1.2 2003/05/01 14:58:30 js10 Exp $
.TH TKTOOLS L "January 2003"
.SH NAME
tktools \- standalone debugging tools for ECLiPSe constraint logic programming system
.SH SYNOPSIS
.B tktools
.SH DESCRIPTION
.I tktools
connects
.I tktools
to an executing instance of ECLiPSe. It starts a small graphical user
interface to which the
.I host
and
.I port
of the target ECLiPSe instance must be supplied. This target can be
any variant of ECLiPSe, for instance an interactive console eclipse,
a tkeclipse development system,
a standalone runtime eclipse, or even an eclipse instance which is
embedded into some host process.

The cooperating instance of ECLiPSe must invoke
remote_tools:attach_tools/0 or remote_tools:attach_tools/3 in order
to make the connection.
If attach_tools/0 is used, the system chooses a port number and prints
it. This number must be entered into the tktools port entry field.
Alternatively, a port number can be choosen, and supplied to both the
tktools port entry field, and to attach_tools/3 on the eclipse side.
The host machine can be
.I localhost
if tktools and eclipse are on the same machine, otherwise the full
domain name of the machine where the eclipse process is running.


.SH OPTIONS
None.

.SH SEE ALSO
.sp
.IR eclipse (1),
.IR tkeclipse (1)
.sp
.I "Online Documentation"
In $ECLIPSEDIR/doc/index.html, in particular:
.sp
.IR "ECLiPSe User Manual" ,
Abderrahamane Aggoun et al, IC-PARC.
Chapter "The TkECLiPSe Development Tools"
