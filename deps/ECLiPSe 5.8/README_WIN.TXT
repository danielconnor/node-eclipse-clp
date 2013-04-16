----------------------------------------------
ECLiPSe - Constraint Logic Programming System
Copyright 1992-1996 ECRC GmbH
Copyright 1996-2004 IC-Parc, Imperial College
Copyright 2001-2004 Parc Technologies Ltd
Copyright 2004      Cisco Technology Inc
----------------------------------------------

-----------------------------------------------------------------------------
ECLiPSe on Windows (see README_UNIX for UNIX/Linux, README_MACOSX for MacOSX)
-----------------------------------------------------------------------------

ECLiPSe for Windows is primarily intended and supported only for
Windows-NT and later versions (Windows-2000, Windows-XP, ...).

However, it works fine on Windows-9X (with only minor restrictions)
provided you have all the necessary Microsoft-DLLs.  In particular,
you need to have Winsock2 installed and you need MSVCRT.DLL.


--------------------------------------
Using the Windows Installer
--------------------------------------

For the most common Windows install configurations, use the ECLiPSe
Windows Installer. To do so, download and execute the single file

	ECLiPSe<Version>_<Build>.exe

The installer contains the ECLiPSe kernel, basic libraries 
and the following optional packages:

- Online documentation (html, txt)
- Printable documentation (pdf)
- The latest interface with the bundled XPRESS-MP OEM version
- Free/3rd party libraries
- Source code for selected ECLiPSe libraries
- The latest Tcl/Tk release

For installation, double click on ECLiPSe<Version>_<Build>.exe and
the install wizard will guide you through the installation.

Should an alternate XPRESS-MP (or CPLEX(r)) interface be required:

Obtain the appropriate .tgz package (if_cplexXY.tgz or 
if_xpressXY.tgz) and unpack it in the directory in which ECLiPSe
was originally installed using the Windows installer.


--------------------------------------
Manual Installation from .tgz Packages
--------------------------------------

You should have gotten the following files for the "i386_nt" architecture:

Base:
    README_WIN.TXT		The file you are reading now
    eclipse_basic.tgz        	Kernel and basic libraries
    eclipse_doc_online.tgz	Documentation (html, txt)
    eclipse_doc_print.tgz	Documentation (pdf)

Optional:
    eclipse_misc.tgz		Free/3rd party libraries
    eclipse_misc_nc.tgz		Free libraries not for commercial use
    if_xpressXYicp.tgz		Interface to XPRESS-MP solver, including a
    				free version of the solver for academic use
    eclipse_source.tgz		Source code for selected ECLiPSe libraries

Support:
    tcl831.exe			A matching Tcl/Tk release (8.3)
    				(you may have that already).
				Needed for the tkeclipse
				development GUI.

These other files are only needed under special circumstances:
    eclipse_rt.tgz		Runtime only (subset of eclipse_basic.tgz)
    if_xpressXY.tgz		Interface to XPRESS-MP version XY
    if_cplexXY.tgz		Interface to CPLEX(r) version XY



Installation
------------

1.  Make a folder for ECLiPSe on your machine and move the
    distribution files there.

    In the following we will refer to this folder as <eclipsedir>. 
    Note that you can have different versions of ECLiPSe installed
    at the same time (in different folders).

    The ECLiPSe installation folder can also be shared between
    machines even with different hardware/operating system
    architectures, e.g. i386_nt and sparc_sunos5.  To do so, just
    unpack the distributions for the different architectures in the
    same installation directory.  All architecture-dependent files
    will go in separate subdirectories.


2.  Unpack the distribution files in this folder

    You can use a tool like WinZip, GNU tar/gzip etc.
    Beware of your download or unpacking tool trying to do clever
    modifications of the file contents!  E.g. with WinZip be sure to
    disable Options->Configurations->Smart Tarfile CR/LF Conversion.

    After unpacking, you should have the ECLiPSe directory structure
    inside the <eclipsedir> folder.

    If you are on  Windows-9X (not NT), you need to make sure you have
    the 3 system DLLs WS2HELP.DLL, WS2_32.DLL (Winsock2) and MSVCRT.DLL.


3.  If you don't have Tcl/Tk 8 on your machine yet, or you have a version
    different from 8.3, install it now by executing tcl831.exe !

    It is not absolutely necessary to have Tcl/Tk in order to
    run ECLiPSe, but it simplifies the installation and allows
    you to use the tkeclipse development environment.


4.  Execute ecl_inst.tcl in <eclipsedir> by double-clicking

    This will create the necessary registry and menu entries.


5.  You should now have a Start->Programs->ECLiPSe menu that lets
    you run the command line version and the Tcl/Tk version of
    ECLiPSe and allows you to browse the documentation.



If you don't have Tcl/Tk
------------------------

After unpaking the distribution files you need to make
a registry entry for ECLiPSe:

   - double-click on <eclipsedir>\lib\i386_nt\eclipse.reg

     This will create the registry key
     HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe\<version>
     with a default value of C:\Eclipse

   - If you have unpacked ECLiPSe in C:\Eclipse, you are done. Otherwise:

   - run regedit (Start->Run...->regedit)

   - change the value of the ECLIPSEDIR entry under
     HKEY_LOCAL_MACHINE\SOFTWARE\IC-Parc\ECLiPSe\<version>
     from "C:\Eclipse" to <eclipsedir>, ie the folder where
     you have unpacked the distribution, e.g. "C:\Eclipse"

You should then make a shortcut or menu entry for the executable
<eclipsedir>/lib/eclipse.exe


Directory structure
-------------------

Directories in the binary distribution:

    lib			supported libraries, object files, executables
    lib_public		third party Prolog libraries
    lib_noncomm		third party Prolog libraries not for commercial use
    lib_tcl		tcl files for the graphical interface
    include		include files for C/C++ interface
    doc			online documentation, pdf files and example code


The important files are in <eclipsedir>\lib\i386_nt:

    eclipse.exe		ECLiPSe standalone executable (needs eclipse.dll)
    eclipse.dll		ECLiPSe linkable library
    eclipse.lib		ECLiPSe linkable library stubs
    eclipse.def		Export specification of the dll
    eclipse.reg		Registry file for use with regedit

To compile an application using the eclipse.dll you will need the
include files in <eclipsedir>\include\i386_nt.

The files related to the GUI version are in <eclipsedir>\lib_tcl:

    tkeclipse.tcl	ECLiPSe development GUI (executable)
    eclipse.tcl		eclipse package for Tcl/Tk-embedding
    eclipse_tools.tcl	eclipse_tools package with support tools
    example*.tcl	examples for using eclipse from Tcl/Tk

Code for embedding into Visual Basic is in <eclipsedir>\doc\examples.



Differences between UNIX and Windows versions of ECLiPSe
--------------------------------------------------------

Pathnames: Eclipse internally always works with platform independent
path names using forward slashes, e.g.

Windows pathname:	C:\Software\Eclipse
ECLiPSe form:		//C/Software/Eclipse


Documentation
-------------

The installation subdirectory <eclipsedir>/doc contains html and pdf
versions of the user documentation. To read the html documentation,
set your browser to e.g.

    file:<eclipsedir>/doc/index.html


The printable documentation is contained in the files:

    tutorial.pdf	Tutorial

    userman.pdf		Basic user manual

    libman.pdf		Library user manual (constraints libraries)

    embedding.pdf	Interfacing and embedding manual

The Reference Manual (the detailed descriptions of the built-in
predicates and all the libraries) is available exclusively in html
format and via the help/1 predicate from within ECLiPSe.


Installation hints for the 'eplex' library
----------------------------------------------

The ECLiPSe 'eplex' library is an interface to an external commercial
Xpress-MP or CPLEX LP/MIP solver.

If you own a separate Xpress-MP or CPLEX licence, then you can use
the 'eplex' library with the corresponding solver. In this case, you
need to download a version of 'eplex' that matches the version of the
Xpress-MP or CPLEX licence that you have (although it may be possible to
run an older version with a newer licence). For example, for Xpress-MP X.Y,
you need the package if_xpressXY.tgz.  Similarly, if you have CPLEX X.Y
you need the package if_cplexXY.tgz.  You can install a mix of different
versions by just unpacking multiple packages.

To tell ECLiPSe which licence you have on which machine, add the
necessary lines to the text file <eclipsedir>/lib/eplex_lic_info.ecl,
according to the instructions provided there.  With this setup, you
can load the eplex library from ECLiPSe by just invoking lib(eplex),
and you will get the correct version for each machine.

Academic licences for CPLEX are available from ILOG at
    http://www.ilog.com/partners/academic/license.cfm
Academic licences for XPRESS-MP are available from Dash Optimization at
    http://www.dashoptimization.com/home/users/app.html


Graphics
--------

The Embedding Interface and the TkEclipse Development Environment
require Tcl/Tk version 8.3 or later.
This might already be installed on your machine, otherwise install
it by executing tcl831.exe.

Several visualisation features require Java. The recommended version
is at least 1.4, but versions from 1.2 upwards should still work.


Emacs ECLiPSe mode
------------------

If you use the GNU emacs editor, an ECLiPSe mode is now provided for it.
**Version 20 (or later) of GNU emacs is required.** This mode is based
on the prolog mode by Umeda, and provides extensive syntax colouring 
for ECLiPSe-specific features. See http://www.gnu.org/ if you need more 
information on GNU and its software. 

In order to use this mode, add the following line to your .emacs file:

(autoload 'eclipse-mode "<eclipsedir>/lib_public/eclipse.el" "ECLIPSE editing mode" t)

where <eclipsedir> is the path to your ECLiPSe installation directory.

You may also need to add these lines so that the .pl and .ecl extensions
are recognised to be ECLiPSe programs:

(setq auto-mode-alist (cons '("\\.pl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))

See the start of the eclipse.el file for some instructions on using this mode.

----------------------------------------------
General Topics
----------------------------------------------

Contact Addresses
-----------------

In case you have supplied us with your electronic mail address, we
will add you automatically to the ECLiPSe announcements mailing list

    eclipse-announce@icparc.ic.ac.uk

to keep you informed about new releases. If you want to participate
in discussions related to the use of ECLiPSe and exchange information
with other users, you should subscribe to the ECLiPSe users mailing list

    eclipse-users@icparc.ic.ac.uk

See the web page http://www.icparc.ic.ac.uk/eclipse/mailinglist.html
for how to subscribe and unsubscribe.

Problem and bug reports can be submitted via the web site

    http://www.icparc.ic.ac.uk/eclipse/bugs.html

or by email to

    eclipse-bugs@icparc.ic.ac.uk

Please take the time to report problems. Your feedback helps us to
make ECLiPSe more reliable!

Other requests directed to the development team can be sent to

    eclipse-request@icparc.ic.ac.uk

