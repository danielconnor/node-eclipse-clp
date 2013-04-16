#! /bin/sh
# \
	exec wish $0 ${1+"$@"}

#----------------------------------------------------------------------
# Preamble: Find and load the eclipse package
#----------------------------------------------------------------------

switch $tcl_platform(platform) {
    unix {
	if {![info exists env(ECLIPSEDIR)]} {
	    puts "Cannot run Eclipse: ECLIPSEDIR environment variable is undefined."
	    exit -1
	}
	set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
    }
    windows {
	package require registry
	set tkecl(ECLIPSEDIR) [registry get \
	    HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\5.8 ECLIPSEDIR]
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]

package require eclipse



#----------------------------------------------------------------------
# Make a standard menu bar and a 'file' menu
#----------------------------------------------------------------------

wm title . "The smallest possible ECLiPSe/Tcl/Tk Application"

. config -menu .mbar

menu .mbar
.mbar add cascade -label File -menu .mbar.file

menu .mbar.file
.mbar.file add command -label Run -command run_eclipse
.mbar.file add command -label Exit -command exit


#----------------------------------------------------------------------
# The command to run the Eclipse-goal
#----------------------------------------------------------------------

proc run_eclipse {} {
    ec_post_goal { writeln("hello world"),flush(output) }
    ec_resume
}


#----------------------------------------------------------------------
# Initialise the embedded ECLiPSe
#----------------------------------------------------------------------

ec_init

