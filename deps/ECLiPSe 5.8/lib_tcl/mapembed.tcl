#! /bin/sh
# \
	exec wish $0 ${1+"$@"}

# mapembed.tcl:
# Tcl code for the embedded variant. This code is a simple wrapper around
# mapcolour.tcl that starts and embedded ECLiPSe and then calls map_init

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
	exit -1
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]

package require eclipse
package require eclipse_tools

source "mapcolour.tcl"

# Initialisation
ec_init
map_init
