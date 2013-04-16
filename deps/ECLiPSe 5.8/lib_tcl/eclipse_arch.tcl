#
# $Id: eclipse_arch.tcl,v 1.2 2003/05/27 17:36:05 ks15 Exp $
#
# compute the ECLiPSe architecture name using Tcl primitives
#

proc ec_arch {} {
    global tcl_platform
    switch -glob $tcl_platform(os) {
	Windows* {
	    return i386_nt
	}
	SunOS {
	    switch -glob $tcl_platform(osVersion) {
		4.*	{ return sun4 }
		5.*	{ return sparc_sunos5 }
	    }
	}
	Linux {
	    switch -glob $tcl_platform(machine) {
		alpha	{ return alpha_linux }
		i?86	{ return i386_linux }
	    }
	}
	Darwin {
	    return ppc_macosx
	}
    }
    error "Platform $tcl_platform(os) $tcl_platform(osVersion) ($tcl_platform(machine)) not supported"
}

# returns the platform that Tk is running under. The tricky part is that
# MacOS X can be using either the native Aqua or X11, and unfortunately
# there is apparently no way to tell before Tk 8.4 (winfo server . crashes 
# under Aqua!)
proc ec_tk_platform {} {
    global tcl_platform

    switch $tcl_platform(platform) {
	unix {
	    if {[info tclversion] >= 8.4} {
		return unix_[tk windowingsystem]
	    } else {
		# Tk < 8.4 does not have tk windowingsystem
		if {$tcl_platform(os) == "Darwin"} {
		    return unix_acqua  ;# just assume it is acqua
		} else {
		    return unix_x11
		}
	    }
	}
	windows {
	    return windows
	}
    }
}