#
# $Id: tkmulti.tcl,v 1.8 2003/12/09 14:36:30 ks15 Exp $
#

package provide eclipse_peer_multitask 1.0

set tkecl(multi_state) not_registered

proc ec_multi:peer_register { {mtcommands {}} } { 
    global tkecl

    if {$tkecl(multi_state) != "not_registered"} {
	error "Calling ec_multi:peer_register when peer is already registered for multitasking."
    }

    set res [ec_rpc [list peer_register_multitask [ec_peer_name] _] {(()_)}]
    switch $res {
	fail -
	throw {
	    error "Unable to establishing a multitasking link with ECLiPSe."
	}
	default {
	    set tkecl(multi_fromec) [lindex $res 2]
	    set tkecl(multi_state) off
	    ec_set_queue_handler $tkecl(multi_fromec) fromec ec_multi:fromec_handler
	}
    }

    foreach commandtype [list multi_start_command \
			     multi_end_command multi_timeslice_command] {
	set tkecl($commandtype) {}
    }

    foreach {type command} $mtcommands {
	switch $type {
	    start {
		set tkecl(multi_start_command) $command
	    }
	    end {
		set tkecl(multi_end_command) $command
	    }
	    interact {
		set tkecl(multi_timeslice_command) $command
	    }
	    default {
		error "Unknown command type $type given in"
		" ec_multi:peer_register"
	    }
	}
    }
}


proc ec_multi:peer_deregister {} {
    global tkecl

    if {$tkecl(multi_state) == "not_registered"} {
	error "Calling ec_multi:peer_deregister when peer is not registered for multitasking."
    }

    set res [ec_rpc [list peer_deregister_multitask [ec_peer_name]] {(())}]
    switch $res {
	fail -
	throw {
	    error "Unable to deregister multitasking link with ECLiPSe."
	}
	default {
	    set tkecl(multi_fromec) ""
	    set tkecl(multi_state) not_registered
	}
    }
}

    
proc ec_multi:fromec_handler {multi_fromec_stream_nr {size 0}} {

    set message [ec_read_exdr [ec_streamnum_to_channel $multi_fromec_stream_nr]]
    set state [lindex $message 0]
    set arg  [lindex $message 1]   ;# could be empty if no argument
    ec_multi:state_action $state $arg

}

proc ec_multi:state_action {state {arg {}}} {
    global tkecl

    switch $state {
	start_multitask {
	    if {$tkecl(multi_state) != "off"} {
		set peername [ec_peer_name]
		tk_messageBox -icon error -type ok -message "peer_multitask error ($peername): Told to start multitasking during multitasking."
		return -code error
	    }

	    set tkecl(multi_type) $arg
	    set tkecl(multi_state) on
	    if {$tkecl(multi_start_command) != {}} {
		switch [eval [list $tkecl(multi_start_command) $arg]]  {
		    continue {
			ec_rpc peer_multitask_confirm
		    }
		    terminate {
			ec_multi:terminate_phase
		    }
		}
	    }
	    update  
	}
	end_multitask {
	    ;# ignore message if not multitasking...
	    if {$tkecl(multi_state) != "off"} {

		update
		set tkecl(multi_state) off
		if {$tkecl(multi_end_command) != {}} {
		    eval [list $tkecl(multi_end_command) $tkecl(multi_type)]
		}
		update ;# allow handler's changes to occur
	    }
	}
	interact {
	    if {$tkecl(multi_state) == "off"} {
		set peername [ec_peer_name]
		tk_messageBox -type ok -icon error -message "peer_multitask error ($peername): Trying to multitask while not multitasking."
		return -code error
	    }

	    update 
	    if {$tkecl(multi_timeslice_command) != {}} {
		switch [eval [list $tkecl(multi_timeslice_command) $tkecl(multi_type)]] {
		    terminate {
			ec_multi:terminate_phase
		    }
		}
	    }
	}
	default {
	    set peername [ec_peer_name]
	    tk_messageBox -icon error -type ok -message "peer_multitask error ($peername): unknown multitasking state message from ECLiPSe: $state."
	    return -code error
	}
    }
}

proc ec_multi:terminate_phase {} {
    global tkecl

    if {$tkecl(multi_state) == "on"} {
	ec_rpc peer_multitask_terminate
    }
}


proc ec_multi:get_multi_status {} {
    global tkecl

    return $tkecl(multi_state)

}
