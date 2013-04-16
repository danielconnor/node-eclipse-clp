#! /bin/sh
# \
	exec wish $0 ${1+"$@"}

#
# ECLiPSe Development Environment
#
# Copyright 1999 IC-Parc, Imperial College, London
#
# $Id: tkeclipse.tcl,v 1.96 2004/12/08 22:57:21 ks15 Exp $
#

#----------------------------------------------------------------------
# Find and load the eclipse package
#----------------------------------------------------------------------
set tkecl(version) 5.8	 ;# update also in eclipse_tools and examples!

switch $tcl_platform(platform) {
    unix {
	set tkecl(ECLIPSEDIR) $env(ECLIPSEDIR)
    }
    windows {
	package require registry
	set tkecl(ECLIPSEDIR) [registry get \
	    HKEY_LOCAL_MACHINE\\SOFTWARE\\IC-Parc\\Eclipse\\$tkecl(version) ECLIPSEDIR]
    }
    default {
	error "$tcl_platform(platform) not supported"
	exit
    }
}

lappend auto_path [file join $tkecl(ECLIPSEDIR) lib_tcl]


# Display a splash window (as soon as possible)

wm title . "ECLiPSe $tkecl(version) Toplevel"
wm iconname . ECLiPSe
set tkecl(ec_image) [image create photo -format ppm -file \
	[file join $tkecl(ECLIPSEDIR) lib_tcl Images eclipse_logo.ppm]]
pack [label .splash -image $tkecl(ec_image) -relief raised] -padx 5 -pady 5
update


#----------------------------------------------------------------------
# Load packages and initialise global settings
#----------------------------------------------------------------------

package require eclipse
package require eclipse_tools
package require AllWidgets

set tkecl(ec_state) Initialising
set tkecl(goal) {}
set tkecl(stop_scrolling) 0
set tkecl(history) {}
set tkecl(historypos) -1
set tkecl(nquery) 0

#----------------------------------------------------------------------
# GUI toplevel
#----------------------------------------------------------------------

# run_mode is one of: call, profile, port_profile

proc tkecl:run_goal {run_mode} {
    global tkecl

    # return if entry empty (avoids calling the goal 'end_of_file')
    if [regexp -- {^[ 	]*$} $tkecl(goal)] {
	return
    }

    switch $tkecl(ec_state) {
	More -
	Yes	{
	    ec_queue_write toplevel_in [ec_tcl2exdr end ()]
	}
	Stopped -
	"Running..." {
	    return
	}
    }
    lappend tkecl(history) $tkecl(goal)
    .tkecl.query.goal_entry add $tkecl(goal)
    set tkecl(historypos) -1
    if [winfo exists .ec_tools.history] {
	.ec_tools.history.box insert 0 $tkecl(goal)
    }
    ec_queue_write toplevel_in [ec_tcl2exdr [list $run_mode $tkecl(goal)] (S)]
    tkecl:resume_query

    if {$tkecl(pref,raise_when_done)} {
	tkinspect:RaiseWindow .
    }
    # Select old query so it can be deleted more easily.
    # Done after raising, because on Windows raising clears the selection
    .tkecl.query.goal_entry selection range 0 end
}

proc tkecl:more_goal {} {
    global tkecl

    if {$tkecl(ec_state) == "More"} {
	ec_queue_write toplevel_in [ec_tcl2exdr more ()]
	tkecl:resume_query 
    }
}


# we are about to call or backtrack into the main query
# and expect a result back on toplevel_out/answer_output

proc tkecl:resume_query {} {
    global tkecl

    set tkecl(ec_state) "Running..."
    set oldcursor [. cget -cursor]
    . configure -cursor watch
    .tkecl.query.buttons.run configure -state disabled
    .tkecl.query.buttons.make configure -state disabled
    .tkecl.query.buttons.more configure -state disabled
    tkecl:activate_abort
    tkecl:remove_current_highlights
    if [winfo exists .ec_dg] {
	.ec_dg.text tag remove highlight 1.0 end
    }
    update

    # resume async (if this is supported) to keep the interface active
    if [catch {ec_resume 1} res] {
        .tkecl.pane.stdio.tout insert end $res errorcolour
    }
 
    set tkecl(ec_state) [ec_read_exdr $tkecl(toplevel_out_channel)]
    if {$tkecl(ec_state) == "More"} {
	.tkecl.query.buttons.more configure -state normal
    }
    . configure -cursor $oldcursor
    .tkecl.query.buttons.run configure -state normal
    .tkecl.query.buttons.make configure -state normal
    tkecl:disable_abort
    if [winfo exists .ec_dg] {
	tkecl:refresh_dg
    }
    update
}

#----------------------------------------------------------------------
# History
#----------------------------------------------------------------------

proc tkecl:popup_history {} {
    global tkecl

    set history .ec_tools.history
    if ![winfo exists $history] {
	toplevel $history
	listbox $history.box -width 40 -height 12 -yscrollcommand "$history.vscroll set" -font tkeclmono
	scrollbar $history.vscroll -command "$history.box yview"
	foreach goal $tkecl(history) {
	    $history.box insert 0 $goal
	}
	bind $history.box <Double-Button-1> {
	    set tkecl(goal) [selection get]
	}
	button $history.close -text Close -command "destroy $history"
	label $history.label -text "Double-click to reuse old query"
	pack $history.close -side bottom -fill x
	pack $history.label -side bottom -fill x
	pack $history.vscroll -side left -fill y
	pack $history.box -side left -fill both -expand 1
    }
}

proc tkecl:select_history {dir} {
    global tkecl

    set size [llength $tkecl(history)]
    if {$tkecl(historypos) == -1} {
	set tkecl(historypos) [expr $size - 1]
    }

    if [string match up $dir] {
	;# directions can only be up or down
	if {$tkecl(historypos) > 0} {
	    incr tkecl(historypos) -1
	    set tkecl(goal) [lindex $tkecl(history) $tkecl(historypos)]
	} else {
	    bell
	}
    } else { ;# move down
	if {$tkecl(historypos) < $size} {
	    incr tkecl(historypos) 1
	    set tkecl(goal) [lindex $tkecl(history) $tkecl(historypos)]
	} else {
	    bell
	}
    }
}

#----------------------------------------------------------------------
# Error notification
#----------------------------------------------------------------------

proc tkecl:error_to_window {Window stream} {
    global tkecl

    set tkecl(stop_scrolling) 1
    $Window see end
    ;# make sure last error is always visible
    tkecl:tkec_stream_to_window errorcolour $Window $tkecl(stop_scrolling) $stream
}


proc tkecl:CreateImage {name format} {
    global tkecl
    return [image create photo -format $format -file [file join $tkecl(ECLIPSEDIR) lib_tcl Images $name.$format]]
}

proc tkecl:Update_current_module {name dummy op} {
    global tkecl

    set result [ec_rpc [list set_flag toplevel_module $tkecl(toplevel_module)] (()())]
    if {$result  == "throw"} {
	;# unsucessful module switch, change back to old module
	set tkecl(toplevel_module) [lindex [ec_rpc get_flag(toplevel_module,_)] 2]
    }
}

#----------------------------------------------------------------------
# About ECLiPSe
#----------------------------------------------------------------------

proc tkecl:About {} {
    global tkecl

    set w .tkecl.tkecl_about

    if [winfo exists $w] {return}
    foreach {name date} [lrange [lindex [ec_rpc_check \
	    "sepia_kernel:sepia_version_banner(Name, Date)"] 2] 1 end] {
	toplevel $w
	wm title $w "About this Eclipse"
	wm resizable $w 0 0
	set t [frame $w.f] 
	pack [label $t.ic -image [tkecl:CreateImage iccrest gif]] -side left
	pack [label $t.icp -image [tkecl:CreateImage ic-parc gif]] -side top
#	pack [label $t.ec -image $tkecl(ec_image)] -side left
	pack [label $t.n -text "$name (Tcl/Tk GUI)"] -side top
	pack $t -side top -padx 10 -pady 10
	pack [label $w.blah -text {
ECLiPSe is not public domain software. To use it, you or your institution
or company is obliged to sign a licence agreement with IC-Parc,
Imperial College, London SW7 2AZ, Fax +44 20 75948432.
See http://www.icparc.ic.ac.uk/eclipse for details.
	}] -side top -expand 1 -fill x -padx 10 -pady 10
	pack [button $w.ok -text OK -command "destroy $w"] \
		-ipady 10 -padx 10 -pady 10 -side bottom -fill x -expand 1
    }
}

# taken and modified from cgi.tcl, by Don Libes 
# return string quoted appropriately to appear in a url
proc cgi_quote_url {in} {
    regsub -all {%}  $in "%25" in
    regsub -all {#}  $in "%23" in
    regsub -all { }  $in "%20" in
    regsub -all {"}  $in "%22" in
    regsub -all {;}  $in "%3b" in
    regsub -all {=}  $in "%3d" in
    regsub -all {\?} $in "%3f" in
    return $in
}

proc tkecl:Documentation {} {
    global tcl_platform env
    set htmldoc [lindex [lindex [ec_rpc "tracer_tcl:return_html_root(_)"] 2] 1]
    switch $tcl_platform(platform) {
	unix { exec netscape -remote openFile([cgi_quote_url $htmldoc]) }
	windows { exec $env(COMSPEC) /c start [cgi_quote_url $htmldoc] & }
    }
}

#----------------------------------------------------------------------
# Selecting a query's output
#----------------------------------------------------------------------

proc tkecl:Select_query_outputs {w other} {
    set left [$w mark previous current]
    set right [$w mark next current]

    while {![regexp -- {^q[0-9]+$} $left]} {
	if [string match "" $left] {
	    ;# got to left edge
	    set left 1.0
	    break
	}
	;# repeat until a qN mark found
	set left [$w mark previous $left]
    }

    while {![regexp -- {^q[0-9]+$} $right]} {
	if [string match "" $right] {
	    ;# got to right edge
	    set right end
	    break
	}
	;# repeat until a qN mark found
	set right [$w mark next $right]
    }

    set notsame 1
    foreach {oldl oldr} [$w tag ranges qsel] {
	if {($oldl == [$w index $left] && $oldr == [$w index $right])} {
	    set notsame 0
	} else {
	    set notsame 1
	}
    }
    $w tag remove qsel 1.0 end
    if {$notsame} {
	$w tag add qsel $left $right
	$w see "$right -1 lines"
    }

    $other tag remove qsel 1.0 end
    if {$notsame} {
	$other tag add qsel $left $right
	$other see "$right -1 lines"
    }
    return
}

#triple click selects all earlier queries
proc tkecl:Select_earlier_queries {w other} {
    set right [$w mark next current]

    while {![regexp -- {^q[0-9]+$} $right]} {
	if [string match "" $right] {
	    ;# got to right edge
	    set right end
	    break
	}
	;# repeat until a qN mark found
	set right [$w mark next $right]
    }

    $w tag remove qsel 1.0 end
    $w tag add qsel 1.0 $right
    # no need to see right; should already be there because of double match

    $other tag remove qsel 1.0 end
    $other tag add qsel 1.0 $right

    return
}

proc tkecl:toplevel_keypress {keysym} {
# used to avoid inserting printing characters 
# (Control, Meta sequences should be allowed seperarely)

    switch  $keysym {
	"Delete" -
	"BackSpace" {  ;# delete and backspace
	    foreach t {.tkecl.pane.stdio.tout .tkecl.pane.answer.tout} {
		foreach {left right} [$t tag ranges qsel] {
		    $t delete $left $right
		}
	    }
	    return -code break
	}
	"Home"   -
	"Prior"  -
	"Next"   -
	"Up"     -
	"Down"   -
	"Left"   -
	"Right" {    ;# special one-key, default allowed
	    return 0
	}
	
	default {
	    return -code break
	}
    }
}

# pop up a menu called $y.popup over the text widget $t
proc tkecl:output_popup {t X Y} {
    if {[$t tag ranges sel] != ""} {
        $t.popup entryconfigure "Copy*" -state normal
    } else {
        $t.popup entryconfigure "Copy*" -state disabled
    }
    tk_popup $t.popup $X $Y
}


# copy the selection of a text widget to the clipboard
proc tkecl:copy_selection {t} {
    if {[$t tag ranges sel] != ""} {
	clipboard clear
	clipboard append [$t get sel.first sel.last]
    }
}

#----------------------------------------------------------------------
# Make the existing outputs in stdio and answer windows non-current 
#----------------------------------------------------------------------

proc tkecl:remove_current_highlights {} {
    global tkecl

    .tkecl.pane.stdio.tout tag remove highlight 1.0 end
    .tkecl.pane.stdio.tout tag remove errorcolour 1.0 end
    .tkecl.pane.stdio.tout tag remove warning 1.0 end
    .tkecl.pane.answer.tout tag remove highlight 1.0 end
    .tkecl.pane.answer.tout tag remove errorcolour 1.0 end
    .tkecl.pane.answer.tout tag remove successcolour 1.0 end
    .tkecl.pane.stdio.tout mark set q$tkecl(nquery) "end -1 chars"
    .tkecl.pane.stdio.tout mark gravity q$tkecl(nquery) left
    .tkecl.pane.answer.tout mark set q$tkecl(nquery) "end -1 chars"
    .tkecl.pane.answer.tout mark gravity q$tkecl(nquery) left
    incr tkecl(nquery) 1
    set tkecl(stop_scrolling) 0
}

#------------------------------------------------------------------------
#  Wrapper around Tcl commands that should only be executed at
#  "toplevel", i.e. when there are no active queries
#-----------------------------------------------------------------------
proc tkecl:exec_toplevel_command {command} {
    global tkecl

    switch $tkecl(ec_state) {
	More -
	Yes    {
	    ;# need to clean up any existing query before command
	    set tkecl(ec_state) Yes
	    .tkecl.query.buttons.more configure -state disabled
	    ec_queue_write toplevel_in [ec_tcl2exdr end ()]
	    if [catch {ec_resume 1} res] {
		.tkecl.pane.stdio.tout insert end $res errorcolour
	    }
	}

	"Running..."  {
	    ;# cannot execute command....
	    bell
	    return
	}
    }

    eval $command
}

#-------------------------------------------------------------------------
# A more sohisticated queue_out_handler; used for error stream
# added ScrollControl and TruncateLength
#-------------------------------------------------------------------------
proc tkecl:tkec_stream_to_window {Tag Window ScrollControl Stream} {
    global tkecl

    set data [ec_queue_read $Stream 1000]
    while {$data != ""} {
	regexp {^([0-9]+)[.]([0-9]+)$} [$Window index end-1char] whole line charp
	if {$charp < $tkecl(pref,text_truncate)} {
	    $Window insert end $data $Tag
	} else {
	    ;# truncate printing of line if too long
	    if {[lsearch [$Window tag names] trunc] != -1} {
		;# not yet defined...
		$Window tag configure trunc -background pink
	    }
	    if {[lsearch [$Window tag names end-2char] trunc] == -1} {
		;# line is first truncated. Note -2 needed (rather than -1)
		$Window insert end "..." trunc
	    }
	    set nl [string first "\n" $data]
	    if {$nl != -1} {
		;# if there is a nl, then a new line was started
		$Window insert end [string range $data $nl end] $Tag
	    }
	}
	set data [ec_queue_read $Stream 1000]
    }

    if {!$ScrollControl || !$tkecl(stop_scrolling)} {
	$Window see end
    }
}


#------------------------------------------------------------------------
# creating + initialising modules
#------------------------------------------------------------------------

proc tkecl:new_module_popup {} {
    global tkecl

    set w .tkecl.new_module_popup
    if {![winfo exists $w]} {
	set tkecl(new_module_name) ""
	set tkecl(new_module_language) "eclipse_language"
	toplevel $w
	wm title $w "Create New Module"
	label $w.ml -text "Module name:" -anchor w
	entry $w.me -textvariable tkecl(new_module_name) -relief sunken -bg white
	grid $w.ml $w.me -sticky news 
	label $w.ll -text "with language:" -anchor w
	entry $w.le -textvariable tkecl(new_module_language) -relief sunken -bg white
	grid $w.ll $w.le -sticky news 
	bind $w.me <Return> "tkecl:create_module $w"
	bind $w.le <Return> "tkecl:create_module $w"
	button $w.ok -text "OK" -command "tkecl:create_module $w"
	button $w.cancel -text "Cancel" -command "destroy $w"
	grid $w.ok $w.cancel -sticky news 
	grid columnconfigure $w 0 -weight 1
	grid columnconfigure $w 1 -weight 1
	grid rowconfigure $w 0 -weight 1
	grid rowconfigure $w 1 -weight 1
	grid rowconfigure $w 2 -weight 1
	focus $w.me
	balloonhelp $w.ml "Name of module to be created. Type <Ret> or click on OK to create module."
	balloonhelp $w.ll "Name of language to be loaded with module. Type <Ret> or click on OK to create module."
	balloonhelp $w.ok "Click to create specified module"
	balloonhelp $w.cancel "Click to cancel without creating module"

    } else {
	tkinspect:RaiseWindow $w
    }
}

proc tkecl:create_module {w} {
    global tkecl

    switch [ec_rpc "current_module($tkecl(new_module_name))"] {
	throw {
	    tk_messageBox -type ok -icon error -message "Invalid module name: cannot create module $tkecl(new_module_name)"
	    return
	}
	fail {}
	default {
	    switch [tk_messageBox -default yes -type yesno -icon question -message \
		    "Module $tkecl(new_module_name) is an existing module. Do you want to try to reinitialise it?"] {
		yes {
		    if {[ec_rpc "erase_module($tkecl(new_module_name))"] == "throw"} {
			tk_messageBox -type ok -icon error -message "Unable to erase module"
			return
		    }
		}
		no  { return }
	    }
	  
	}
    }

    append goal "create_module(" $tkecl(new_module_name) "," {[]} ", $tkecl(new_module_language))"
    switch  [ec_rpc $goal] {
	fail  -
	throw {
	    ec_rpc "erase_module($tkecl(new_module_name))"  ;# clean up
	    tk_messageBox -type ok -icon error -message "Unable to create module $tkecl(new_module_name) with language $tkecl(new_module_language)"
	}
	default {
	    set tkecl(toplevel_module) $tkecl(new_module_name)
	    destroy $w
	}
    }

}

proc tkecl:init_toplev_module {} {
    global tkecl

    if {[tk_messageBox -default ok -type okcancel -icon warning -message "This will erase the current content of module '$tkecl(toplevel_module)'"] == "ok"} {
	ec_rpc "tracer_tcl:init_toplevel_module"
    }
}


#------------------------------------------------------------------------
# default settings
#------------------------------------------------------------------------

proc tkecl:set_toplevel_defaults {} {
    global tkecl


    lappend tkecl(preferences) \
	    {globalsize "" +integer tkeclipserc "Global/trail stack size (in megabytes)"} \
            {localsize  "" +integer tkeclipserc "Local/Control stack size (in megabytes)"} \
	    {default_module "" string tkeclipserc "Default module name"} \
	    {initquery "" string tkeclipserc "Initial query called by TkECLiPSe on start-up"} \
	    {raise_when_done 1 boolean tkeclipserc "Raise toplevel window when query finishes"}

    set tkecl(pref,globalsize) ""
    set tkecl(pref,localsize) ""
    set tkecl(pref,initquery) ""
    set tkecl(pref,default_module) ""
    set tkecl(pref,raise_when_done) 1

    set toplevdefaults [tkecl:get_user_defaults tkeclipserc]

    foreach dname $toplevdefaults {
	set dvalue $tkecl(prefset,$dname)

	if {[string trimleft $dvalue] != ""} {
	    switch -exact -- $dname {
		globalsize -
		localsize {
		    if [regexp {^[0-9]+$} $dvalue size] { 
			;# make sure it is a number!
			ec_set_option $dname [expr $dvalue * 1048576] ;# in megabytes 
			set tkecl(pref,$dname) $dvalue
		    } else {
			tk_messageBox -icon warning -message "$dname parameter: $dvalue should be a number" -type ok
		    }
		}
		default_module {
		    set tkecl(pref,$dname) $dvalue
		    ec_set_option $dname $dvalue
		}

		default {set tkecl(pref,$dname) $dvalue }
	    }
	}
    }

}

#----------------------------------------------------------------------
# Start of toplevel initialisation code
#----------------------------------------------------------------------
lappend tkecl(helpfiles) topl "TkECLiPSe Toplevel" toplevelhelp.txt
tkecl:set_tkecl_tkdefaults tkecl
frame .tkecl
tkecl:set_toplevel_defaults 

#----------------------------------------------------------------------
# Make the toplevel window
#----------------------------------------------------------------------

menu .tkecl.mbar
. config -menu .tkecl.mbar
.tkecl.mbar add cascade -label "File" -menu .tkecl.mbar.file -underline 0
menu .tkecl.mbar.file
.tkecl.mbar.file add command -label "Compile ..." -command {tkecl:exec_toplevel_command {tkecl:remove_current_highlights; tkecl:compile_popup}}
.tkecl.mbar.file add command -label "Use module ..." -command {tkecl:exec_toplevel_command {tkecl:remove_current_highlights; tkecl:use_module_popup}}
.tkecl.mbar.file add command -label "Edit ..." -command tkecl:edit_popup
.tkecl.mbar.file add command -label "Edit new ..." -command tkecl:edit_new_popup
.tkecl.mbar.file add command -label "Cross referencer ..." -command {tkecl:exec_toplevel_command tkecl:xref_popup}
.tkecl.mbar.file add separator
.tkecl.mbar.file add command -label "Change directory ..." -command {tkecl:remove_current_highlights; tkecl:get_newcwd}
.tkecl.mbar.file add command -label "Change to example directory" -command {
	tkecl:newcwd [file join $tkecl(ECLIPSEDIR) doc examples]
	tk_messageBox -type ok -message "Changed directory to $tkecl(cwd)"
    }
.tkecl.mbar.file add separator
.tkecl.mbar.file add command -label "New module ..." -command {tkecl:exec_toplevel_command tkecl:new_module_popup}
.tkecl.mbar.file add command -label "Clear toplevel module" -command {tkecl:exec_toplevel_command tkecl:init_toplev_module}
.tkecl.mbar.file add separator
.tkecl.mbar.file add command -label Exit -command {destroy .}

.tkecl.mbar add cascade -label "Query" -underline 0 -menu .tkecl.mbar.run
menu .tkecl.mbar.run
.tkecl.mbar.run add command -label "Run" -command {tkecl:run_goal call}
.tkecl.mbar.run add command -label "Time Profile" -command {tkecl:run_goal profile}
.tkecl.mbar.run add command -label "Port Profile" -command {tkecl:run_goal port_profile}
.tkecl.mbar.run add separator
.tkecl.mbar.run add command -label "History" -command {tkecl:popup_history}
switch $tcl_platform(platform) {
    # currently not supported on Windows
    windows { .tkecl.mbar.run entryconfigure "Time Profile" -state disabled }
}

.tkecl.mbar add cascade -label "Tools" -underline 0 -menu .tkecl.mbar.windows

.tkecl.mbar add cascade -label "Help" -menu .tkecl.mbar.help -underline 0
menu .tkecl.mbar.help
.tkecl.mbar.help add command -label "About this ECLiPSe ..." -command tkecl:About
.tkecl.mbar.help add command -label "Full Documentation ..." -command tkecl:Documentation
.tkecl.mbar.help add separator
.tkecl.mbar.help add check -label "Balloon Help" -variable tkecl(pref,balloonhelp)
.tkecl.mbar.help add separator

#----------------------------------------------------------------------
frame .tkecl.query -relief groove -bd 3
#----------------------------------------------------------------------
label .tkecl.query.label -text "Query Entry"
#label .tkecl.query.module -textvariable tkecl(toplevel_module)
combobox .tkecl.query.module -click single -listheight 6 -width 10 -editable 0 \
	-postcommand {tkecl:combo_add_modules .tkecl.query.module} \
	-textvariable tkecl(toplevel_module)
label .tkecl.query.colon -text ":"

trace variable tkecl(toplevel_module) w tkecl:Update_current_module
frame .tkecl.query.buttons
button .tkecl.query.buttons.make -text "make" -command \
    {tkecl:exec_toplevel_command {tkecl:remove_current_highlights; \
		ec_rpc "make,flush(output),flush(error), flush(warning_output)"}}
button .tkecl.query.buttons.run -text "run" -command {tkecl:run_goal call}
button .tkecl.query.buttons.more -text "more" -command tkecl:more_goal

frame .tkecl.query.buttons.abort

#entry .tkecl.query.goal_entry -bg white -width 80 -textvariable tkecl(goal)

option add *tkecl.query.goal_entry*Listbox.font tkeclmono
combobox .tkecl.query.goal_entry -click single -listheight 6 -bg white -width 65 \
	-textvariable tkecl(goal) -takefocus 1
bind .tkecl.query.goal_entry <Return> {tkecl:run_goal call}
bind .tkecl.query.goal_entry <Button-3> {tkecl:popup_history}
bind .tkecl.query.goal_entry <Control-Button-1> {tkecl:popup_history}
bind .tkecl.query.goal_entry <Key-Up> {tkecl:select_history up}
bind .tkecl.query.goal_entry <Key-Down> {tkecl:select_history down}

label .tkecl.query.buttons.status -bg white -relief sunken -width 20 -textvariable tkecl(ec_state)

#----------------------------------------------------------------------
# Answer binding window and output/error window
# they are together in a frame and managed by the pane-manager
#----------------------------------------------------------------------
frame .tkecl.pane -height 12c

frame .tkecl.pane.answer -relief groove -bd 3
scrollbar .tkecl.pane.answer.vscroll -command ".tkecl.pane.answer.tout yview"
scrollbar .tkecl.pane.answer.hscroll -command ".tkecl.pane.answer.tout xview" -orient horizontal
#text .tkecl.pane.answer.tout -bg white -height 15 -yscrollcommand ".tkecl.pane.answer.vscroll set" -wrap none -xscrollcommand ".tkecl.pane.answer.hscroll set"
text .tkecl.pane.answer.tout -bg white -width 80 -yscrollcommand ".tkecl.pane.answer.vscroll set" -wrap none -xscrollcommand ".tkecl.pane.answer.hscroll set" 
label .tkecl.pane.answer.label -text "Results"
.tkecl.pane.answer.tout tag configure highlight -foreground blue
.tkecl.pane.answer.tout tag configure errorcolour -foreground red
.tkecl.pane.answer.tout tag configure successcolour -foreground #00b000
.tkecl.pane.answer.tout tag configure qsel -background lightblue
menu .tkecl.pane.answer.tout.popup -tearoff 0
.tkecl.pane.answer.tout.popup add command -label "Copy selection to clipboard" -command "tkecl:copy_selection .tkecl.pane.answer.tout"
.tkecl.pane.answer.tout.popup add command -label "Highlight corresponding output" -command "tkecl:Select_query_outputs .tkecl.pane.answer.tout .tkecl.pane.stdio.tout"
.tkecl.pane.answer.tout.popup add command -label "Clear this window" -command ".tkecl.pane.answer.tout delete 1.0 end"
bind .tkecl.pane.answer.tout <Any-Key> "tkecl:toplevel_keypress %K"
bind .tkecl.pane.answer.tout <Control-Key> "continue"
bind .tkecl.pane.answer.tout <Meta-Key> "continue"
# allow ^C to work as copy in window
bind .tkecl.pane.answer.tout <ButtonRelease-2> {break} 
bind .tkecl.pane.answer.tout <Button-3> {tkecl:output_popup .tkecl.pane.answer.tout %X %Y}
bind .tkecl.pane.answer.tout <Control-Button-1> \
    {tkecl:output_popup .tkecl.pane.answer.tout %X %Y}
#bind .tkecl.pane.answer.tout <Double-Button-3> "tkecl:Select_query_outputs .tkecl.pane.answer.tout .tkecl.pane.stdio.tout"
#bind .tkecl.pane.answer.tout <Triple-Button-3> "tkecl:Select_earlier_queries .tkecl.pane.answer.tout .tkecl.pane.stdio.tout"

#pack .tkecl.pane.answer.vscroll -side left -fill y
#pack .tkecl.pane.answer.label -side top -fill x
#pack .tkecl.pane.answer.hscroll -side bottom -fill x
#pack .tkecl.pane.answer.tout -side bottom -expand 1 -fill both

pack .tkecl.pane.answer.label -side top -fill x
pack .tkecl.pane.answer.vscroll -side left -fill y
pack .tkecl.pane.answer.hscroll -side bottom -fill x
pack .tkecl.pane.answer.tout -expand 1 -fill both


frame .tkecl.pane.stdio -relief groove -bd 3
scrollbar .tkecl.pane.stdio.vscroll -command ".tkecl.pane.stdio.tout yview"
scrollbar .tkecl.pane.stdio.hscroll -command ".tkecl.pane.stdio.tout xview" -orient horizontal
text .tkecl.pane.stdio.tout -width 80 -bg white -height 15 -yscrollcommand ".tkecl.pane.stdio.vscroll set" -wrap none -xscrollcommand ".tkecl.pane.stdio.hscroll set"
.tkecl.pane.stdio.tout tag configure highlight -foreground blue
.tkecl.pane.stdio.tout tag configure warning -foreground orange
.tkecl.pane.stdio.tout tag configure errorcolour -foreground red
.tkecl.pane.stdio.tout tag configure nohandlercolour -foreground green
.tkecl.pane.stdio.tout tag configure qsel -background lightblue
label .tkecl.pane.stdio.label -text "Output and Error Messages"
menu .tkecl.pane.stdio.tout.popup -tearoff 0
.tkecl.pane.stdio.tout.popup add command -label "Copy selection to clipboard" -command "tkecl:copy_selection .tkecl.pane.stdio.tout"
.tkecl.pane.stdio.tout.popup add command -label "Highlight corresponding query" -command "tkecl:Select_query_outputs .tkecl.pane.stdio.tout .tkecl.pane.answer.tout"
.tkecl.pane.stdio.tout.popup add command -label "Clear this window" -command ".tkecl.pane.stdio.tout delete 1.0 end"

pack .tkecl.pane.stdio.label -side top -fill x
pack .tkecl.pane.stdio.vscroll -side left -fill y
pack .tkecl.pane.stdio.hscroll -side bottom -fill x
pack .tkecl.pane.stdio.tout -expand 1 -fill both
bind .tkecl.pane.stdio.tout <Any-Key> "tkecl:toplevel_keypress %K"
bind .tkecl.pane.stdio.tout <Control-Key> "continue"
bind .tkecl.pane.stdio.tout <Meta-Key> "continue"
bind .tkecl.pane.stdio.tout <ButtonRelease-2> {break}
bind .tkecl.pane.stdio.tout <Button-3> {tkecl:output_popup .tkecl.pane.stdio.tout %X %Y}
bind .tkecl.pane.stdio.tout <Control-Button-1> \
    {tkecl:output_popup .tkecl.pane.stdio.tout %X %Y}
#bind .tkecl.pane.stdio.tout <Double-Button-3> "tkecl:Select_query_outputs .tkecl.pane.stdio.tout .tkecl.pane.answer.tout"
#bind .tkecl.pane.stdio.tout <Triple-Button-3> "tkecl:Select_earlier_queries .tkecl.pane.stdio.tout .tkecl.pane.answer.tout"
bind .tkecl.pane.stdio.vscroll <ButtonRelease-1> "set tkecl(stop_scrolling) 0"

pane .tkecl.pane.answer .tkecl.pane.stdio -orient vertical -initfrac [list 0.35 0.65]


#----------------------------------------------------------------------
# Pack the toplevel window
#----------------------------------------------------------------------

pack .tkecl.query -side top -fill x
pack .tkecl.pane -side top -fill both -expand 1

pack .tkecl.query.buttons.run .tkecl.query.buttons.more -side left -expand 1 -fill x
pack .tkecl.query.buttons.status -side left -fill y
pack .tkecl.query.buttons.make -side left -expand 1 -fill x
pack .tkecl.query.buttons.abort -side left -expand 1 -fill x
pack .tkecl.query.label -side top -fill x -expand 1
pack .tkecl.query.buttons -side bottom -fill x -expand 1
pack .tkecl.query.module -side left
pack .tkecl.query.colon -side left
pack .tkecl.query.goal_entry -side left -expand 1 -fill x
focus [.tkecl.query.goal_entry subwidget entry]


#----------------------------------------------------------------------
# The abort button
#
# On Unix, the abort button is implemented using a separate process
# in order to allow aborts while eclipse is running; with X11,
# this process is placed in the TkECLiPSe window using a container,
# which is started only after the TkECLiPSe window is displayed.
# Aqua does not allow the container mechanism, so the button is implemented
# as an independent window. In both cases, the abort button and TkECLiPSe
# are coordinated via a socket connection.
#----------------------------------------------------------------------

switch $tcl_platform(platform) {
    unix {

	proc tkecl:abort_button_connect {} {
	    global tkecl
	    # start from a high port number (1024-5000 apparently often
            # used by OS's client programs) and work upwards until a free port
	    set port 5001 
	    while {[catch  "socket -server tkecl:abort_button_accepted $port" tkecl(abort,server)]} {
		incr port 1
	    }
	    return $port
	}

	proc tkecl:abort_button_accepted {abort_channel addr port} {
	    global tkecl
	    set tkecl(abort,channel) $abort_channel
	    fileevent $abort_channel readable "tkecl:from_abort_button $abort_channel"
	    catch {close $tkecl(abort,server)}
	}

	proc tkecl:from_abort_button {abort_channel} {
	    ;# process output from tkabortbutton; currently only eof
	    if [eof $abort_channel] {
		;# eof if tkabortbutton was killed, recreate it
		catch {close $abort_channel}

		;# catch for case when ThECLiPSe was destroyed
		catch { tkecl:create_abort_button }
		return
	    }
	    gets $abort_channel line
	}

	proc tkecl:create_abort_button {} {
	    global tkecl

	    set port [tkecl:abort_button_connect]
	    switch [ec_tk_platform] {
		unix_aqua {
		    exec [info nameofexecutable] \
			[file join $tkecl(ECLIPSEDIR) lib_tcl tkabortbutton] \
			[pid] standalone $port &
		}
		unix_x11 {
		    exec [info nameofexecutable] \
			[file join $tkecl(ECLIPSEDIR) lib_tcl tkabortbutton] \
			-use [winfo id .tkecl.query.buttons.abort.abort_frame] \
                        [pid] embedded $port &
		}
	    }

	    vwait tkecl(abort,channel)
	    tkecl:disable_abort
	}

	proc tkecl:disable_abort {} {
	    global tkecl
	    puts $tkecl(abort,channel) disable
	    flush $tkecl(abort,channel)
	}

	proc tkecl:activate_abort {} {
	    global tkecl
	    puts $tkecl(abort,channel) activate
	    flush $tkecl(abort,channel)
	}

	switch [ec_tk_platform] {
	    unix_x11 {
		frame .tkecl.query.buttons.abort.abort_frame -container true
		pack .tkecl.query.buttons.abort.abort_frame -expand 1 -fill both
	    }
	    unix_aqua {
		pack forget .tkecl.query.buttons.abort
		tkecl:create_abort_button
	    }
	}
    }

    windows {
	button .tkecl.query.buttons.abort.abort_button -text interrupt -command "ec_post_event int"
	pack .tkecl.query.buttons.abort.abort_button -expand 1 -fill both

	proc tkecl:disable_abort {} {
	    .tkecl.query.buttons.abort.abort_button configure -state disabled
	}
	proc tkecl:activate_abort {} {
	    .tkecl.query.buttons.abort.abort_button configure -state normal
	}
    }
}


proc tkecl:stop_request_handler {stream} {
    global tkecl
    set event [ec_read_exdr ec_queue$stream]
    if ![winfo exists .tkecl.ec_stop_continue_box] {
        # We don't use a tk_messageBox or tk_dialog because they are modal.
	toplevel .tkecl.ec_stop_continue_box
	wm title .tkecl.ec_stop_continue_box "ECLiPSe interrupt"
	label .tkecl.ec_stop_continue_box.msg -relief raised -height 3 -width 50 \
		-text "Execution interrupted - do you want to abort?"
	button .tkecl.ec_stop_continue_box.abort -text "Yes, abort" \
	    -command {set tkecl(stop_continue) abort; destroy .tkecl.ec_stop_continue_box}
	button .tkecl.ec_stop_continue_box.cont -text "No, continue" \
	    -command {set tkecl(stop_continue) cont; destroy .tkecl.ec_stop_continue_box}
	button .tkecl.ec_stop_continue_box.creep -text "Continue in creep mode" \
	    -command {set tkecl(stop_continue) creep; destroy .tkecl.ec_stop_continue_box}
	pack .tkecl.ec_stop_continue_box.msg -side top -fill both -expand 1
	pack .tkecl.ec_stop_continue_box.abort -side left -expand 1 -pady 3m -padx 3m
	pack .tkecl.ec_stop_continue_box.cont -side left -expand 1 -pady 3m -padx 3m
	pack .tkecl.ec_stop_continue_box.creep -side left -expand 1 -pady 3m -padx 3m

	switch [lindex [ec_rpc get_flag(debugging,_)] 2] {
	    nodebug { .tkecl.ec_stop_continue_box.creep configure -state disabled }
	}

	tkwait variable tkecl(stop_continue)
	switch $tkecl(stop_continue) {
	    abort {ec_post_event abort}
	    creep {ec_rpc sepia_kernel:trace_mode(0,0)}
	}
    }
}

#----------------------------------------------------------------------
# Balloon Help
#----------------------------------------------------------------------

balloonhelp .tkecl.query.goal_entry "Query entry - type query in here (terminating `.' optional). <Ret> or run to execute \n Up and down arrows moves through previous queries, <Tab> for query completion\n Left-click arrow on right-hand side for selecting previous queries (non-duplicated)\n Right-click (or control-left) popups history window (duplicated, most recent first)"
balloonhelp .tkecl.query.buttons.status "Status after executing last query (Yes/More/No/Abort)"
balloonhelp .tkecl.query.buttons.more "Click to try find more solutions"
balloonhelp .tkecl.query.buttons.run "Click to start execution of query in query entry.\n Note: Execution will restart even if there are pending solutions to an executing query."
balloonhelp .tkecl.query.buttons.make "Click to recompile modified files"
balloonhelp .tkecl.query.buttons.abort "Click to interrupt executing query"
balloonhelp .tkecl.query.module "Module in which the query will be executed\nClick arrow on right to select a different module"
balloonhelp .tkecl.pane.answer.label "Results window - results (top-level bindings and status after execution) are displayed here.\n Results for the most recent query are in blue.\n\
Right (or control-left) to popup a menu to copy selection to clipboard, match a query's outputs, or clear the window."
balloonhelp .tkecl.pane.stdio.label "Output and Error Messages from Eclipse are displayed here.\n Most recent outputs are in blue, error messages are in red, warnings in orange.\n\
Scrolling is disabled by warning and error messages. Left-click on scrollbar to re-enable scrolling.\n\
Right (or control-left) click to popup a menu to copy selection to clipboard, match a query's outputs, or clear the window."
balloonhelp .tkecl.pane.__h1 "Press and drag left mouse to adjust Results and Output window sizes"
# bind . <Alt-h> "tkecl:Get_helpfileinfo topl {}" get help menu

#----------------------------------------------------------------------
# Initialise and start eclipse toplevel
#----------------------------------------------------------------------

#ec_set_option io 0

ec_init
bind .tkecl.query.label <Destroy> {ec_rpc "event(152)"; ec_cleanup}

pack forget .splash
destroy .splash
pack .tkecl -expand true -fill both

# The exec is started only after .tkecl is packed. Otherwise tkabortbutton
# might be executed before there is a window and this leads to an error
if {[ec_tk_platform] == "unix_x11"} {
    tkecl:create_abort_button
}

ec_tools_init .tkecl.mbar.windows

foreach {key topic filename} $tkecl(helpfiles) {
    .tkecl.mbar.help add command -label $topic -command "tkecl:Get_helpfileinfo $key {}"
}

# use the more sophisticated version of ec_stream_to_window for more control
ec_set_queue_handler output r {tkecl:tkec_stream_to_window highlight .tkecl.pane.stdio.tout 1}
ec_set_queue_handler error r "tkecl:error_to_window .tkecl.pane.stdio.tout"

# ensure_loaded rather than use_module: we don't want to import
ec_rpc "ensure_loaded(library(toplevel))"
ec_rpc "toplevel:toplevel_init(gui)"

ec_queue_connect gui_interrupt_request r tkecl:stop_request_handler
ec_set_queue_handler answer_output r "ec_stream_to_window highlight .tkecl.pane.answer.tout"
ec_set_queue_handler warning_output r "tkecl:tkec_stream_to_window warning .tkecl.pane.stdio.tout $tkecl(stop_scrolling)"
set tkecl(toplevel_out_channel) [ec_queue_connect toplevel_out r]

if {![string match $tkecl(version) [lindex [ec_rpc_check "get_flag(version, V)"] 2]]} {
    tk_messageBox -icon warning -message "Version differences detected between Tcl and ECLiPSe codes" -type ok
}

if {[string trimleft $tkecl(pref,initquery)] != ""} {
    ec_rpc_check $tkecl(pref,initquery)
}

ec_post_goal "toplevel:toplevel"
tkecl:resume_query 


