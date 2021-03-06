# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.

package ifneeded eclipse_peer_multitask 1.0 [list source [file join $dir tkmulti.tcl]]
package ifneeded remote_eclipse 1.0 [list source [file join $dir tkec_remote.tcl]]
package ifneeded eclipse 1.0 [list tclPkgSetup $dir eclipse 1.0 {{eclipse.tcl source {ec_init ec_set_option ec_tk_platform}}}]
package ifneeded eclipse_tools 1.0 [list tclPkgSetup $dir eclipse_tools 1.0 {{eclipse_tools.tcl source {ec_tools_init tkecl:get_defaults tkecl:set_tkecl_tkdefaults tkecl:tracer_popup tkecl:kill_tracer}}}]
package ifneeded tkinspect 1.0 [list tclPkgSetup $dir tkinspect 1.0 {{tkinspect.tcl source {tkinspect:CreateImage tkinspect:CurrentSelection tkinspect:Display tkinspect:DisplayImage tkinspect:DisplayKey tkinspect:DisplayPath tkinspect:Exit tkinspect:Expand_termtype tkinspect:Get_numentry tkinspect:Get_subterm_info tkinspect:Get_subterms tkinspect:Inspect_term_init tkinspect:Look_term tkinspect:Modify_name tkinspect:Move tkinspect:MoveDown tkinspect:Newselection tkinspect:Numentry tkinspect:Popnumentry tkinspect:PostSelect tkinspect:SelectCurrent tkinspect:SelectInvoc tkinspect:Setup_move tkinspect:ToggleSymbols tkinspect:Update_menulabel tkinspect:Update_optmenu tkinspect:Write_command_initial tkinspect:Write_path tkinspect:ec_resume_inspect tkinspect:RaiseWindow tkinspect:helpinfo tkinspect:inspect_command}}}]





