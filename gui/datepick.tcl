# datepick.tcl - a text subwidget for letting a user edit/pick one or more date/times
#                It tags portions of a text widget to represent
#                date/time components in a standard format, and binds
#                keystrokes / mousewheel events to them.  At any time,
#                the most recently validated date is available in the
#                variable used to create the datepick subwidget.
#
# (C) 2007 by John Brzustowski
# Licence: GPL
#

namespace eval Datepick {
    ## date formats for 12 and 24 hour clocks
    array set tformat {
	12 { %d-%b-%Y %I:%M:%S %p }
	24 { %d-%b-%Y %H:%M:%S }
    }
    ## widths of date formats
    array set tformat_w {
	12 26
	24 23
    }

    ## amount by which the date/time changes for a cursor Up/Down
    ## event in each character position in the formatted strings
    ## We use the same array for both 12 and 24 hour clocks.

    array set delta {0 ""
	1 "10 days"
	2 "1 days"
	3 "1 day"
	4 "1 month"
	5 "1 month"
	6 "1 month"
	7 "1 month"
	8 "1 year"
	9 "1 year"
	10 "1 year"
	11 "1 year"
	12 "1 year"
	13 "10 hours"
	14 "1 hour"
	15 "1 hour"
	16 "10 minutes"
	17 "1 minute"
	18 "1 minute"
	19 "10 seconds"
	20 "1 second"
	21 "1 second"
	22 "12 hours"
	23 "12 hours"
	24 "12 hours"
    }

    ## background colours
    ## FIXME: make this configurable at datepick creation time

    array set bg {
	normal \#ffffff
	changed yellow
	error red
    }

    ## default text widget options
    variable textopts {-pady 5 -padx 3 -font {Fixed 17}}

    ## tag prefix
    variable tag_prefix datepick_

    proc change {name sign {x ""} {y ""}} {
	# change the date/time up or down based
	# on an event happening at text widget coordinates x,y
	# (or at the insertion point, if x,y is empty)
	# if sign is "+", increase; if sign is "-", decrease
	# Maintains the current insert position.
	# Returns 1 if a datepick item was changed, 0 otherwise

	set which [Datepick::which $name $x $y]
	set var [lindex $which 0]
	if {"$var" != ""} {
	    set tname [lindex $which 1]
	    catch {
		set pos [$name index insert]
		if {"$x" == ""} {
		    set w [string length [$name get $tname.first $pos]]
		} else {
		    set w [string length [$name get $tname.first @$x,$y]]
		}
		set ::$var [clock scan [join [list $sign $Datepick::delta($w)] {}] -base [expr "\$::$var"]]
		[$name mark set insert $pos]
	    }
	    return 1
	}
	return 0
    }

    proc setvar {name var hours ign1 ign2 ign3} {
	# the value of variable var has been changed;
	# update the corresponding datepick subwidget to match it
	# We look for a tagged region corresponding to var
	# and if none is found, insert a new datepick
	# widget at the current insertion point of text widget "name"
	# ign1, ign2, and ign3 are ignored
	set tname $Datepick::tag_prefix$var
	if {
	    [catch {
		set first [$name index $tname.first]
		$name delete $tname.first $tname.last
	    }]
	} {
	    set first [$name index insert]
	}
	$name insert $first [clock format [expr int(\$::$var)] -format "$Datepick::tformat($hours)"]
	$name tag add $tname $first $first+$Datepick::tformat_w($hours)chars-1char
	$name tag configure $tname -background $Datepick::bg(normal)
    }

    proc which {name {x ""} {y ""}} {

	# return a list consisting of the the name of the variable and
	# the tag corresponding
	# to the datepick subwidget at text position @x,y in
	# text widget "name"; use the insertion point if
	# x,y is empty

	if {"$x" != ""} {
	    set tags [$name tag names @$x,$y]
	} else {
	    set tags [$name tag names insert]
	}
	set ti [lsearch -glob $tags $Datepick::tag_prefix*]
	if {$ti >= 0} {
	    set tn [lindex $tags $ti]
	    return [list [string range $tn [string length $Datepick::tag_prefix ] end] $tn]
	} else {
	    return [list "" ""]
	}
    }

    proc reparse_date {name} {
	# reparse the date which the user may have edited
	# name is the name of the text widget
	# it's assumed the insertion point is within the
	# date to be reparsed
	# return 1 if the datepick subwidget handled
	# the keystroke, zero otherwise
	set which [Datepick::which $name]
	set tn [lindex $which 1]
	if {"$tn" != ""} {
	    set v [lindex $which 0]
	    set pos [$name index insert]
	    if {[catch {
		set tt [clock scan [$name get $tn.first $tn.last]]
		$name configure -background $Datepick::bg(normal)
		set ::$v $tt
	    }]} {
		$name tag configure $tn -background $Datepick::bg(error)
	    }
	    $name mark set insert $pos
	    return 1
	}
	return 0
    }

    proc cancel_date_change {name} {
	# the user may have edited a date; restore it to the
	# one stored in the backing variable
	# name is the name of the text widget
	# it's assumed the insertion point is within the
	# date to be reparsed
	# return 1 if the datepick subwidget handled
	# the keystroke, zero otherwise
	set which [Datepick::which $name]
	set tn [lindex $which 1]
	if {"$tn" != ""} {
	    set v [lindex $which 0]
	    set pos [$name index insert]
	    set ::$v [expr \$::$v]
	    $name mark set insert $pos
	    return 1
	}
	return 0
    }

    proc show_date_changed {key name} {
	# highlight the date pick object a user has begun editing
	# to mark it as changed
	if {"$key" != ""} {
	    set tn [lindex [Datepick::which $name] 1]
	    if {"$tn" != ""} {
		$name tag configure $tn -background $Datepick::bg(changed)
	    }
	}
    }

    proc datepick {name var {hours 12} {textopts ""} {callback ""}} {
	# create a datepicker text subwidget
	# name: pathname of a text window; if it does not exist, it is created
	#       if it does exist, the text subwidget is inserted at the current insertion point,
	#       which allows for multiple datepick subwidgets in a single text object

	if {"$textopts" == ""} {set textopts $Datepick::textopts}
	if {[winfo exists $name]} {
	    set loc [$name index insert]
	} else {
	    eval "text $name -width $Datepick::tformat_w($hours) -height 1 -wrap none $textopts"
	    set loc 1.0
	}

	trace add variable ::$var write "Datepick::setvar $name $var $hours"
	if {"$callback" != ""} {
	    trace add variable ::$var write "$callback"
	}
	set ::$var [expr \$::$var]
	bind $name <Button-4> "Datepick::change $name + %x %y; break"
	bind $name <Button-5> "Datepick::change $name - %x %y; break"
	bind $name <KeyPress-Up> "if {\[Datepick::change $name +]} {break}"
	bind $name <KeyPress-Down> "if {\[Datepick::change $name -]} {break}"
	bind $name <MouseWheel> "Datepick::change $name \[expr (%D > 0) ? \\\"+\\\" : \\\"-\\\"] %x %y; break"
	bind $name <KeyPress-Return> "if {\[Datepick::reparse_date $name]} {break}"
	bind $name <KeyRelease-Escape> "if {\[Datepick::cancel_date_change $name]} {break}"
	bind $name <KeyPress> "Datepick::show_date_changed %A $name"
	return $name
    }
    namespace export change which setvar reparse_date show_date_changed cancel_date_change datepick
}


# ## test: (Run by uncommenting the following lines and doing 'wish datepick.tcl')
#
# eval "text .a  $Datepick::textopts"
# .a insert 0.0 {this is a test}
# set x [clock seconds]
# set y [expr $x + 3600]
# Datepick::datepick .a x
# .a insert end {and more stuff}
# Datepick::datepick .a y 24
# pack .a
# ## after 10 seconds of editing, show the two timestamps
# after 10000 {puts "Got timestamps $::x  and  $::y\n"}
