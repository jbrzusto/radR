#  svn $Id$
#
#  radR : an R-based platform for acquisition and analysis of radar data
#  Copyright (C) 2006-2009 John Brzustowski        
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#########################################################################

# guicanvas.tcl: canvas functions for the radR GUI in Tcl/Tk

# args:
#   c      : canvas window pathname
#   t      : tagOrId expression identifying which items in the canvas to process
#   d      : list; amount to move canvas items; {x y}
#   org    : list: origin for scaling; {x y}
#   sf     : scale factor
#   a      : rotation angle (degrees clockwise)

# get an object's center i.e. (mean(x), mean(y))

proc get_center {c i} {
    set cc [$c coords $i]
    set n [llength $cc]
    set x 0
    set y 0
    for {set j 0} {$j < $n} {incr j} {
	set x [expr $x + [lindex $cc $j]]
	incr j
	set y [expr $y + [lindex $cc $j]]
    }
    return [list [expr $x / ($n/2)] [expr $y / ($n/2)]]
}  

# move items matching a given tag
    
proc canvas_pan {c t d} {
    set dx [lindex $d 0]
    set dy [lindex $d 1]
    if {[winfo exists $c]} {
	foreach i [$c find withtag $t] {
	    $c move $i $dx $dy
	}
    }
}

# scale items matching a given tag

proc canvas_zoom {c t org sf} {
    if {[winfo exists $c]} {
	set ox [lindex $org 0]
	set oy [lindex $org 1]
	foreach i [$c find withtag $t] {
	    $c scale $i $ox $oy $sf $sf
	}
    }
}

# move an item so that its centre has zoomed from the given
# origin by the given factor

proc canvas_zoom_center {c t org sf} {
    if {[winfo exists $c]} {
	set ox [lindex $org 0]
	set oy [lindex $org 1]
	foreach i [$c find withtag $t] {
	    set cen [get_center $c $i]
	    $c move $i [expr ([lindex $cen 0] - $ox) * ($sf - 1)] [expr ([lindex $cen 1] - $oy) * ($sf - 1)]
	}
    }
}

# show/hide arcs based on whether they are entirely beyond the screen boundary
# This is to work around an X server bug in drawing big arcs

proc canvas_showhide_big_arc {c t} {
    if {[winfo exists $c]} {
	set w [winfo width $c]
	set h [winfo width $c]
	foreach i [$c find withtag $t] {
	    set cc [$c coords $i]
	    set aw [expr ([lindex $cc 2] - [lindex $cc 0]) / 2]
	    set ah [expr ([lindex $cc 3] - [lindex $cc 1]) / 2]
	    set ax [expr ([lindex $cc 2] + [lindex $cc 0]) / 2]
	    set ay [expr ([lindex $cc 3] + [lindex $cc 1]) / 2]
	    # check that each corner of the screen is within the arc
	    if {[expr pow($ax/$aw, 2) + pow($ay/$ah, 2) < 1 && \
		      pow(($w - $ax)/$aw, 2) + pow($ay/$ah, 2) < 1 && \
		      pow($ax/$aw, 2) + pow(($h - $ay)/$ah, 2) < 1 && \
		 pow(($w - $ax)/$aw, 2) + pow(($h - $ay)/$ah, 2) < 1]} {
		$c itemconfigure $i -state hidden
	    } else {
		$c itemconfigure $i -state normal
	    }
	}
    }
}
	
# rotate an item's coordinates a given angle around a
# given origin  
# FIXME: if/when tk adopts a canvas "rotate" subcommand, use that here.

proc canvas_rotate {c t org a} {
    if {[winfo exists $c]} {
	set ox [lindex $org 0]
	set oy [lindex $org 1]
	set theta [expr $a * 3.1415926536 / 180]
	set st [expr - sin($theta)]
	set ct [expr cos($theta)]
	foreach i [$c find withtag $t] {
	    set cc [$c coords $i]
	    set n [llength $cc]
	    for {set j 0} {$j < $n} {incr j} {
		set x [expr [lindex $cc $j] - $ox]
		set y [expr [lindex $cc [expr 1 + $j]] - $oy]
		lset cc $j [expr $ox + $ct * $x - $st * $y]
		incr j
		lset cc $j [expr $oy + $ct * $y + $st * $x]
	    }
	    $c coords $i $cc
	}
    }
}

# move an item so that its centre has rotated a given angle 
# around the given origin

proc canvas_rotate_center {c t org a} {
    if {[winfo exists $c]} {
	set ox [lindex $org 0]
	set oy [lindex $org 1]
	set theta [expr $a * 3.1415926536 / 180]
	set st [expr - sin($theta)]
	set ct [expr cos($theta)]
	foreach i [$c find withtag $t] {
	    set cc [$c coords $i]
	    set cen [get_center $c $i]
	    set x [expr [lindex $cen 0] - $ox]
	    set y [expr [lindex $cen 1] - $oy]
	    set dx [expr $ct * $x - $st * $y - $x]
	    set dy [expr $ct * $y + $st * $x - $y]
	    $c move $i $dx $dy
	}
    }
}

## rotate an arc around its center
proc canvas_rotate_arc {c t a} {
    if {[winfo exists $c]} {
	foreach i [$c find withtag $t] {
	    if {[$c type $i] == "arc"} {
		$c itemconfigure $i -start [expr [$c itemcget $i -start] + $a]
	    }
	}
    }
}


# configure items matching a given tag according to a list
    
proc canvas_config {c t args} {
    if {[winfo exists $c]} {
	set n [llength $args]
	foreach i [$c find withtag $t] {
	    for {set j 0} {$j < $n} {incr j} {
		$c itemconfigure $i [lindex $args $j]  [lindex $args [incr j]]
	    }
	}
    }
}
