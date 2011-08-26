##  svn $Id: guicanvas.R 676 2010-11-04 13:13:14Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2009 John Brzustowski        
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.


## Canvas functions for the radR GUI in Tcl/Tk
##
## These perform transformations on canvas objects when GUI$plot.is.tk.image is
## TRUE.

gui.canvas.panned <- function(delta, tagOrId="pan", tagOrId2="zoom_arc") {

  ## The plot canvas has been panned by delta (x, y) pixels.  For all
  ## canvas elements having the "pan" tag, move them by delta.

  if (GUI$plot.is.tk.image && any(delta != 0.0)) {
    tcl("canvas_pan", GUI$plot, tagOrId, delta)
    if (GUI$range.rings.enabled)
      tcl("canvas_showhide_big_arc", GUI$plot, tagOrId2)
  }
}

gui.canvas.zoomed <- function(factor, origin, tagOrId="zoom", tagOrId2="zoom_center", tagOrId3="zoom_arc") {
  
  ## The plot canvas has been zoomed by factor relative to origin.  For all
  ## canvas elements having the "zoom" tag, scale them by delta.
         
  if (GUI$plot.is.tk.image && factor != 1.0) {
    if (!is.null(tagOrId))
      tcl("canvas_zoom", GUI$plot, tagOrId, GUI$plot.origin, factor)
    if (!is.null(tagOrId2))
      tcl("canvas_zoom_center", GUI$plot, tagOrId2, GUI$plot.origin, factor)
    if (!is.null(tagOrId2) && GUI$range.rings.enabled)
      tcl("canvas_showhide_big_arc", GUI$plot, tagOrId3)
  }
}

gui.canvas.rotated <- function(theta, origin, tagOrId="rotate", tagOrId2="rotate_center", tagOrId3="rotate_arc", tagOrId4="zoom_arc") {

  ## The plot canvas has been rotated by theta degrees clockwise
  ## around origin.  For all canvas elements having the "rotate" tag,
  ## rotate them by theta. 

  if (GUI$plot.is.tk.image && theta != 0.0) {
    tcl("canvas_rotate", GUI$plot, tagOrId, GUI$plot.origin, -theta)
    tcl("canvas_rotate_center", GUI$plot, tagOrId2, GUI$plot.origin, -theta)
    tcl("canvas_rotate_arc", GUI$plot, tagOrId3, -theta)
    if (GUI$range.rings.enabled)
      tcl("canvas_showhide_big_arc", GUI$plot, tagOrId4)
  }
}
