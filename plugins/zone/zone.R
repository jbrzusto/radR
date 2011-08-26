##  svn $Id: zone.R 264 2009-03-04 17:01:47Z john $
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
##

## zone.R - GUI editor for creating a zone in the radar data, namely a
##          collection of sectors or pie slices whose centre is the radar
##          location.

## Design:
##   A zone is an environment with these items:
##   .id:       a tag carried by each Tk object belonging to this zone
##   par:      a list of appearance parameters with these items:
##      point:
##      arc:
##      line:  each is a pair of strings with parameters for line thickness, and style
##             the first for unhighlighted, the second for highlighted items
##             e.g. c("-dash {}", "-outline white -dash {.}")
##             Colour should not be specified for the unhighlighted item, as it is part of the zone definition.
##
##      radius:   the radius of control point circles, in pixels
##   origin:  the x, y planar spatial coordinates of the zone's centre, relative to the radar
##               (this is provided for when we start using multiple radars)
##   enabled: whether this zone is going to be used.

##   The zone's area consists of a union of segments given by these items:

##   r[[1]], r[[2]]: the radii of point pairs, in metres; so r[[1]][3] is the radius of point 1 of segment 3
##   a[[1]], a[[2]]: the angles of the first point, in degrees clockwise from North; and the angular extent,
##                   going clockwise.

## During editing, additional items are added to the zone:
##   
##   .hilite: boolean: for each segment, is it currently highlighted?
##   .to.redraw: boolean: for each segment, must it be redrawn?
##   .recreate:  boolean: should tk canvas items be recreated
##   .tag: character: tk canvas tag used for all items related to this zone
##   .delproc: function to call to delete a zone
##   .reproc: function to call when reprocessing a scan (to reflect changed zone geometry etc.)

##   The tk items corresponding to a zone are as so:
##   each point is represented by a circle;
##   each zone is drawn as a set of arcs and straight lines
##   The canvas items have these tags:

##   points: TAG, z_sN, z_p, z_p[1-4] where N is in 1:length(r1)

##   arcs:   TAG, z_sN, (z_a1 or z_a2) where N is in 1:length(r1)

##   lines:  TAG, z_sNn, (z_l1 or z_l2) where N is in 1:length(r1)

##   (here, TAG is zone$.tag)

## Bindings:

##   points, lines, arcs:   drag:  record new coordinates in R object and redraw
##   points, lines, arcs: button 2: menu: 
## 	   		- extend to range 0
##                      - extend to range max
##                      - create new segment
##                      - clone segment
##                      - set range max here
##                      - set range min here
## 			- delete segment (quits if last segment)
## 			- save and quit
## 			- cancel and quit
##                      - delete zone and quit

## In bound methods, the item's type comes from the TAG.[alp][12] tag,
## and its segment index comes from the TAG.n tag.

## We use one method for each event and bind it to all tags:

## Button-1-Down:
##    - record the current mouse position and item tag
##    - append the Motion handler below

## Button-1-Up: 
##    - restore the original Motion handler
	  
## Button-3-Down:
##    - popup the context menu

## Motion:
##    - reset item's coordinates according to the change between
##      the current and old coordinates

show.zone = function(canvas, zone, geom) {
  ##
  ## show a zone on a canvas
  ##
  ## canvas: the Tk pathname of the canvas
  ## zone: a list with zone items, as described above
  ## geom: a function returning a list describing the canvas geometry
  ##
  ## Returns: NULL

  ## This works by adding items to the canvas corresponding to
  ## the zone, and adding editing bindings to them.

  zone$.tag <- "z_" %:% zone$.id
  zone$.geom <- geom
  zone$visible <- TRUE
  draw.zone (canvas, zone)
  bind.zone (canvas, zone)
}

hide.zone = function(canvas, zone) {
  ## just destroy the zone's tk canvas items
  zone$visible <- FALSE
  .Tcl(canvas % % "delete" % % zone$.tag)
}

draw.zone = function(canvas, zone) {
  ##
  ## draw a zone on a canvas
  ##
  ## angles in radians
  a <- list ((90 - (zone$a[[1]] + zone$a[[2]]) - zone$.geom()$north.angle) * (pi / 180))
  a[[2]] <- a[[1]] + zone$a[[2]] * (pi / 180)
             

  org <- c(gui.xy.to.plot.coords(t(zone$.origin)))
  ## control point coordinates
  ## notice that the screen y-axis increases downward
  p <- list()
  for (j in 1:2)
    p[[j]] <- org + t(zone$r[[j]] / zone$.geom()$mpp * cbind(cos(a[[1]]), - sin(a[[1]])))
  
  ## complementary point coordinates
  for (j in 1:2)
    p[[2 + j]] <- org + t(zone$r[[j]] / zone$.geom()$mpp * cbind(cos(a[[2]]), - sin(a[[2]])))

  line.tags <- c("zone", "zoom", "pan", "rotate")
  arc.tags <- c("zone", "zoom", "pan", "rotate_center", "rotate_arc")
  point.tags <- c("zone", "zoom_center", "pan", "rotate_center")

  if (isTRUE(zone$.recreate)) {
    tcl(canvas, "delete", zone$.tag)
    zone$.recreate <- FALSE
  }
  
  if (!length(tclint(canvas, "find", "withtag", zone$.tag))) {
    ## the tk items don't exist; create them with bogus coordinates
    for (i in seq(along=zone$r[[1]])) {
      ## draw lines
      for (j in 1:2) {
        id <- tcl(canvas, "create", "line", c(0, 0, 0, 0),
                  tags = c(line.tags, zone$.tag, "z_s" %:% i, "z_l" %:% j))
      }
      
      ## draw arcs
      for (j in 1:2) {
        id <- tcl(canvas, "create", "arc", c(0, 0, 0, 0),
                  start = 0,
                  extent = 0,
                  style = "arc",
                  tags = c(arc.tags, zone$.tag, "z_s" %:% i, "z_a" %:% j))
      }

      ## draw control points (do these last so they have click priority)
      for (j in 1:4) {
        id <- tcl(canvas, "create", "oval", c(0, 0, 0, 0),
                  tags = c(point.tags, zone$.tag, "z_s" %:% i, "z_p", "z_p" %:% j))
      }

    }
  }

  to.redraw <- zone$.to.redraw
  if (is.null(to.redraw))
    to.redraw <- seq(along=zone$r[[1]])

  ## set the control points as visible or not, depending on whether we're editing the zone
  
  .Tcl(canvas % % "itemconfigure" % % zone$.tag %:% "&&z_p" % % "-state" % %  if (isTRUE(zone$.edit)) "normal" else "hidden")
  
  ## Now set the coordinates correctly; we do this on initial draw and on redraws
  for (i in to.redraw) {
    ## set control point coords and style
    for (j in 1:4) {
      tagexp <- paste(zone$.tag, "z_s" %:% i, "z_p" %:% j, sep="&&")
      tcl(canvas, "coords", tagexp,
          c(p[[j]][,i] - zone$.par$radius, p[[j]][,i] + zone$.par$radius))
      .Tcl(paste(canvas, "itemconfigure", tagexp,"-outline", zone$colour, zone$.par$point[1 + zone$.hilite[i]]))
    }

    ## set line coords and style
    for (j in 1:2) {
      tagexp <- paste(zone$.tag, "z_s" %:% i, "z_l" %:% j, sep="&&")
      tcl(canvas, "coords", tagexp,
          c(p[[2 * j - 1]][,i], p[[2 * j]][,i]))
      .Tcl(paste(canvas, "itemconfigure", tagexp,"-fill", zone$colour, zone$.par$line[1 + zone$.hilite[i]]))
    }
    
    ## set arc coords and style
    for (j in 1:2) {
      tagexp <- paste(zone$.tag, "z_s" %:% i, "z_a" %:% j, sep="&&")
      tcl(canvas, "coords", tagexp,
          c(org - zone$r[[j]][i] / zone$.geom()$mpp, org + zone$r[[j]][i] / zone$.geom()$mpp))
      .Tcl(paste(canvas, "itemconfigure", tagexp,
                 "-start", 90 - (zone$a[[1]][i] + zone$a[[2]][i]) - zone$.geom()$north.angle,
                 "-extent", if (zone$a[[2]][i] == 360.0) 359.999 else zone$a[[2]][i],
                 "-outline", zone$colour, zone$.par$arc[1 + zone$.hilite[i]]))
    }
  }
  zone$.to.redraw <- NULL
}

destroy.zone = function(canvas, zone) {
  unbind.zone(canvas, zone)
  .Tcl(canvas % % "delete" % % zone$.tag)
  zone$.delproc(zone)
}

destroy.all.zones = function(canvas) {
  .Tcl(canvas % % "delete zone")
}

hilite.zone = function(canvas, zone, on=TRUE, which=seq(along=zone$r[[1]])) {
  zone$.hilite[which] <- on
  draw.zone(canvas, zone)
}  

item.info.zone = function(canvas, zone) {
  ## get the segment id and type of the currently active zone element
  ## and return these as a list like:  list(segno=10, type="a", index=1)
  ## or NULL if there is no zone element active
  id <- tclint(canvas, "find", "withtag", "current")
  if (!length(id))
    return(NULL)

  ## get the "z_sn" and "z_OBJ" tags, which give the sector index and object type
  tags <- tclchar(canvas, "gettags", id)
  if (!any(tags == zone$.tag))
    return(NULL)
  segno <- as.integer(substring(grep("^z_s[0-9]", tags, perl=TRUE, value=TRUE), 4))
  tags <- grep("^z_[alp][1-9]$", tags, perl=TRUE, value=TRUE)
  index <- as.integer(substring(tags, 4))
  type <- substring(tags, 3, 3)
  return (list (segno=segno, type=type, index=index))
}

xy.to.zone.coords = function(zone, xy) {
  ## convert canvas coordinates to zone coordinates
  ## zone: the zone
  ## xy: 2 x n matrix of canvas coordinates

  ## Returns: 2 x n matrix consisting of rbind(r, a) where r is in metres and a is degrees clockwise from North

  ## Uses the current GUI plot window

  rp <- xy - GUI$plot.origin
  rbind(sqrt(apply(rp^2, 2, sum)) * GUI$mpp,
    (90 - atan2(-rp[2,], rp[1,]) * 180 / pi - GUI$north.angle) %% 360)
}

zone.coords.to.xy = function(zone, ra) {
  ## convert zone coordinates to canvas coordinates
  ## (the inverse of xy.to.zone.coords)
  ## zone: the zone
  ## ra: 2 x n matrix of vectors of coordinates where r is in metres and a is degrees clockwise from North

  ## Returns: 2 x n matrix consisting of rbind(x, y) which are canvas coordinates

  ## Uses the current GUI plot window

  th <- (90 - ra[, 2] - GUI$north.angle) * (pi / 180)
  r <- r / GUI$mpp
  GUI$plot.origin + rbind(r * cos(th), r * sin(th))
}
  
  
drag.zone = function(pc, ...) {
  if (is.null(info <- item.info.zone(canvas, zone)))
    return(NULL)
  if (isTRUE(zone$.edit)) {
    org <- c(gui.xy.to.plot.coords(t(zone$.origin)))
    relpos <- pc - org
    relstart <- zone$.start.pos - org


    if (isTRUE(zone$.dragging)) {
      if (! gui.state.has.key(GUI$last.motion.state, "Alt")) {
        ## motion without the Alt key being held down - only the dragged item "moves", changing the shape
        
        if (info$type == "p" || info$type == "a")
          ## for points and arcs, we adjust the radius
          zone$r[[c(1, 2, 1, 2)[info$index]]][info$segno] <- sqrt(sum(relpos^2)) * zone$.geom()$mpp
        
        if (info$type == "p" || info$type == "l") {
          ## for points and lines, we adjust the angle
          new.angle <- (90 - atan2(-relpos[2], relpos[1]) * 180 / pi - zone$.geom()$north.angle) %% 360
          if (info$index == 1 || (info$type == "p" && info$index == 2)) {
            zone$a[[2]][info$segno] <- round.angular.extent(new.angle - zone$a[[1]][info$segno])
          } else {
            zone$a[[2]][info$segno] <- round.angular.extent(zone$a[[2]][info$segno] + (zone$a[[1]][info$segno] - new.angle))
            zone$a[[1]][info$segno] <- new.angle
          }
        }
      } else {
        ## motion with a key held down - this is a "rigid" motion in that the element parallel to
        ## the dragged one moves in tandem with it, preserving angular and radial span.
        
        if (info$type == "p" || info$type == "a") {
          ## for points and arcs, we adjust the radii in parallel (i.e. rigid radial motion)
          dr <- (sqrt(sum(relpos^2)) - sqrt(sum(relstart^2))) * zone$.geom()$mpp
          zone$r[[1]][info$segno] <- max(0, zone$.start.r[[1]][info$segno] + dr)
          zone$r[[2]][info$segno] <- max(0, zone$.start.r[[2]][info$segno] + dr)
        }
        
        if (info$type == "p" || info$type == "l") {
          ## for points and lines, we adjust the angles in parallel (i.e. rigid circumferential motion)
          da <- atan2(-relpos[2], relpos[1]) * 180 / pi - atan2(-relstart[2], relstart[1]) * 180 / pi
          zone$a[[1]][info$segno] <- (zone$.start.a[[1]][info$segno] - da) %% 360
        }
      }
      zone$.to.redraw <- info$segno
      draw.zone(canvas, zone)

    }
  }
  ## Display the segment's coordinates.

  return(c(sprintf("Zone edge: %s  Segment: r=(%.0f, %.0f) m, ",
                       zone$.id,
                       zone$r[[1]][info$segno],
                       zone$r[[2]][info$segno]) %:%
           if (zone$a[[2]][info$segno] == 360.0) {
             "full circle"
           } else {
             sprintf("a=(%.2f, %.2f) deg",
                     zone$a[[1]][info$segno],
                     (zone$a[[1]][info$segno]+zone$a[[2]][info$segno]) %% 360
                     )
           },
           ""
           )
         )
}

start.drag.zone = function(x, y) {
  if (!isTRUE(zone$.edit))
    return()
  pos <- as.integer(c(x, y))
  zone$.start.pos <- pos
  zone$.start.a <- zone$a
  zone$.start.r <- zone$r
  zone$.dragging <- TRUE
  hilite.zone(canvas, zone, TRUE, item.info.zone(canvas, zone)$segno)
}

end.drag.zone = function(x, y) {
  if (!isTRUE(zone$.edit))
    return()
  zone$.dragging <- FALSE
  zone$.start.pos <- NULL
  zone$.start.a <- NULL
  zone$.start.r <- NULL
  hilite.zone(canvas, zone, FALSE, item.info.zone(canvas, zone)$segno)
  zone$.reproc()
}

quit.zone = function(canvas, zone, cancel=FALSE) {
  ## call the at.end handler for zone, with a flag indicating whether we're cancelling the edit
  tcl(canvas, "delete", zone$.tag)
  if (!is.null(f <- zone$.at.end)) {
    p <- zone$.at.end.parms
    rm(list=c("at.end", "at.end.parms", "hilite", "recreate", "to.redraw"), envir=zone)
    do.call(f, c(list(zone=zone, cancel=cancel), p))
  }
}

handle.context.menu.zone = function() {
  ## handler for the zone context menu; its environment contains canvas, zone, what, segno

  relpos <- pos - c(gui.xy.to.plot.coords(t(zone$.origin)))
  imin <- 1 + (zone$r[[2]][segno] < zone$r[[1]][segno])
  imax <- 3 - imin
  rhere <- sqrt(sum(relpos^2)) * zone$.geom()$mpp
  ahere <- (90 - atan2(-relpos[2], relpos[1]) * 180 / pi - zone$.geom()$north.angle) %% 360
  keep.hilite <- FALSE
  reprocess <- FALSE
  switch(what,
         new = {
           zone$r[[1]] <- c(zone$r[[1]], rhere)
           zone$r[[2]] <- c(zone$r[[2]], rhere + 50 * zone$.geom()$mpp)
           zone$a[[1]] <- c(zone$a[[1]], ahere)
           zone$a[[2]] <- c(zone$a[[2]], 30)
           zone$.recreate <- TRUE
           zone$.hilite <- c(FALSE & zone$.hilite, TRUE)
           keep.hilite <- TRUE
           reprocess <- TRUE
         },
         
         to.zero = {
           zone$r[[imin]][segno] <- 0.0
           reprocess <- TRUE
         },
         
         to.max = {
           ## set max range to 120km or outer limit of current data
           zone$r[[imax]][segno] <- if (is.null(RSS$scan.info$sample.dist)) 120000 else with(RSS$scan.info, first.sample.dist + sample.dist * samples.per.pulse)
           reprocess <- TRUE
         },
         
         min.here = {
           zone$r[[imin]][segno] <- rhere
           reprocess <- TRUE
         },
         
         max.here = {
           zone$r[[imax]][segno] <- rhere
           reprocess <- TRUE
         },

         ring = {
           zone$a[[2]][segno] <- 360.0
           reprocess <- TRUE
         },
         
         clone = {
           which <- c(seq(along=zone$r[[1]]), segno)
           for (j in 1:2) {
             zone$r[[j]] <- zone$r[[j]][which]
             zone$a[[j]] <- zone$a[[j]][which]
           }
           zone$.recreate <- TRUE
           zone$.hilite <- c(FALSE & zone$.hilite, TRUE)
           keep.hilite <- TRUE
           reprocess <- TRUE
         },
         
         revert = {
           zone$r <- zone$.saved.r
           zone$a <- zone$.saved.a
           zone$.recreate <- TRUE
           reprocess <- TRUE
         },
         
         edit = {
           zone$.saved.r <- zone$r
           zone$.saved.a <- zone$a
           zone$.edit <- TRUE
         },
         
         hide = {
           hide.zone(canvas, zone)
           return()
         },
         
         done = {
           zone$.edit <- FALSE
         },

         colour = {
           col <- tclchar("tk_chooseColor", initialcolor = zone$colour,
                          title="Choose colour for " %:% zone$.id %:% " zone")
           if (length(col) > 0) {
             zone$colour <- col
             zone$.recreate <- TRUE
           } else {
             return()
           }
         },

         del.seg = {
           for (j in 1:2) {
             zone$r[[j]] <- zone$r[[j]][-segno]
             zone$a[[j]] <- zone$a[[j]][-segno]
           }
           zone$.recreate <- TRUE
           if (length(zone$r[[1]]) == 0) {
             destroy.zone(canvas, zone)
             return()
           }
           reprocess <- TRUE
         },
         
         del.all = {
           destroy.zone(canvas, zone)
           return()
         },
         
         nil = {
         }  # do nothing but delete the menu
         )
  if (!keep.hilite)
    zone$.hilite[] <- FALSE
  draw.zone(canvas, zone)
  if (reprocess)
    zone$.reproc()
}
           
  
show.context.menu.zone = function(X, Y, x, y) {
  ## show a context menu for the zone
  ## The menu differes depending on whether zone$.edit is TRUE
  
  if (is.null(info <- item.info.zone(canvas, zone)))
    return()
  hilite.zone(canvas, zone, TRUE, which=if (isTRUE(zone$.edit)) info$segno else rep(TRUE, length(zone$r[[1]])))
  e <- c(list(zone=zone, canvas=canvas, pos=as.integer(c(x,y))), info)
  menu.list <- list(" (close menu)" = rss.make.closure(handle.context.menu.zone, c(e, what="nil"), parent=ZONE),
                    "Zone: " %:% zone$.id
                    )
  
  if (isTRUE(zone$.edit))
    menu.list <- c( menu.list,
                         "New segment"     = rss.make.closure(handle.context.menu.zone, c(e, what="new"), parent=ZONE),
                         "Range -> 0"      = rss.make.closure(handle.context.menu.zone, c(e, what="to.zero"), parent=ZONE),
                         "Range -> max"    = rss.make.closure(handle.context.menu.zone, c(e, what="to.max"), parent=ZONE),
                         "Max range here"  = rss.make.closure(handle.context.menu.zone, c(e, what="max.here"), parent=ZONE),
                         "Min range here"  = rss.make.closure(handle.context.menu.zone, c(e, what="min.here"), parent=ZONE),
                         "Full circle"     = rss.make.closure(handle.context.menu.zone, c(e, what="ring"), parent=ZONE),
                         "Clone segment"   = rss.make.closure(handle.context.menu.zone, c(e, what="clone"), parent=ZONE),
                         "---",
                         "Undo changes"    = rss.make.closure(handle.context.menu.zone, c(e, what="revert"), parent=ZONE),
                         "Finish edit"     = rss.make.closure(handle.context.menu.zone, c(e, what="done"), parent=ZONE),
                         "---",
                         "Delete this segment" = rss.make.closure(handle.context.menu.zone, c(e, what="del.seg"), parent=ZONE)
                   )
    else
      menu.list <- c(menu.list,
                     "Edit zone                            " = rss.make.closure(handle.context.menu.zone, c(e, what="edit"), parent=ZONE),
                     "---",
                     "Hide zone" = rss.make.closure(handle.context.menu.zone, c(e, what="hide"), parent=ZONE),
                     "---"
                     )
  
  menu.list <- c(menu.list,
                 list ("Delete entire zone" = rss.make.closure(handle.context.menu.zone, c(e, what="del.all"), parent=ZONE),
                       "---",
                       "Change colour"      = rss.make.closure(handle.context.menu.zone, c(e, what="colour"), parent=ZONE),
                       list("choose.any",
                            on.set = function(n, v) {
                              zone$.hilite[] <- FALSE
                              draw.zone(canvas, zone)
                              enable.zones(zone$.id, v)
                            },
                            "zone enabled" = zone$enabled
                            )
                       )
                 )
                     
  tcl(gui.menu.from.list(".zone_edit", menu.list, zone$.id % % "zone"), "post", X, Y)
}

bind.zone = function(canvas, zone) {
  ## set up edit bindings
  ## for the zone on the canvas

  tcl(canvas, "bind", zone$.tag, "<Button-1>",         rss.make.closure(start.drag.zone, environment()))
  tcl(canvas, "bind", zone$.tag, "<B1-ButtonRelease>", rss.make.closure(end.drag.zone, environment()))
  tcl(canvas, "bind", zone$.tag, "<Button-3>",         rss.make.closure(show.context.menu.zone, environment()))

  ## use read.only=FALSE to cause this hook to run last
  rss.add.hook("PLOT_CURSOR_MOVED", "zone" %:% zone$.tag, rss.make.closure(drag.zone, environment()), read.only=FALSE)
}

unbind.zone = function(canvas, zone) {
  rss.drop.hook("PLOT_CURSOR_MOVED", "zone" %:% zone$.tag)
  tcl(canvas, "bind", zone$.tag, "<Button-1>", "")
  tcl(canvas, "bind", zone$.tag, "<B1-ButtonRelease>", "")
  tcl(canvas, "bind", zone$.tag, "<Button-3>", "")
}

as.data.frame.zone <- function(x, ...) {
  data.frame(r1 = x$r1, r2 = x$r2, a1 = x$a1, a2 = x$a2)
}

round.angular.extent <- function(x) {
  if (x >= 0.0 && x <= 360.0)
    x
  else
    x %% 360
}
