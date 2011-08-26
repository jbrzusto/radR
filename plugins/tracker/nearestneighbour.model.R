##  svn $Id: nearestneighbour.model.R 246 2009-02-27 19:28:22Z john $
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
##
##   Constrained Nearest Neighbour Tracker Model
##
## An ad-hoc model which considers all track/point extensions,
## eliminates those not satisfying filtering criteria such as maximum
## speed, turning rate, etc., and then uses negative distance as the
## gain function for a maximum matching.  Points and tracks and tracks
## have time-to-live components: if they are not added to within a
## certain amount of time, they are discarded or finished,
## respectively.
##
## This is a "model" component for the radR tracker plugin.
##
## It provides these functions:
##
## update(TR, points)
##     update tracks with the given points, starting new tracks where appropriate
##
## end.all.tracks()
##     do anything required when forcing all tracks to end
##
## get.menu()
##     returns a list defining a menu for parameters specific to this model
##
## select()
##     do anything required when this model is chosen
##
## deselect()
##     do anything required when this model is unchosen
##
## load()
##     do anything required at model load time
##
## unload()
##     do anything required at model unload time
##
## the parameter TR is the TRACKER plugin object.  See tracker.plugin.R
##

desc = "nearest similar neighbour model"

update = function(TR, blips, i.blips, time.now, is.preview) {

  ## add blips to tracks; the blips being added are blips,
  ## and their absolute indexes are i.blips
  ## blips are matched to the last blips in all tracks;
  ## blips are appended to the closest track (if any) such that the
  ## maximum speed, turning angle rate are not exceeded.
  ## Unmatched blips begin new tracks

  if(length(i.blips) > 0) {
    actual.tracks <- .Call("which_slots_full", TR$tracks)
###.if $DEBUG    
    gui.print.cons("Have " %:% length(actual.tracks) %:% " active tracks")
###.endif    
    num.tracks <- length(actual.tracks)
    num.blips <- dim(blips)[1]
    if (num.tracks > 0) {

      ## compute distances between all pairs of new blips and last blips on active tracks
      
      old.pts <- TR$all.blips[int.lapply(TR$tracks[actual.tracks], function(tr) tail(tr$points, 1)), ]
      dists <- rss.dist.3d(blips$x, blips$y, blips$z, old.pts$x, old.pts$y, old.pts$z)
      time.diffs <- outer(unclass(blips$t), unclass(old.pts$t), `-`)

      ## create a gain matrix, reversing the ordering of distances
      ## and ensuring that every distance provides a positive gain

      gain <- 100 + max(dists) - dists
      
      ## for distances implying excessive speeds, mark the gain as 0
      gain[abs(dists / time.diffs) > track.max.speed / 3.6] <- as.integer(0)

      ## mark zero gain where relative change rates in blip area or intensity
      ## are too high

      area.ratio <- outer(blips$area, old.pts$area, `/`)
      int.ratio  <- outer(blips$int,  old.pts$int,  `/`)

      ## notice the alternating use of "*" and "/" for time.diffs
      gain[ area.ratio / time.diffs >      (1 + max.area.change.rate / 100) |
           area.ratio * time.diffs <  1 / (1 + max.area.change.rate / 100) |
           int.ratio  / time.diffs >      (1 + max.int.change.rate  / 100) |
           int.ratio  * time.diffs <  1 / (1 + max.int.change.rate  / 100)  ] <- as.integer(0)

      ## For (blip,track) pairs which would cause the max turning angle rate to be exceeded,
      ## mark their gain as zero

      for (i in seq(along=actual.tracks)) {
        tr <- TR$tracks[[actual.tracks[i]]]
        if (length(tr$points) >= 2) {
          pts <- TR$all.blips[tail(tr$points, 2),]
          cos.ang <- rss.cos.angle.3d(pts$x[2] - pts$x[1], pts$y[2] - pts$y[1], pts$z[2] - pts$z[1],
                                      blips$x  - pts$x[2], blips$y  - pts$y[2], blips$z  - pts$z[2])
          gain[is.finite(cos.ang) & cos.ang < cos(max.turn.rate * pi / 180 * time.diffs[, i]), i] <- as.integer(0)
        }
      }
      if (num.blips > num.tracks) {
        ## each track can get a best blip
        which.blip <- .Call("bipartite_matching", t(gain))
        tmp <- which(!is.na(which.blip))
        which.track <- rep(NA, num.blips)
        which.track[which.blip[tmp]] <- tmp
      } else {         
        ## each blip can get a best track
        which.track <- .Call("bipartite_matching", gain)
      }  
    } else {
      ## no existing tracks; force each blip to begin one
      which.track <- rep(NA, length(i.blips)[1])
    }
    
    ## for each new blip, either add it to the matched
    ## track, or start a new track with it
    for (i in seq(along=which.track))
      if (!is.na(which.track[i])) {
###.if $DEBUG
        gui.print.cons("About to call add.blip.to.track")
###.endif
        TR$add.blip.to.track(i.blips[i], actual.tracks[which.track[i]], expiry=all.blips[i.blips[i], COL.T] + track.stale.time) 
      } else {
###.if $DEBUG
        gui.print.cons("About to call start.new.track")
###.endif
        TR$start.new.track(i.blips[i], expiry = all.blips[i.blips[i], COL.T] + track.stale.time)
      }
###.if $DEBUG
    gui.print.cons("Done update")
###.endif
  }
}

end.all.tracks = function(TR) {
  ## no need to do anything here
}


set.max.speed = function(x) {
  ## no need to do anything here
}

get.menu = function() {
  list(
       "nearest_neighbour" = "menu",
       "options" = "no-tearoff",
       list ("gauge",
             label = "how long a lone blip is retained as a possible track starter (seconds)",
             range = c(1, 3600),
             increment = 1,
             value = blip.fresh.time,
             on.set = set.blip.fresh.time
             ),
       list ("gauge",
             label = "how long a track stays active after its last blip (seconds)",
             range = c(1, 3600),
             increment = 1,
             value = track.stale.time,
             on.set = set.track.stale.time
             ),
       list ("gauge",
             label = "maximum turning rate (degrees per second)",
             range = c(0, 180),
             increment = .5,
             value = max.turn.rate,
             on.set = function(x) { max.turn.rate <<- x; reprocess.scan() }
             ),
       list ("gauge",
             label = "maximum consecutive blip area change rate (percent per second)",
             range = c(0, 300),
             increment = 1,
             value = max.area.change.rate,
             on.set = function(x) { max.area.change.rate <<- x; reprocess.scan() }
             ),
       list ("gauge",
             label = "maximum consecutive blip intensity change rate (percent per second)",
             range = c(0, 300),
             increment = 1,
             value = max.int.change.rate,
             on.set = function(x) { max.int.change.rate <<- x; reprocess.scan() }
             )
       )
}

select = function() {
  auto.expire.tracks <<- TRUE
}

deselect = function() {
}

load = function() {           
  rss.dyn.load("maxmatch", in.dir.of=plugin.file)
}

unload = function(save.config) {
  rss.dyn.unload("maxmatch")
}

set.blip.fresh.time = function(x) {
  ## set a new value for the length of time a blip
  ## is retained as a possible seed for a track
  if (x == blip.fresh.time)
    return()
  if (RSS$previewing) {
    ## for saved tracks, bump their expiry time up or down,
    ## then reprocess the scan
    for (tr in saved.state$tracks)
      if (!is.null(tr) && tr$state == TS.NASCENT)
        tr$expiry <- tr$expiry + (x - blip.fresh.time)
    blip.fresh.time <<- x
    reprocess.scan()
  } else {
    ## for current tracks, bump their expiry time up or down
    for (tr in tracks)
      if (!is.null(tr) && tr$state == TS.NASCENT)
        tr$expiry <- tr$expiry + (x - blip.fresh.time)
    blip.fresh.time <<- x
  }
}

set.track.stale.time = function(x) {
  ## set a new value for the length of time a track stays active
  ## after its last blip
  if (x == track.stale.time)
    return()
  if (RSS$previewing) {
    ## for saved tracks, bump their expiry time up or down,
    ## then reprocess the scan
    for (tr in saved.state$tracks)
      if (!is.null(tr) && tr$state == TS.ACTIVE)
        tr$expiry <- tr$expiry + (x - track.stale.time)
    track.stale.time <<- x
    reprocess.scan()
  } else {
    ## for current tracks, bump their expiry time up or down
    for (tr in tracks)
      if (!is.null(tr) && tr$state == TS.ACTIVE)
        tr$expiry <- tr$expiry + (x - track.stale.time)
    track.stale.time <<- x
  }
}

gain.from.track.to.point = function(...) {
  ## a stub to return a constant gain
  ## Because of the way gain was defined above, it is a real pain
  ## to reconstruct it later.
  0.5
}
