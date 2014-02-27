##  svn $Id: multiframecorr.model.R 574 2010-05-11 02:07:15Z john $
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
##   Multi Frame Correspondence Tracker Model
##
## This is a "model" component for the radR tracker plugin.
## The algorithm is from:
##
##     K. Shafique and M. Shah (2005).  "A Noniterative Greedy Algorithm
##     for Multiframe Point Correspondence".  IEEE Transactions on Pattern
##     Analysis and Machine Intelligence, Vol. 27(1), pp. 51 - 65.
##
## This implementation omits the backtracking phase at frame k, and uses
## non-recursive false hypothesis replacement.
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
## load()
##     do anything required at model load time
##
## unload()
##     do anything required at model unload time
##
## select()
##     do anything required when this model is chosen
##
## deselect()
##     do anything required when this model is unchosen
##
## the parameter TR is the TRACKER plugin object.  See tracker.plugin.R
##
## In keeping with the notation in the reference, we call a scan a "frame".
## Track objects are only begun once k+d-2 scans have been processed, where k is the
## number of frames used in the MFC model, and d is the number of points along
## a track required to compute the gain function.  In the case where gain depends
## only on position and velocity, d = 2.
##
## IMPORTANT VARIABLES
##
## first.blip
##     a vector of length <= k, giving the index in TR$all.blips of the first blip in each frame
##
## num.blips
##     a vector of length <= k, giving the number of blips in each frame
##
## TR$tracks[[i]]$info
##     this is a list of state variables associated with the last point in the track
##     e.g. its velocity, acceleration.  Knowing the state at a point and the location
##     of its successor point, the state at the successor point can be calculated.
##
## succ
##     a vector of length sum(num.blips)
##     If succ[i] != NA, then point i (numbered from 1=first point
##     in first frame) is joined (as part of a track) to the point
##     whose index in TR$all.blips is succ[i]; otherwise, point i
##     has no forward correspondent in a track. i.e. it is the last
##     point of a track (possibly because it is in frame k-1), or
##     not in a track
##
## gain
##     a vector of length sum(num.blips)
##     Non-negative gain values corresponding to the edges represented by "succ".
##
## pred.atid
##     a vector of length num.blips[1].  For each blip in the first frame, this is either NA or
##     the active track ID (atid) of the track whose last point is the blip.
##

desc = "Multi-frame correspondence (Shafique & Shah, 2005)"

update = function(TR, blips, i.blips, time.now, is.preview) {
  ## The maximum possible distance between two detected points depends on the beam cone shape.
  ## For antenna.angle >= 60 degrees, the maximum distance between two detectable points is just the maximum range.
  ## Otherwise, it is the distance across the beam circle.
  
  max.dist <<- with(RSS$scan.info,
                    if (antenna.angle[1] >= 60)
                      sample.dist * samples.per.pulse + first.sample.dist
                    else
                      2 * (sample.dist * samples.per.pulse + first.sample.dist) * cos(antenna.angle[1] * pi / 180)
                    )
  new.mfcp <- FALSE
  if (is.null(mfcp)) {
    mfcp <<- .Call("init", k, alpha, eps, max.dist, track.max.speed / 3.6, min.gain, use.gain.pp, use.gain.tp, NULL, PACKAGE="multiframecorr")
    new.mfcp <- TRUE
  } else {
    ## in case radar range or max speed has changed since last scan
    .Call("set_max_dist", mfcp, max.dist, PACKAGE="multiframecorr")
  }
  .Call("update", mfcp, environment(), blips$x, blips$y, blips$z, blips$t, i.blips, time.now, is.preview, PACKAGE="multiframecorr")
  if (is.preview && new.mfcp) {
    .Call("destroy", mfcp, PACKAGE="multiframecorr")
    mfcp <<- NULL
  }
}

end.all.tracks = function(TR) {
  ## tell the C code to end all tracks
  .Call("end_all_tracks", mfcp, environment(), PACKAGE="multiframecorr")
  ## delete this instance of the mfcp problem
  if (!is.null(mfcp)) {
    .Call("destroy", mfcp, PACKAGE="multiframecorr")
    mfcp <<- NULL
  }
}

gain.from.track.to.point = function(coords1, state, coords2, is.consec) {
  ## calculate the gain from point at coords1 in given state to coords2
  ## (regardless of whether coords2 is at a blip)
  ## is.consec is TRUE if coords1 and coords2 are from consecutive scans,
  ## FALSE otherwise.
  ## This is only used to get a gain to display in the pointerinfo window.
  
  on.track <- length(state) > 0
  user.fun <- attr(mfcp, if (on.track) "gain.tp" else "gain.pp")
  if (is.null(user.fun)) {
    .Call("gain_from_track_to_point", mfcp, coords1, state, coords2, is.consec, PACKAGE = "multiframecorr")
  } else {
    if (on.track)
      user.fun(coords1, as.matrix(coords2), state, is.consec)
    else
      user.fun(coords1, as.matrix(coords2), is.consec)
  }
}

set.max.speed = function(x) {
  .Call("set_max_speed", mfcp, x / 3.6, PACKAGE="multiframecorr")
}

set.gain.pp = function(f) {
  use.gain.pp <<- f
  if (!is.null(mfcp))
    .Call("set_gain_pp", mfcp, f)
}

set.gain.tp = function(f) {
  use.gain.tp <<- f
  if (!is.null(mfcp))
    .Call("set_gain_tp", mfcp, f)
}

set.new.state = function(f) {
  use.new.state <<- f
  if (!is.null(mfcp))
    .Call("set_new_state", mfcp, f)
}

get.menu = function() {
  list(
       "multiframecorr" = "menu",
       "options" = "no-tearoff",
       list ("gauge",
             label = "number of scans to backtrack over in building tracks",
             range = c(2, 100),
             increment = 1,
             value = k,
             on.set = function(x) {
               ## FIXME: we reinitialize; eventually we should retain data as much as possible
               k <<- x
               if (!is.null(mfcp))
                 .Call("destroy", mfcp, PACKAGE="multiframecorr")
               mfcp <<- .Call("init", k, alpha, eps, max.dist, track.max.speed / 3.6, min.gain, use.gain.pp, use.gain.tp, NULL, PACKAGE="multiframecorr")
               reprocess.scan()
             }
             ),
       list ("gauge",
             label = "weight of directional coherence vs proximity to prediction",
             range = c(0, 1),
             increment = 0.01,
             value = alpha,
             on.set = function(x) {
               alpha <<- x
               if (!is.null(mfcp)) {
                 .Call("set_alpha", mfcp, alpha, PACKAGE="multiframecorr")
                 reprocess.scan()
               }
             }
             ),
       list ("gauge",
             label = "minimum gain for a blip to join a track (log units)",
             range = c(-150, 150),
             increment = 5,
             format = "%.2f",
             value = gain.to.log.units(min.gain),
             on.set = function(x) {
               min.gain <<- log.units.to.gain(x)
               if (!is.null(mfcp)) {
                 .Call("set_min_gain", mfcp, min.gain, PACKAGE="multiframecorr")
                 reprocess.scan()
               }
             }
             ),
       list ("gauge",
             label = "small penalty for blips missing from tracks (gain units)",
             range = c(0, 1),
             increment = 0.005,
             value = eps,
             on.set = function(x) {
               eps <<- x
               if (!is.null(mfcp)) {
                 .Call("set_eps", mfcp, eps, PACKAGE="multiframecorr")
                 reprocess.scan()
               }
             }
             )
       )
}

select = function() {
  auto.expire.tracks <<- FALSE
}

deselect = function() {
}

load = function() {           
  rss.dyn.load("maxmatch", in.dir.of = plugin.file)
  rss.dyn.load("maxpathcover", in.dir.of = plugin.file)
  rss.dyn.load("multiframecorr", in.dir.of = plugin.file)
}

unload = function(save.config) {
  rss.dyn.unload("multiframecorr")
  rss.dyn.unload("maxpathcover")
  rss.dyn.unload("maxmatch")
}

## R-versions of the C gain and state functions built into
## multiframecorr.c By default, these are not used.  They can be
## forced into use by doing
##
##    with(TRACKER$models$multiframecorr, {set.gain.pp(gain.pp); set.gain.tp(gain.tp); set.new.state(new.state)})
##
## Use of the internal versions can be restored by doing
##
##    with(TRACKER$models$multiframecorr, {set.gain.pp(NULL); set.gain.tp(NULL); set.new.state(NULL)})
##
## and their results compared to those from the internal C versions.

gain.pp = function (u, v, is.cons) {
  ## This function is called in an environment with the multiframe model as an enclosing
  ## environment, so variables from this model and the tracker plugin are available to it
  ## This function corresponds to the function multiframecorr.c: calc_nn_gain(), except that v
  ## is allowed to be a 4 x n matrix of coordinates for n points.  All points in v must be
  ## from the same scan.

  ##  u: real vector with 4 elements x, y, z, t coordinates of point in earlier scan
  ##  v: real 4 x n matrix with rows 1, 2, 3, and 4
  ##     corresponding to x, y, z, and t of points in later scan
  ##  is.cons: logical scalar: are u and all points in v from consecutive scans?

  ## Calculate the gain between each pair of points using the
  ## nearest neighbour gain function.  This is
  ## 1 minus the scaled distance between the points, scaled by 1 - alpha
  ## (i.e. term 2 of Eqn. (4) of [1])
  
  dist <- sqrt(apply((v[1:3, , drop=FALSE] - u[1:3]) ^ 2, 2, sum))

  rv <- (1 - alpha) * (1 - dist / max.dist) - (!is.cons) * eps

  
  ## If the implied speed between two points is larger than
  ## track.max.speed, force it to zero, which will effectively bar
  ## this point/point pair from being matched.  Note: track.max.speed
  ## is in km/h, so convert to m/s by dividing by 3.6

  rv[dist / (v[4, , drop=FALSE] - u[4]) > (track.max.speed / 3.6)] <- 0.0


  return (rv)
}

gain.tp = function (u, v, state, is.cons) {
  ## this function is called in an environment with the multiframe model as an enclosing
  ## environment; it corresponds to the function multiframecorr.c: calc_gain()
  ## except that v is allowed to be a 4 x n matrix of coordinates for n points.
  ## All points in v must be from the same scan.

  ##     calculate the gain in matching the object at u with the object at v, given
  ##     the state at u is given by state.

  ##  u: real vector with elements x, y, z, t
  ##  v: real 4 x n matrix with rows 1, 2, 3, and 4
  ##     corresponding to x, y, z, and t
  ##  state: real vector with elements vx, vy, vz
  ##  is.cons: logical scalar: are u and all points in v from consecutive scans?

  ##     We use Euclidean norms (p=2).  [1] does not indicate what p they use.
  ##     To normalize distances, we use max_dist, which is the diameter of the radar's
  ##     field of view.
  ##     This model assumes constant velocity, and predicts the location of the point given
  ##     its starting location, velocity, and the elapsed time.
  ##     Note: the order of coords in rows of all.blips is t, x, y, z
  ##     The ornate notation in equation 4 of [1] can be reduced to this:
  
  ##     Let u be the position of a point observed in frame i
  ##     Let w be the predicted position in frame j > i of the object represented by that point
  ##     Let v be the position of a point observed in frame j > i
  
  ##     The gain function g3(u, v, w) is given by
  
  ##     alpha * (0.5 + (w-u).(v-u)/(2|w-u||v-u|)) + (1-alpha) * (1 - |v-w|/(2*max.range))
  
  ##     where | | is vector magnitude and . is dot product


  uxyz <- u[1:3]
  ut <- u[4]
  vxyz <- v[1:3, , drop=FALSE]
  vt <- v[4, ]

  dt <- vt - ut


  dist <- sqrt(apply((vxyz - uxyz)^2, 2, sum))

  ## keep track of which pairs imply too high a speed
  ## Note: track.max.speed is in km/h, so convert to m/s by dividing by 3.6

  too.fast <- dist / dt > (track.max.speed / 3.6)

  ## predict a new point position assuming constant velocity

  w <- uxyz + state %o% dt

  ## the "closeness to predicted location" gain component; a negative gain, meaning
  ## a prediction outside the radar coverage area, is replaced by 0.0

  g1 <- pmax(0.0, 1 - sqrt(apply((w - vxyz) ^ 2, 2, sum)) / max.dist)
  
  ## the "directional coherence" gain component
  ## i.e. (1 + the cosine of the turning angle) / 2

  g2 <- 0.5 + apply((w - uxyz) * (vxyz - uxyz), 2, sum) / (2 * sqrt(apply((w - uxyz) ^ 2, 2, sum)) * dist)
  
  ## the actual gain, with possible penalties
  rv <- alpha * g2 + (1 - alpha) * g1  - (!is.cons) * eps

  ## set out-of-bounds gains to zero, ensuring this track / blip pairing
  ## will not be made
  
  rv[(rv < min.gain) | too.fast] <- 0.0

  return (rv)
}

new.state = function (u, state, v) {
  ## Calculate the state of the target at new location v, given its previous state at
  ## previous location u.
  ##
  ## This function is called in an environment with the multiframe model as an enclosing
  ## environment; it corresponds to the function multiframecorr.c: calc_state_internal()
  

  ## u: previous location of target
  ## state: state of target at location u
  ## v: new location of target

  ## Currently, we don't use "state".

  ## Just calculate the apparent velocities in x, y, and z
  ## (the 4th coordinate is t)
  
  return ((v[1:3]-u[1:3]) / (v[4]-u[4]))
}
  
## plugin-level variables

max.dist = 100       ## maximum distance between a pair of points visible in radar;  == 2 * radar range (in m)
mfcp = NULL          ## EXTPTR for the c-level MFC problem instance
use.gain.pp = NULL   ## R function for point-to-point gain calculation
use.gain.tp = NULL   ## R function for track-to-point gain calculation
use.new.state = NULL ## R function for calculating target's new state from old position, old state, and new position

