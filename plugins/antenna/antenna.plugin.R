##  svn $Id: antenna.plugin.R 578 2010-05-11 02:36:53Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2010 John Brzustowski
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

## The ANTENNA radR plugin, for editing antenna configuration.

about = function() {
  return(c("Antenna configuration:  this plugin is always loaded and enabled.",
           "\nCurrent configuration from file: " %:% antenna.filename,
           "\nAntenna description: " %:% parms$antenna.desc,
           "\n\n    Note on Antenna Parameters\n\
This plugin allows you to specify that the radar antenna is rotating about\
any axis, not just the usual vertical axis.  The rotation axis is given by two\
angles: the angle above horizontal (90 degrees, for the usual vertical axis)\
and the compass direction toward which the axis has been tilted from the vertical.\
The plane perpendicular to the axis of rotation is called the 'rotation plane'.\
\nYou may also specify that the antenna is tilted with respect to the rotation axis.\
For example, for a dish antenna, this could mean that the dish is rotating about\
a vertical axis, but that the parabola axis is pointing at 30 degrees above the horizon.\
This parameter is referred to as 'angle of beam above the rotation plane', and in this\
example, would be '30'.  Other combinations are possible, and plugins are permitted\
to dynamically change these values, so that a given pulse might be pointing in any\
spherical direction."))
}

get.menus = function() {
  list(
       plugin = c(list(
         "Load antenna configuration ..." =
         list ("file",
               type = "open.one",
               title = "Choose an antenna configuration file", 
               file.types = file.types.list,
               on.set = function(f) {
                 load.antenna.config(f)
                 sync.controls()
                 do.update()
               },
               init.file = function () antenna.filename
               ),
         "Save antenna configuration" = function() save.antenna.config(),
         "Save as ..." =
         list ("file",
               type = "save",
               title = "Choose a antenna configuration file", 
               file.types = file.types.list,
               on.set = function(f) {
                 if (length(grep(names(file.types.list)[1] %:% "$", f)) == 0)
                   f <- f %:% names(file.types.list)[1]
                 save.antenna.config(f)
               },
               init.file = function() antenna.filename
               ),
         "---",
         parm.label$antenna.type,
         c(list(mode="choose.one",
                on.set = function(n) {
                  parms$antenna.type <<- antenna.types[n]
                  sync.controls()
                  do.update()
                }),
           structure(if (is.null(parms$antenna.type)) rep(FALSE, length=length(antenna.types)) else parms$antenna.type == antenna.types,
                     names=antenna.types)
           )),
         "Edit antenna description..." = edit.antenna.description,
         get.gauges(),
         list(
              "Values from controls above override these source parameters:",
              c(
                list(override="choose.any",
                     set.or.get="ANTENNA.override",
                     on.set = set.override 
                     ),
                structure(override, names=as.character(parm.label))
                ),
              "Override all" = function() {
                for (i in seq(along=parms))
                  ANTENNA.override(i, TRUE)
                set.override(seq(along=parms), TRUE)
                
              },
              "Override none" = function() {
                for (i in seq(along=parms))
                  ANTENNA.override(i, FALSE)
                set.override(seq(along=parms), FALSE)
              }
              )
         )
       )
}

set.override = function(n, v) {
  ## enable/disable override of existing antenna information
  ## n: index into parms of parameter(s) to be overriden
  ## v: TRUE to override, FALSE to preserve any existing value
  ##    from the source.  Note that if the source provides no
  ##    value for a parameter, the antenna plugin will always supply it.
  
  override[n] <<- v
  
  if (RSS$play.state < RSS$PS$PLAYING) {
    names <- parm.names[n]
    if (any(override[n] != i.am.provider[n])) {
      ## for any overriden parameters, mark this plugin as the provider
      i.am.provider[n[override[n]]] <<- TRUE
      ## for any non-overriden parameters for which the source provides a value,
      ## mark this plugin as NOT the provider, and restore the original values
      not.provider <- (!override[n]) & sapply(names, function(x)!is.null(RSS$source.scan.info[[x]]))
      i.am.provider[n[not.provider]] <<- FALSE
      RSS$scan.info[names[not.provider]] <- RSS$source.scan.info[names[not.provider]]
      do.update(TRUE)
    }
  }
}

get.gauges = function() {
  ## return the gauges for antenna parameters in
  ## alphabetical order by parameter name,
  ## which is the same name used in the override menu
  rv <- list (
              list (bearing.offset = "gauge",
                    label = parm.label$bearing.offset,
                    range = c(-180, 360),
                    increment = 1,
                    value = parms$bearing.offset,
                    format = "%.1f",
                    on.set = function(x) { parms$bearing.offset <<- x; do.update()},
                    set.or.get = "set.bearing.offset"
                    ),
              list (first.sample.dist = "gauge",
                    label = parm.label$first.sample.dist,
                    range = c(-10000, 100000),
                    increment = 5,
                    value = parms$first.sample.dist,
                    format = "%.1f",
                    on.set = function(x) { parms$first.sample.dist <<- x; do.update()},
                    set.or.get = "set.first.sample.dist"
                    ),
              list (angle = "gauge",
                    label = parm.label$antenna.angle,
                    range = c(-90, 90),
                    increment = 1,
                    format = "%.1f",
                    value = parms$antenna.angle,
                    on.set = function(x) { parms$antenna.angle <<- x; do.update()},
                    set.or.get = "set.angle"
                    ),
              list (aperture.h = "gauge",
                    label = parm.label$antenna.aperture.h,
                    range = c(0, 360),
                    increment = 1,
                    format = "%.1f",
                    value = parms$antenna.aperture.h,
                    on.set = function(x) { parms$antenna.aperture.h <<- x; do.update()},
                    set.or.get = "set.aperture.h"
                    ),
              list (aperture.v = "gauge",
                    label = parm.label$antenna.aperture.v,
                    range = c(0, 180),
                    increment = 1,
                    format = "%.1f",
                    value = parms$antenna.aperture.v,
                    on.set = function(x) { parms$antenna.aperture.v <<- x; do.update()},
                    set.or.get = "set.aperture.v"
                    ),
              list (latitude = "gauge",
                    label = parm.label$latitude,
                    range = c(-90, 90),
                    increment = 1,
                    format = "%.1f",
                    value = parms$latitude,
                    on.set = function(x) { parms$latitude <<- x; do.update()},
                    set.or.get = "set.latitude"
                    ),
              list (longitude = "gauge",
                    label = parm.label$longitude,
                    range = c(-180, 180),
                    increment = 1,
                    format = "%.1f",
                    value = parms$longitude,
                    on.set = function(x) { parms$longitude <<- x; do.update()},
                    set.or.get = "set.longitude"
                    ),
              list (elevation = "gauge",
                    label = parm.label$elevation,
                    range = c(-10000, 10000),
                    increment =  1,
                    value = parms$elevation,
                    on.set = function(x) { parms$elevation <<- x; do.update()},
                    set.or.get = "set.elevation"
                    ),
              list (longitude = "gauge",
                    label = "Rotation axis: elevation above horizontal, in degrees",
                    range = c(-90, 90),
                    increment = 1,
                    format = "%.1f",
                    value = parms$rotation.axis[2],
                    on.set = function(x) { parms$rotation.axis[2] <<- x; do.update()},
                    set.or.get = "set.rotation.axis.elev"
                    ),
              list (longitude = "gauge",
                    label = "Rotation axis: direction of tilt, in degrees clockwise from N",
                    range = c(0, 360),
                    increment = 1,
                    format = "%.1f",
                    value = parms$rotation.axis[1],
                    on.set = function(x) { parms$rotation.axis[1] <<- x; do.update()},
                    set.or.get = "set.rotation.axis.azi"
                    )
              )
  return(rv[order(sapply(rv, function(x) names(x)[1]))])
}

sync.controls = function() {
  ## update the values in the GUI controls for
  ## antenna parameters.  Called when these are
  ## read from a new file.
  ## In case the plugin is enabled upon loading,
  ## we need to check whether the set.XXX functions
  ## have been created by the gui (they are created
  ## at GUI menu creation time).

  if (exists("set.angle")) {
    set.bearing.offset     (parms$bearing.offset)
    set.first.sample.dist  (parms$first.sample.dist)
    set.angle              (parms$antenna.angle)
    set.aperture.h         (parms$antenna.aperture.h)
    set.aperture.v         (parms$antenna.aperture.v)
    set.latitude           (parms$latitude)
    set.longitude          (parms$longitude)
    set.elevation          (parms$elevation)
    set.rotation.axis.azi  (parms$rotation.axis[1])
    set.rotation.axis.elev (parms$rotation.axis[2])
  }
}

do.update = function(changed = FALSE) {
  ## we've updated parms internally
  ## Now (maybe) change them in RSS$scan.info and
  ## inform other plugins of the change.
  for (i in seq(along=parms))
    if (i.am.provider[i]) {
      RSS$scan.info[[parm.names[i]]] <- parms[[parm.names[i]]]
      changed <- TRUE
    }
  if (changed)
    rss.call.hooks(RSS$ANTENNA_CONFIG_HOOK)
}

load = function() {
  ## read antenna configuration
  load.antenna.config()
  i.am.provider <<- rep(FALSE, length(parms))
}

unload = function(save.config) {
  if (save.config)
    save.antenna.config()
}
  
load.antenna.config = function(f = antenna.filename) {
  ## load antenna configuration from the file f
  e <<- strictenv(PARENT=.GlobalEnv)
  rss.source.into(f, e)
  if (length(names(e)) == 0) {
    rss.gui(POPUP_MESSAGEBOX,
            "Antenna missing",
            "radR was unable to load antenna information from the file '" %:%
            f %:% "'.\nDefault values will be used, but you should fix this.",
            time.to.live=30)
    parms <<- default.parms
  } else {
    parms <<- as.list(e)
  }
  parms <<- parms[order(names(parms))]
  parm.names <<- names(parms)
  antenna.filename <<- f
}

edit.antenna.description = function() {
  rv <- rss.gui(POPUP_DIALOG,
                title = "Describe antenna",
                msg = "Enter a description of this antenna",
                entry = TRUE,
                buttons = c("Ok", "Cancel"),
                default.entry = parms$antenna.desc)
  if (rv[[1]] == 1) {
    parms$antenna.desc <<- rv[[2]]
    return (TRUE)
  }
  FALSE
}

save.antenna.config <- function(f = antenna.filename) {
  ## save antenna information to the file f
  ## We copy the original from which it was loaded, since
  ## rss.rewrite.config needs a template.
  if (f != antenna.filename)
    file.copy(antenna.filename, f, overwrite=TRUE)
  rss.rewrite.config(f, parms, allow.new=TRUE)
  antenna.filename <<- f
}

hooks = list(

  GET_SCAN_INFO = list( enabled = TRUE, read.only = TRUE,
    f = function(si) {
      ## For each possible item of antenna information, see whether it has has been provided by the data source,
      ## and if not, supply it ourselves.  This allows an old blipmovie archive to be read and have antenna
      ## information added while re-recording as a new blipmovie, while allowing for data sources to provide
      ## some antenna info (e.g. lat/long)

      i.am.provider <<- override | as.logical(lapply(si[parm.names], is.null))
      p <- parms[i.am.provider]
      si[names(p)] <- p
      return (si)
    })
  
  ) ## end of hooks

## additional plugin variables

file.types.list = list(".antenna.R" = "radR antenna config files", ".*" = "All files")

parms = NULL ## where antenna information is loaded to / saved from, and copied to scan.info from

parm.names = NULL ## where we cache names(parms)

i.am.provider = NULL  ## for each element of parms, did this plugin supply it for the current scan?
