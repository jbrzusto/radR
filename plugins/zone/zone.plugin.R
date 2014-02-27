##  svn $Id: zone.plugin.R 626 2010-07-17 00:03:27Z john $
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

##      The ZONE radR plugin
##
## After a plugin is loaded, it is assigned to the global environment
## using its upper case name.
##
## e.g. this plugin will be installed as ZONE in .GlobalEnv

## Note: the exclusion zone, if it exists, will always be the first zone
## in plugin variable "zones".

MYCLASS="zone"

about = function() {
  ## a function returning a description and the current state of the plugin.
  paste(c("This plugin define regions for special treatment during processing.\n\nTo edit and create zones, you must enable Tk plotting mode.\n",
          if (is.null(zones)) {
            "No zones are currently defined."
          } else {
            c("Zones read from file: '" %:% zones.file %:% "'",
              capture.output( {
                for (n in names(zones)) {
                  cat(sprintf("\nZone '%s' (%s):\n", n, ifelse(zones[[n]]$enabled, "enabled", "disabled")))
                  print(data.frame(r1=zones[[n]]$r[[1]], r2=zones[[n]]$r[[2]], a.start=zones[[n]]$a[[1]], a.extent=zones[[n]]$a[[2]]))
                }
              })
              )
          }
          ), collapse="\n")
}

## This function returns a set list of plugin-specified menus.
## The menu is only installed after the plugin's load() function is called.
## The menus types are "main", "sources", and "sinks" and get attached to the
## right-click plot-window menu, the player source menu, and the player sink menu, respectively.

get.menus = function() {
  list (
        plugin = list (
          "Create new zone..." = create.new.zone,
          "Show all zones" = function()show.zones(show=TRUE),
          "Hide all zones" = function()show.zones(show=FALSE),
          "Show zone..." = list (
            "dyn.menu",
            function () {
              if (length(zones)) {
                list(zone.list = "Choose zones to show:",
                     c(list(option="choose.any",
                            on.set = function(n, v) show.zone.with.id(names(zones)[n], v)
                            ),
                       set.or.get = ".zone.show",
                       structure(zones %$0% visible, names=names(zones))
                       ))
              } else {
                list("There are no zones defined.")
              }
            }
            ),
          "---",
          "Enable all zones" = function()enable.zones(enable=TRUE),
          "Disable all zones" = function()enable.zones(enable=FALSE),
          "Enable zone..." = list (
            "dyn.menu",
            function () {
              if (length(zones)) {
              list(zone.list = "Choose zones to enable:",
                   c(list(option="choose.any",
                          on.set = function(n, v) {
                            enable.zones(names(zones)[[n]], v)
                            do.update()
                          },
                          set.or.get = ".zone.enable"
                          ),
                     structure(zones %$0% enabled, names = names(zones))
                     )
                   )
              } else {
                list("There are no zones defined.")
              }
            }
            ),
          "Rename zone..." = list (
            "dyn.menu",
            function () {
              n <- names(zones)
              if (length(n)) {
                list(zone.list = "Choose zone to rename:",
                     c(list(option="do.one",
                            on.do = function(i) {
                              rename.zone(n[i])
                            }),
                       n
                       )
                     )
              } else {
                list (zone.list = "No zones defined")
              }
            }
            ),
          
          "---",
          "Copy current blip filtering parameters to zone..." = list (
            "dyn.menu",
            function () {
              n <- names(zones) [names(zones) != "exclusion"]

              if (length(n)) {
                list(zone.list = "Choose zone to receive current blip filtering parameters:",
                     c(list(option="do.one",
                            on.do = function(i) {
                              copy.current.zoneable.pars(n[i])
                              update.zonepar.controls(n[i])
                            }),
                       n
                       )
                     )
              } else {
                list (zone.list = "No special zones defined")
              }
            }
            ),
          
          "Edit zone parameters..." = list (
            "dyn.menu",
            function () {
              n <- names(zones) [names(zones) != "exclusion"]

              if (length(n)) {
                list(zone.list = "Edit blip filtering parameters in zone:",
                     c(list(option="do.one",
                            on.do = function(i) {
                              edit.zone.pars(n[i])
                            }),
                       n
                       )
                     )
              } else {
                list (zone.list = "No special zones defined")
              }
            }
            ),
          "---",
          "Change zone set description..." = edit.zones.desc,
          
          "Load zones..." =
          list ("file",
                type = "open.one",
                title = "Choose a zone file", 
                file.types = file.types.list,
                on.set = function(f) {
                  load.zones(f)
                  do.update()
                },
                init.file = function () zones.file
                ),
          "Save zones" = function() save.zones(),
          "Save zones as ..." =
          list ("file",
                type = "save",
                title = "Create a zone file", 
                file.types = file.types.list,
                on.set = function(f) {
                  if (length(f) == 1 && nchar(f[1]) != 0) {
                    if (length(grep(names(file.types.list)[1] %:% "$", f)) == 0)
                      f <- f %:% names(file.types.list)[1]
                    if (edit.zones.desc())
                      save.zones(f)
                  }
                },
                init.file = function() zones.file
                ),
          "---",
          "zero.data" = 
          list(option="choose.any",
               on.set = function(n, v) {
                 zero.exclusion <<- v
                 ## if excluding, do so immediately; conversely, we can't immediately un-zero,
                 ## since the data have been lost.
                 if (v)
                   do.exclude(RSS$scan.mat, 0)
                 rss.enable.hook("FULL_SCAN", name, v)
                 do.update()
               },
               set.or.get = ".zero.exclusion",
               "Zero data in exclusion zone" = zero.exclusion
               )
          )
        )
}

update.zones = function() {
  ## given that zone parameters or antenna parameters have changed,
  ## recompile the zones, replace that info in RSS$scan.info,
  ## and reset RSS$class.mat to COLD
  
  si <- get.scan.info(RSS$scan.info)
  RSS$scan.info[names(si)] <- si
  if (!RSS$have.valid$classification) {
    ## reset to cold class
    RSS$class.mat[] <- RSS$CLASS.VAL$cold
    RSS$prev.class.mat[] <- RSS$CLASS.VAL$cold
  }
}

do.update = function() {
  ## update the current preview to take into account zone changes
  if (RSS$play.state < RSS$PS$PLAYING) {
    update.zones()
    rss.process.scan(put.scan = FALSE,
                     calculate.scores = FALSE,
                     convert.scan = TRUE,
                     is.preview = TRUE)
  }
  if ( ! isTRUE(zones$exclusion$enabled))
    reset.excluded(RSS$class.mat)
}

enable.zones = function(which = TRUE, enable=TRUE) {
  ## enable (or disable) zones specified by which
  zones[which] %$$% enabled <<- enable
  sync.patchstats.hook()
  do.update()
}

show.zones = function(which = TRUE, show = TRUE) {
  ## show (or hide) zones specified by which, a
  ## vector of indexes (LOGICAL or INTEGER) into zones;
  ## hide if "show" is FALSE

  if (enabled)
    for (z in names(zones)[which])
      show.zone.with.id(z, show)
}

exclusion.to.first = function() {
  ## make sure the exclusion zone, if any, is first in the list
  exi <- which(names(zones) == "exclusion")
  if (length(exi))
    zones <<- c(zones[exi], zones[-exi])
}

ask.for.zone.name = function(title, msg, default) {
  errmsg <- ""
  repeat {
    rv <- rss.gui(POPUP_DIALOG,
                  title = title,
                  msg = errmsg %:% msg %:%
                  if (!"exclusion" %in% names(zones)) "\nTo make this the exclusion zone, use 'exclusion'." else
                  if (default == "exclusion") "\nIf you change the name, this will no longer be the exclusion zone.",
                  entry = TRUE,
                  buttons = c("Ok", "Cancel"),
                  default.entry = default
                  )
    if (rv[[1]] != 1)
      return(NULL)

    id <- gsub(" ", "_", rv[[2]], fixed=TRUE)
    if (id == default || is.null(zones[[id]]))
      break
    errmsg <- "ERROR: There is already a zone named " %:% id %:% "\n"
  }
  return (id)
}
  
rename.zone = function(old.id) {
  z <- zones[[old.id]]
  id <- ask.for.zone.name("Rename zone", "Enter a new one-word name for zone '" %:% old.id %:% "'.", old.id)
  if (is.null(id))
    return()
  destroy.zone(GUI$plot, z)
  zones[[id]] <<- z
  exclusion.to.first()
  if (z$visible)
    show.zone.with.id(id, TRUE)
  if (id == "exclusion" || old.id == "exclusion") {
    sync.patchstats.hook()
    do.update()
  }
}  

load.zones = function(f) {
  ## load zones from the file f
  ok <- FALSE
  if (file.exists(f)) {
    e <- new.env()
    rss.source.into(f, e)
    if (!is.null(e$desc) && !is.null(e$zones)) {

      zones <<- lapply(e$zones, make.env)
      zones.desc <<- e$desc

      exclusion.to.first()
      
      ## for each non-exclusion zone, if any of the zoneable.par
      ## values is unset, copy them from current settings

      lapply(zones[names(zones) != "exclusion"],
             function(z) {
               if (is.null(z$par))
                 z$par <- list()
               for (p in zoneable.pars) 
                 if (is.null(z$par[[p]]))
                   z$par[[p]] <- RSS[[p]]
             })

      ok <- TRUE
      destroy.all.zones(GUI$plot)
      destroy.edit.windows()
    }
  }
  if (ok) {
    show.zones(zones %$0% visible)
    zones.file <<- f
    sync.patchstats.hook()
  } else {
    rss.plugin.error("unable to load zones from file " %:% f)
  }
}

save.zones = function(f = zones.file) {
  ## save zones information to the file f
  ## We copy the original from which it was loaded, since
  ## rss.rewrite.config needs a template.
  
  if (f != zones.file)
    file.copy(zones.file, f, overwrite=TRUE)

  ## rewrite the zone information; note that we write from a listifed version of the zones
  ## variable which omits temporary editing items (whose names start with ".")
  rss.rewrite.config(f, list(desc=zones.desc, zones=lapply(zones, rss.visible.only)), allow.new=TRUE)
  zones.file <<- f
}

edit.zones.desc = function() {
  rv <- rss.gui(POPUP_DIALOG,
                title = "Describe zone set",
                msg = "Enter a description of this set of zones",
                entry = TRUE,
                buttons = c("Ok", "Cancel"),
                default.entry = zones.desc)
  if (rv[[1]] == 1) {
    zones.desc <<- rv[[2]]
    return (TRUE)
  }
  FALSE
}

create.new.zone = function() {
  id <- ask.for.zone.name("Name new zone", "Enter a new one word name for this zone.",
                          default = "special" %:% (1 + max (c(0, as.integer(substring(grep("^special[0-9]+", names(zones), perl=TRUE, value=TRUE), 8))))))

  if (is.null(id))
    return()
  
  ## create a new zone near the middle of the current plot window
  ## i.e. whose "bounding box" is the middle third of the plot in both x and y

  p1 <- dim(RSS$pix.mat) / 3
  p2 <- p1 * 2

  zc <- xy.to.zone.coords(zones$exclusion, cbind(p1, p2))

  ## don't set enabled=TRUE without calling enable.zones to invoke the required side effects
  zones[[id]] <<- make.env(list(visible=TRUE, enabled=FALSE, r = list(r1=zc[1,1] / 2, r2=zc[1,2]), a = list(start=zc[2,1], extent=zc[2,2]), colour = default.new.zone.colour, .hilite=TRUE, .edit=TRUE))

  copy.current.zoneable.pars(id)
  
  exclusion.to.first()
  
  show.zone.with.id(id)
}

show.zone.with.id = function(id, show=TRUE) {
  if (!GUI$enabled) return()
  z <- zones[[id]]
  z$.id <- id
  z$.delproc <- delete.zone
  z$.reproc <- do.update
  if (show) {
    if (!isTRUE(z$.edit))
      z$.hilite <- rep(FALSE, length(z$r[[1]]))
    
    z$.par <- if (id %in% names(styles)) styles[[id]] else styles$default
    
    z$.origin <- c(0, 0) ## coordinates of zone centre, which is the (one and only) radar for now
    
    if (!GUI$plot.is.tk.image)
      gui.set.plot.is.tk.image(TRUE)
    
    show.zone(GUI$plot, z, geom = function() list(mpp = GUI$mpp, north.angle=GUI$north.angle))
  } else {
    hide.zone(GUI$plot, z)
  }
}

copy.current.zoneable.pars = function(id) {
  for (p in zoneable.pars)
    zones[[id]]$par[[p]] <- RSS[[p]]
}

destroy.edit.windows = function() {
  ## destroy any parameter editing windows
  for (w in zonepar.edit.windows)
    tcl("destroy", w)
}

edit.zone.pars = function(id) {
  ## horrible code copied from gui.create.blip.window!!!
  ## FIXME: do we need to defer changes to zone parameters?
  
  z <- zones[[id]]
  w <- ".zone_" %:% id
  if (tclbool("winfo", "exists", w)) {
    tcl("wm", "focus", w)
  } else {
    tcl("toplevel", w)
    tcl("wm", "withdraw", w)
    tcl("wm", "title", w, "Parameters: zone" % % id)
    tcl("wm", "geometry", w, "350x260+" %:% tclchar("winfo", "pointerx", ".") %:% "+" %:% tclchar("winfo", "pointery", ".") )
    tcl("wm", "iconbitmap", w, GUI$application.icon)

    ## create gauges for the parameters in zoneable.pars
    ## NOTE: the order of the setter names must match that in zoneable.pars
    ## so that update.zonepar.controls() works!

    pre <- ".zone_" %:% id %:% ".setter."
    
    g.nslo <- gui.create.gauge(w, "min blip samples", c(2, 50000),
                               GUI$minsamples.spinner.increment,
                               z$par$blip.samples.minmax[1],
                               rss.make.closure(
                                                function(x){
                                                  z$par$blip.samples.minmax <- c(x, z$par$blip.samples.minmax[2])
                                                  if (z$enabled)
                                                    do.update()
                                                }, list(z=z), parent=ZONE),
                               setter.name= pre %:% "1",
                               setter.env = z
                               )

    g.nshi <- gui.create.gauge(w, "max blip samples", c(-1, 50000),
                               GUI$maxsamples.spinner.increment,
                               z$par$blip.samples.minmax[2],
                               rss.make.closure(
                                                function(x){
                                                  z$par$blip.samples.minmax <- c(z$par$blip.samples.minmax[1], x)
                                                  if (z$enabled)
                                                    do.update()
                                                }, list(z=z), parent=ZONE),
                               setter.name= pre %:% "2",
                               setter.env = z
                               )

    g.alo <- gui.create.gauge(w, "min blip area (m^2)", c(0, 50000),
                              GUI$minblip.spinner.increment,
                              z$par$blip.area.minmax[1],
                              rss.make.closure(
                                               function(x){
                                                 z$par$blip.area.minmax <- c(x, z$par$blip.area.minmax[2])
                                                  if (z$enabled)
                                                    do.update()
                                               }, list(z=z), parent=ZONE),
                              setter.name= pre %:% "3",
                              setter.env = z
                              )

    g.ahi <- gui.create.gauge(w, "max blip area (m^2)", c(-1, 50000),
                              GUI$maxblip.spinner.increment,
                              z$par$blip.area.minmax[2],
                              rss.make.closure(
                                               function(x){
                                                 z$par$blip.area.minmax <- c(z$par$blip.area.minmax[1], x)
                                                 if (z$enabled)
                                                   do.update()
                                               }, list(z=z), parent=ZONE),
                              setter.name= pre %:% "4",
                              setter.env = z
                              )
    g.anglo <- gui.create.gauge(w, "min angular span", c(0, 1024),
                                GUI$minangles.spinner.increment,
                                z$par$blip.angular.minmax[1],
                                rss.make.closure(
                                                 function(x){
                                                   z$par$blip.angular.minmax <- c(x, z$par$blip.angular.minmax[2])
                                                   if (z$enabled)
                                                     do.update()
                                                 }, list(z=z), parent=ZONE),
                                setter.name= pre %:% "5",
                                setter.env = z
                                )

    g.anghi <- gui.create.gauge(w, "max angular span", c(-1, 1024),
                                GUI$maxangles.spinner.increment,
                                z$par$blip.angular.minmax[2],
                                rss.make.closure(
                                                 function(x){
                                                   z$par$blip.angular.minmax <- c(z$par$blip.angular.minmax[1], x)
                                                   if (z$enabled)
                                                     do.update()
                                                 }, list(z=z), parent=ZONE),
                                setter.name= pre %:% "6",
                                setter.env = z
                                )

    g.radlo <- gui.create.gauge(w, "min radial span", c(0, 1024),
                                GUI$minangles.spinner.increment,
                                z$par$blip.radial.minmax[1],
                                rss.make.closure(
                                                 function(x){
                                                   z$par$blip.radial.minmax <- c(x, z$par$blip.radial.minmax[2])
                                                   if (z$enabled)
                                                     do.update()
                                                 }, list(z=z), parent=ZONE),
                                setter.name= pre %:% "7",
                                setter.env = z
                                )

    g.radhi <- gui.create.gauge(w, "max radial span", c(-1, 1024),
                                GUI$maxangles.spinner.increment,
                                z$par$blip.radial.minmax[2],
                                rss.make.closure(
                                                 function(x){
                                                   z$par$blip.radial.minmax <- c(z$par$blip.radial.minmax[1], x)
                                                   if (z$enabled)
                                                     do.update()
                                                 }, list(z=z), parent=ZONE),
                                setter.name= pre %:% "8",
                                setter.env = z
                                )

    g.useexpr <-
      tcl("checkbutton", w %:% ".useexpr",
          anchor="w",
          variable=z$.tag %:% "_use.blip.filter.expr",
          text="also filter by logical expression:",
          command = rss.make.closure(
            function() {
              val <- tclboolvar(z$.tag %:% "_use.blip.filter.expr")
              z$par$use.blip.filter.expr <- val
              do.update()
            }, list(z=z), parent=ZONE)
          )

    tcl("set", z$.tag %:% "_use.blip.filter.expr", z$par$use.blip.filter.expr)
    
    g.filtexpr <-
      gui.create.string(parent = w,
                        label = "R",
                        short.name = z$.tag %:% "_blipfilterexpr",
                        width = 30,
                        height = 30,
                        val = as.character(z$par$blip.filter.expr),
                        val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
                        set.fun = rss.make.closure(
                          function(x) {
                            z$par$blip.filter.expr <- parse(text=x)
                            do.update()
                          }, list(z=z), parent=ZONE)
                        )


    tcl("pack", g.nslo, g.nshi, g.alo, g.ahi, g.anglo, g.anghi, g.radlo, g.radhi, g.useexpr, g.filtexpr, side="top", fill="x")

    tcl("wm", "protocol", w, "WM_DELETE_WINDOW",
        rss.make.closure(
                         function() {
                           tcl("wm", "withdraw", w)
                         },
                         list(w=w)
                         )
        )
  }
  tcl("wm", "deiconify", w)
  zonepar.edit.windows <<- c(zonepar.edit.windows, w)
}

delete.zone = function(zone) {
  zones[zone$.id] <<- NULL
}

load = function() {
  rss.dyn.load(MYCLASS, in.dir.of=plugin.file)
  load.zones(zones.file)
  patch.zone.index <<- extmat("patch zone index", type="int", dim=c(10, 1))
}

unload = function(save.config) {
  enable(FALSE)
  RSS$scan.info$zones <- NULL
  RSS$scan.info$zonepars <- NULL
  destroy.edit.windows()
  rss.dyn.unload(MYCLASS)
}

sync.patchstats.hook <- function() {
  ## enable the patchstats hook if necessary
  rss.enable.hook("PATCH_STATS", MYCLASS, enabled && any(zones %$0% enabled))
}

enable = function(enab) {
  enabled <<- enab
  for (h in c("GET_SCAN_INFO", "CLASSIFY", "PRE_SCAN_CONVERT", "PLOT_CURSOR_MOVED"))
    rss.enable.hook(h, MYCLASS, enab)
  rss.enable.hook("FULL_SCAN", MYCLASS, enab && zero.exclusion)
  if (RSS$have.valid$scan.data) 
    do.update()
  if (enab) {
    show.zones(zones %$0% visible)
  } else {
    destroy.all.zones(GUI$plot)
  }
  sync.patchstats.hook()
}

update.zonepar.controls = function (id) {
  ## For the zone given by id, go through the list of zoneable pars and
  ## update corresponding gui controls from the zone's par list.
  
  ## Note that the 
  z <- zones[[id]]
  pref <- ".zone_" %:% id %:% ".setter."
  i <- 1
  for (p in zoneable.pars) {
    for (j in 1:length(z$par[[p]])) {
      try(get(pref %:% i, z) (z$par[[p]][j]), silent=TRUE)
      i <- i + 1
    }
  }
}

compile.zone = function(z, si) {
  ## convert a zone to sample, pulse coordinates
  ##
  ## si is a list of scan.info parameters, having at least these items:
  ##   first.sample.dist
  ##   sample.dist
  ##   bearing.offset
  ##   antenna.angle
  ##   pulses
  ##   samples.per.pulse
  
  ## returns a matrix, m,  with one row per segment and whose columns are:
  ##  1: sample low
  ##  2: sample high
  ##  3: pulse low
  ##  4: pulse high
  ## These cover the corresponding segment in that
  ## e.g. RSS$scan.mat[m[i,1]:m[i,2], m[i,3]:m[i,4]] is the set of samples
  ## in segment i.  If segment i "wraps" (i.e. includes both the first and last
  ## pulse), then all(m[i, 3:4] < 0); i.e. we use "negative indexing" to represent
  ## the non-wrapping complement of the segment.

  ## Note: the compiled form of the zone includes precisely those samples which
  ## overlap with the zone, including boundaries.
  
  ## FIXME: we don't take into account scan.info$orientation (but do we anywhere??)
  
  rv <- matrix(integer(0), nrow = n <- length(z$r[[1]]), ncol=4)
  rps <- si$sample.dist * cos(si$antenna.angle[1] * pi / 180)
  b0 <- si$bearing.offset %% 360.0
  for (i in 1:n) {
    s <- pmax(1, pmin(si$samples.per.pulse, 1 +
                      floor ((sort(c(z$r[[1]][i], z$r[[2]][i]))) / rps - si$first.sample.dist / si$sample.dist)))
    if (z$a[[2]][i] %% 360 != 0) {
      p <- pmax(1, 1 + floor(((c(0, z$a[[2]][i]) + (z$a[[1]][i] + 180 / si$pulses) - b0) %% 360) * (si$pulses / 360)))
      ## if it's a range that includes the first and last pulses, then correct and rewrite it using negative indices
      if (p[2] < p[1])
        p <- - p[2:1] - c(1, 0)
    } else if (z$a[[2]][i] == 360.0) {
      ## special case: full circle
      p <- c(1, si$pulses)
    } else {
      ## special case: empty segment
      p <- c(0, 0)
    }
    rv[i, ] <- as.integer(c(s, p))
  }
  return(rv)
}

compile.pars = function() {
  ## compile the parameters for enabled zones
  
  use <- zones %$0% enabled

  ## the compiled parameter list: for each parameter name, the concatenation of
  ## the parameter values for the enabled zones.
  ## The exclusion zone has no par element, so won't add to these entries;
  ## i.e. each element of compiled.pars contains the catenation of parameters
  ## from the enabled non-exclusion zones, if there are any.

  ## KLUDGE: to mimic the behaviour of the existing filtering, a
  ## negative upper bound in these parameter ranges means not to
  ## apply the max.  Dumb, I know.

  compiled.pars <<- list()
  for (p in zoneable.pars)
    lapply(zones[use],
           function(z) {
             pars <- z$par[[p]]
             if (length(pars) == 2 && pars[2] < 0)
               pars[2] <- Inf
             compiled.pars[[p]] <<- c(compiled.pars[[p]], pars)
           })
}


do.exclude = function(mat, v = RSS$CLASS.VAL$excluded) {
  ex <- RSS$scan.info$zones$exclusion
  if (!is.null(ex)) {
    for (i in 1:dim(ex)[1])
      mat[ex[i,1]:ex[i,2], ex[i,3]:ex[i,4]] <- v
    return(TRUE)
  } else {
    return (FALSE)
  }
}

reset.excluded = function(classmat) {
  ## reset class of samples from "excluded" to "cold"
  classmat[c(classmat[]) == RSS$CLASS.VAL$excluded] <- RSS$CLASS.VAL$cold
}

get.scan.info = function(si) {
  ## Return the compiled zone info
  ## This is a list of compiled zone matrices, tagged by zone id.
  ## It is this compiled version, created at the top of each scan, that
  ## is used by processing hooks, avoiding GUI race conditions.

  if (!is.null(si$pulses)) {
    enabled.zones <<- names(zones) [zones %$0% enabled]
    si$zones <- compiled.zones <<- lapply(zones[enabled.zones], compile.zone, si)
    compile.pars()
    si$zonepars <- compiled.pars
    si$zero.exclusion <- zero.exclusion
    ## If the source already had zone info (and possibly "prev" zone info), then
    ## combine these into the "prev" attribute of the new zone info fields
    ## The point is to keep a record of all zones and their parameters used in
    ## arriving at the current scan.
    if (!is.null(RSS$source.scan.info$zones)) {
      attr(si$zones, "prev")    <- c(attr(RSS$source.scan.info$zones, "prev"),    "---" = NULL, RSS$source.scan.info$zones)
      attr(si$zonepars, "prev") <- c(attr(RSS$source.scan.info$zonepars, "prev"), "---" = NULL, RSS$source.scan.info$zonepars)
    }
  } else {
    ## create a slot for the zones, even though we don't populate it
    si$zones <- list()
    si$zonepars <- list()
    si$zero.exclusion <- FALSE
  }
  return(si)
}

hooks = list(
  ANTENNA_CONFIG = list( enabled = TRUE, read.only = TRUE, f = update.zones),
  
  GET_SCAN_INFO = list( enabled = TRUE, read.only = FALSE, f = get.scan.info),

  PLOT_CURSOR_MOVED = list( enabled = TRUE, read.only = FALSE,
    f = function (pc, sc, sa, ce, ...) {
      zi <- integer(1)
      if (!is.null(compiled.zones)) {
        zz <- compiled.zones
        .Call("sample_pulse_to_zone", zz, as.integer(sa), zi)
      } else {
        zz <- zones[zones %$0% enabled]
        .Call("x_y_to_zone", zz, sc$xyz[1:2], zi)
      }
      return(c(if (zi > 0) "Zone: " %:% names(zz)[[zi]] else "", ""))
    }),     

  CLASSIFY = list( enabled = FALSE, read.only = FALSE,
    f = function(classmat, counts) {
       ## only classify excluded samples as EXCLUDED if we haven't already zeroed them
      if ( !zero.exclusion && do.exclude(classmat)) {
        nhot <- sum(classmat[] == RSS$CLASS.VAL$hot)
        return(c(prod(dim(classmat)), nhot, 0))
      } else {
        return(NULL)
      }
    }),

  FULL_SCAN= list( enabled = FALSE, read.only = FALSE,
    ## if we are supposed to zero data in the excluded zone, do so now
    f = function(scanmat) if (zero.exclusion) do.exclude(scanmat, 0)),

  PRE_SCAN_CONVERT = list (enabled = FALSE, read.only = FALSE,
    ## do exclusion before scan conversion if the stats learning
    ## phase has not been completed; this allows the exclusion zone
    ## to be blanked even during learning
    
    f = function(...) {
      if (! RSS$have.valid$classification) {
        dim(RSS$prev.class.mat) <- dim(RSS$class.mat) <- dim(RSS$scan.mat)
        if ("exclusion" %in% names(RSS$scan.info$zones))
          do.exclude(RSS$class.mat)
      }
    }),
  
  PATCH_STATS = list( enabled = FALSE, read.only = FALSE,
    f= function(is.blip) {

      if (!length(is.blip))
        return(is.blip)

      stats <- RSS$patches

      ## get the numbers of patches in each zone, and each patch's zone index
      npz <- .Call("sample_pulse_to_zone", compiled.zones, rss.xy.to.sp(stats$x[], stats$y[]), patch.zone.index)

      ## The exclusion zone, if any, is in the first slot.
      have.ex <- names(compiled.zones)[1] == "exclusion"

      ## Only examine those patches which are in a non-exclusion zone
      ## (index 0 means not in a zone, so if there is no exclusion
      ## zone, this simply restricts to patches that are in a zone)
      
      in.zone <- which(patch.zone.index[] > have.ex)

      ## The max and min allowed values for parameters are stored at
      ## even and odd indexes in the compiled.pars sublists,
      ## respectively.  Compute the relevant indices once:
      
      hi <- 2L * (patch.zone.index[in.zone] - have.ex)
      lo <- hi - 1L
      
      is.blip[in.zone] <- (
                           stats$ns   [in.zone] >= compiled.pars$blip.samples.minmax[lo] &
                           stats$ns   [in.zone] <= compiled.pars$blip.samples.minmax[hi] &
                           stats$area [in.zone] >= compiled.pars$blip.area.minmax   [lo] &
                           stats$area [in.zone] <= compiled.pars$blip.area.minmax   [hi] &
                           stats$aspan[in.zone] >= compiled.pars$blip.angular.minmax[lo] &
                           stats$aspan[in.zone] <= compiled.pars$blip.angular.minmax[hi] &
                           stats$rspan[in.zone] >= compiled.pars$blip.radial.minmax [lo] &
                           stats$rspan[in.zone] <= compiled.pars$blip.radial.minmax [hi]
                           )
      
      ## Filter out any blips in the exclusion zone
      ## and remove the blip count for that zone
      if (have.ex) {
        is.blip[patch.zone.index[] == 1] <- FALSE
        npz <- npz[-1]
      }

      ## for those special zones which have patches and where a blip filtering
      ## expression is active apply it.  

      if (length(npz)) {
        for (i in which(compiled.pars$use.blip.filter.expr & npz > 0)) {
          in.zone <- which(patch.zone.index[] == i + have.ex)
          is.blip[in.zone] <- rss.filter.patches.by.expr(is.blip, compiled.pars$blip.filter.expr[[i]])[in.zone]
        }
      }
      
      return(is.blip)
    })
  )

globals = list(
  )

## zone code

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
             

  org <- c(GUI$tx.xy.to.plot(t(zone$.origin)))
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
    org <- c(GUI$tx.xy.to.plot(t(zone$.origin)))
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

  relpos <- pos - c(GUI$tx.xy.to.plot(t(zone$.origin)))
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

## plugin vars

## the list of current zones (if an exclusion zone exists, it must be the first
## item in the list)
zones = NULL

## description of current zones
zones.desc = NULL

## file types for saving zones
file.types.list = list(".zone.R" = "radR zone definition files", ".*" = "All files")

## list of active zone editors (which are environments)
active.editors = list()

## the ids of zones which were compiled, valid for the current scan
enabled.zones = NULL

## the compiled list of zones, valid for the current scan
compiled.zones = NULL

## the list of blip-filtering parameters, each being a vector with one
## element per enabled zone, including the "exclusion" zone.

compiled.pars = NULL

## an extmat for holding the zone index of each patch in the current scan
patch.zone.index = NULL

## which blip-finding/filtering parameters can be specified for a zone
zoneable.pars = c("blip.samples.minmax", "blip.area.minmax", "blip.angular.minmax", "blip.radial.minmax", "use.blip.filter.expr", "blip.filter.expr")

## what zone parameter editing windows exist?
zonepar.edit.windows = NULL

