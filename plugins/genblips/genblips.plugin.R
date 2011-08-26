##  svn $Id: genblips.plugin.R 692 2010-12-07 15:46:22Z john $
##
###< Copyright
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
###>

##   genblips.plugin.R
##
## A simple plugin for generating artificial blips from a dynamic
## model of targets.  Provides a reader port.
##

MYCLASS="genblips"

about = function() {
  return("This plugin provides a reader port that provides random artificial blips\naccording to a dynamic model.\nSome of the model parameters are actually R expressions,\nwhich you can modify.\n\nTargets within the radar beam are classified as blips, whereas\n those outside the beam are classified as cold.")
}

get.ports = function() {

  rv <- list()

  make.port <- function(name, id, is.source, is.sink) {
    structure(strictenv(
                        ## common to all ports
                        name = name,
                        id = id,
                        is.source = is.source,
                        is.sink = is.sink,
                        is.live = FALSE,
                        is.file = FALSE,
                        is.seekable = FALSE,
                        can.specify.start.time = FALSE,
                        config = list(),
                        file.ext = "",
                        has.toc = FALSE
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- make.port("Artificial blip generator", 1,  TRUE, FALSE)
  rv
}

load = function() {
  ## initialize state variables here
  rss.dyn.load("genblips", in.dir.of=plugin.file)
}

unload = function(save.config) {
  rss.dyn.unload("genblips")
}


get.menus = function() {
  list(
       sources = list(
         titles = "Artificial blip generator",
         menu = list(
           options = "no-tearoff",
            "Start" = function() rss.set.port(get.ports()[[1]])
           )
         ),
       plugin = list (
         list(option="choose.any",
              on.set = function(n, v) {
                if (n == 1)
                  generate.csv.file <<- v
                else if (n == 2)
                  only.export.visible <<- v
              },
              "Generate a truetracks.csv file" = generate.csv.file,
              "Only export visible targets to .CSV file" = only.export.visible
              ),
         radius=list ("gauge",
           label = "radius of plotted targets and blips",
           range = c(0, 100),
           increment = 1,
           value = radius,
           on.set = function(x) { radius <<- x; reset.blips.deltas()}
           ),
         list ("gauge",
               label = "maximum number of simultaneous targets:   target.n.max",
               range = c(0, 1000),
               increment = 1,
               value = target.n.max,
               on.set = function(x) { target.n.max <<- x}
               ),
         list ("gauge",
               label = "mean new targets per scan:   target.new.mu"
               ,
               range = c(0, 1000),
               increment = 1,
               value = target.new.mu,
               on.set = function(x) { target.new.mu <<- x}
               ),
         list ("gauge",
               label = "mean noise blips per scan:   noise.blips.mu",
               range = c(0, 1000),
               increment = 1,
               value = noise.blips.mu,
               on.set = function(x) { noise.blips.mu <<- x}
               ),
         list ("gauge",
               label = "mean target radar cross section; (UNUSED):   target.rcs.mu",
               range = c(0, 1000),
               increment = 1,
               value = target.rcs.mu,
               on.set = function(x) { target.rcs.mu <<- x}
               ),
         list ("gauge",
               label = "mean noise radar cross section; (UNUSED):   noise.rcs.mu",
               range = c(0, 1000),
               increment = 1,
               value = noise.rcs.mu,
               on.set = function(x) { noise.rcs.mu <<- x}
               ),
         list ("gauge",
               label = "multiplies accelerations each scan:   target.accel.decay",
               range = c(0, 5),
               increment = .01,
               value = target.accel.decay,
               on.set = function(x) { target.accel.decay <<- x}
               ),
         list ("gauge",
               label = "maximum target speed in km/hr:   target.max.speed",
               range = c(0, 1000),
               increment = 1,
               value = target.max.speed,
               on.set = function(x) { target.max.speed <<- x}
               ),
         list ("string",
               label = "number of new targets in scan #s:   target.new.num",
               width = 40,
               height = 2,
               value = as.character(target.num.new),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { target.num.new <<- parse(text=x)}
               ),
         list ("string",
               label = "n x 3 matrix of initial target locations:   target.x0",
               width = 40,
               height = 2,
               value = as.character(target.x0),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { target.x0 <<- parse(text=x)}
               ),
         list ("string",
               label = "n x 3 matrix of initial target velocities:   target.v0",
               width = 40,
               height = 2,
               value = as.character(target.v0),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { target.v0 <<- parse(text=x)}
               ),
         list ("string",
               label = "n x 3 matrix of initial target accelerations:   target.a0",
               width = 40,
               height = 2,
               value = as.character(target.a0),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { target.a0 <<- parse(text=x)}
               ),
         list ("string",
               label = "vector of n target radar cross sections:   target.rcs",
               width = 40,
               height = 2,
               value = as.character(target.rcs),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { target.rcs <<- parse(text=x)}
               ),
         list ("string",
               label = "number of noise blips in scan #s:   noise.num.blips",
               width = 40,
               height = 2,
               value = as.character(noise.num.blips),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { noise.num.blips <<- parse(text=x)}
               ),
         list ("string",
               label = "n x 3 matrix of noise blip locations:   noise.x0",
               width = 40,
               height = 2,
               value = as.character(noise.x0),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { noise.x0 <<- parse(text=x)}
               ),
         list ("string",
               label = "vector of n noise blip radar cross sections:   noise.rcs",
               width = 40,
               height = 2,
               value = as.character(noise.rcs),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { noise.rcs <<- parse(text=x)}
               ),
         list ("string",
               label = "number of noise blips in scan s:   noise.num.blips",
               width = 40,
               height = 2,
               value = as.character(noise.num.blips),
               val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
               on.set = function(x) { noise.num.blips <<- parse(text=x)}
               )
         )
       )
}

reset.blip.deltas <- function() {
  blip.deltas <<- outer(-radius:radius, scan.info$samples.per.pulse * (-radius:radius), `+`)
}

paint.blips <- function(scan.mat, class.mat, xrad, yrad, rcs, inbeam) {
  ## given "targets" at radar coordinates (xrad, yrad)
  ## and with radar cross section rcs
  ## paint plausible representations of the
  ## targets into the scan and class matrices
  ## (which must both be extmats already dimensioned
  ## to match scan.info[c("pulses", "samples.per.pulse")]

  sp <- rss.xy.to.sp(xrad, yrad)

  keep <- !is.na(sp[, 1]) & !is.na(sp[, 2]) &
           sp[, 1] > radius & sp[, 1] <= scan.info$samples.per.pulse - radius 
  sp <- sp[keep, ,drop = FALSE]
  if (!length(sp))
    return()
  cv <- as.integer(ifelse(inbeam[keep], RSS$CLASS.VAL$hot, RSS$CLASS.VAL$excluded))

  ## get linear sample-pulse coordinates for centre of each blip
  lsp <- as.integer((sp[, 2] - 1) * (scan.info$samples.per.pulse) + sp[, 1])

  ## compute linear coordinates of all samples in all blips and wrap around at 0-degree cut
  nn <- prod(dim(scan.mat))
  lcc <- 1 + (outer(blip.deltas, lsp, `+`) - 1) %% nn

  ## paint in scan and class matrices; FIXME: do something sensible with the rcs
  scan.mat[lcc] <- rep(as.integer(pmin(2^scan.info$bits.per.sample - 1, rcs[keep] * 100)), each=length(blip.deltas))
  class.mat[lcc] <- rep(cv, each=length(blip.deltas))
}

globals = list (
  as.character.genblips = function(x, ...) {
    sprintf("radR interface port: %s",
            MYCLASS
            )
  },

  print.genblips = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.genblips = function(port, ...) {
    ## send or get configuration options from a port
    ## for example, this is how to set the filename for a port:
    ## config(port, filename="whatever.dat")
    ## if opts == NULL, the current configuration is returned.
    ## if opts != NULL, the configuration is set
    ## and TRUE is returned on success, FALSE on error

    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 port$file.basename <- strsplit(opts$filename, "\\.[a-zA-Z]*$")[[1]]
               },
               {
                 rss.plugin.error("raw: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

##   get.contents.genblips = function(port, ...) {
##     ## gets the contents of the current (filetype)
##     ## source, namely
##     ## a list with three elements
##     ## num.images: number of images in each run
##     ## start.time:  starting timestamp of each run
##     ## end.time:    ending timestamp of each run

##     port$contents <- port$bl[[1]]
##     class(port$contents$start.time) <- "POSIXct"
##     class(port$contents$end.time) <- "POSIXct"
##     port$first.scan <- cumsum(c(1, port$contents$num.scans))
##     return(port$contents)
##   },

  end.of.data.genblips = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    return(scan.num >= num.scans)
  },

  get.scan.info.genblips = function(port, ...) {
    ## gets the header information for the next scan
    ## along with probably caching this information internally,
    ## this returns a list with these elements:
    ##
    ## pulses: number of pulses in this scan
    ##
    ## samples.per.pulse: number of samples per pulse
    ##
    ## bits.per.sample: number of bits per sample
    ##
    ## timestamp: POSIXct-style timestamp of start of scan, including fractional seconds
    ##
    ## duration: length of this scan (milliseconds) (i.e. rotation time
    ## for radar)
    ##
    ## sample.dist: what distance increment each sample represents (metres)
    ##
    ## first.sample.dist: distance at which first sample begins (metres)
    ##
    ## bearing:  how many degrees clockwise from north is the first
    ##                 pulse?
    ##
    ## orientation: rotation direction: +1 = clockwise, -1 = counterclockwise

    ## we simply copy these parameters from the plugin's config,
    ## which is in scope

    scan.num <<- 1 + scan.num
    si <- scan.info
    si$timestamp <- init.time + scan.num * si$duration / 1000
    return(si)
  },

  get.scan.data.genblips = function(port, extmat, ...) {

    ## empty the extmats corresponding to raw data and class
    dim(extmat) <- c(scan.info$samples.per.pulse, scan.info$pulses)
    zero(extmat)
    dim(RSS$class.mat) <- dim(extmat)
    zero(RSS$class.mat)

    ## drop any targets which are no longer in range or which have hit the ground
    targets <<- subset(targets, range <= 2 * max.range & z > 0)

    ## move any remaining targets
    if (nrow(targets)) {
      ## determine the time to next encounter for each target
      .C("next_encounter", nrow(targets), 1L, targets$x, targets$y, targets$z, targets$vx, targets$vy, targets$vz, targets$ax, targets$ay, targets$az, targets$timestamp, 1000 / scan.info$duration, DUP=FALSE)
    }

    ## add more blips

    if (nrow(targets) < target.n.max) {
      n <- min(target.n.max - nrow(targets), eval(target.num.new))
      if (n) {
        new <- nrow(targets)+(1:n)
        targets[new ,] <<- cbind(rep(0, n),                                  ## scan.no
                                 (1:n) + max.target.id,                      ## id
                                 rep(0, n),                                  ## age
                                 rep(0, n),                                  ## timestamp
                                 eval(target.x0),                            ## x, y, z
                                 eval(target.v0),                            ## vx, vy, vz
                                 eval(target.a0),                            ## ax, ay, az
                                 rep(0, n), rep(0, n), rep(0, n), rep(0, n), ## range, azi, speed, elev
                                 eval(target.rcs), rep(FALSE, n),            ## rcs, visible
                                 double(n), double(n), double(n))            ## x.rad, y.rad, z.rad
        
        max.target.id <<- max.target.id + n
        ## timestamp corresponding to time at top of scan plus offset in sweep
        targets$timestamp[new] <<- init.time + scan.num * scan.info$duration + (pi / 2 - atan2(targets$y[new], targets$x[new])) / (2 * pi) * scan.info$duration / 1000
      }
    }

    ## update age, range, speed, elevation angle and visibility

    if (nrow(targets)) {
      targets$age <<- targets$age + 1
      targets$range <<- with(targets, sqrt(x^2+y^2+z^2))
      targets$speed <<- with(targets, sqrt(vx^2+vy^2+vz^2))
      targets$scan.no <<- scan.num
      too.fast <- targets$speed > target.max.speed
      if (sum(too.fast)) {
        ## for speeds exceeding the maximum, scale the velocity so speed equals the maximum
        adj <- target.max.speed / targets$speed[too.fast] 
        targets$vx[too.fast] <<- targets$vx[too.fast] * adj
        targets$vy[too.fast] <<- targets$vy[too.fast] * adj
        targets$vz[too.fast] <<- targets$vz[too.fast] * adj
      }
      targets$elev <<- with(targets, deg(asin (z / range)))
      targets$visible <<- with(targets, abs(elev - RSS$scan.info$antenna.angle) <= RSS$scan.info$antenna.aperture.v / 2)
      targets$azi <<- with(targets, atan2(y, x))
      cr <- cos(rad(RSS$scan.info$antenna.angle))
      targets$x.rad <<- with(targets, range * cr * cos(targets$azi))
      targets$y.rad <<- with(targets, range * cr * sin(targets$azi))
      targets$z.rad <<- with(targets, range * sin(rad(RSS$scan.info$antenna.angle)))
    }
    
    ## generate noise blips
    n <- eval(noise.num.blips)
    if (n) {
      noise.x <- eval(noise.x0)
      ## convert noise locations to what would be seen by the radar
      noise.range <- sqrt(apply(noise.x^2, 1, sum))
      noise.azi <- atan2(noise.x[,2], noise.x[,1])
      cr <- cos(rad(RSS$scan.info$antenna.angle))
      noise.xrad <- noise.range * cr * cos(noise.azi)
      noise.yrad <- noise.range * cr * sin(noise.azi)
      noise.rcs <- eval(noise.rcs)
    } else {
      noise.xrad <- NULL
      noise.yrad <- NULL
      noise.rcs <- NULL
    }

    ## paint blips into scan and class matrices
    if (nrow(targets) + length(noise.xrad) > 0) {
      paint.blips(extmat, RSS$class.mat, c(targets$x.rad, noise.xrad), c(targets$y.rad, noise.yrad), c(targets$rcs, noise.rcs), c(targets$visible, rep(TRUE, length(noise.xrad))))
      RSS$num.blips <- sum(targets$visible, length(noise.xrad))
    } else {
      RSS$num.blips <- 0
    }
    RSS$num.hot.samples <- RSS$num.blips * (2*radius+1)^2
    RSS$new.have.valid$stats <- FALSE
    RSS$new.have.valid$scores <- FALSE
    RSS$new.have.valid$classification <- TRUE

    return(extmat)
  },

  start.up.genblips = function(port, ...) {
    ## disable the blip finding controls
    rss.gui(ENABLE_CONTROLS, "blip.finding", FALSE)
    scan.num <<- 0
    init.data()
    RSS$skip[c("calculate.scores", "classify.samples", "update.stats")] <- c(TRUE, TRUE, TRUE)
    RSS$scans.to.learn <- 0
    rss.enable.hook("ONPLAY", name)
    rss.enable.hook("ONSTOP", name)
    TRUE
  },

  shut.down.genblips = function(port, ...) {
    ## re-enable the blipping controls
    rss.gui(ENABLE_CONTROLS, "blip.finding", TRUE)
    rss.disable.hook("ONPLAY", name)
    rss.disable.hook("ONSTOP", name)
    RSS$skip[c("calculate.scores", "classify.samples", "update.stats")] <- c(FALSE, FALSE, FALSE)
  },

  new.play.state.genblips = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }
  )  ## end of globals

init.data <- function() {
  max.range <<- scan.info$first.sample.dist + scan.info$sample.dist * scan.info$samples.per.pulse
  init.time <<- as.numeric(Sys.time())
  targets <<- data.frame(scan.no=integer(),id=integer(), age=integer(), timestamp=double(),
                         x=double(), y=double(), z=double(),
                         vx=double(), vy=double(), vz=double(),
                         ax=double(), ay=double(), az=double(),
                         range=double(), azi=double(), speed=double(), elev=double(),
                         rcs=double(), visible=logical(),
                         x.rad=double(), y.rad=double(), z.rad=double())
  max.target.id <<- 0
  reset.blip.deltas()
}

hooks = list(

  SCAN_INFO = list (enabled = TRUE, read.only = FALSE,
    f = function(si) {
      ## assign to the global scan.info object,
      ## because get.scan.data.genblips needs an existing RSS$scan.info
      RSS$scan.info <- si
    }),

  ONPLAY = list( enabled = FALSE, read.only = FALSE,
    f = function(...) {
      RSS$scans.to.learn <- 0
    }),

  ONSTOP = list( enabled = FALSE, read.only = FALSE,
    f = function(...) {
      scan.num <<- 0
      zero(RSS$scan.mat)
      zero(RSS$class.mat)
      init.data()
      if (!is.null(csv.file)) {
        close(csv.file)
        csv.file <<- NULL
      }
    }),

  DONE_SCAN = list (enabled = TRUE, read.only = TRUE,
    f = function(...) {
      if (RSS$previewing || !RSS$have.valid$classification)
        return()
      if (generate.csv.file) {
        if (is.null(csv.file)) {
          csv.file <<- file(csv.filename, "w")
          cat(c("scan.no,track.no,blip.no,timestamp,x,y,z,vx,vy,vz,ax,ay,az,range,azi,speed,elev,rcs,visible,xrad,yrad,zrad"), "\n", file=csv.file)
        }
        if (only.export.visible) {
          if (sum(targets$visible))
            write.table(subset(targets, visible), col.names=FALSE, row.names=FALSE, file=csv.file, append=TRUE, sep=",")
        } else {
          if (nrow(targets)) 
            write.table(targets, col.names=FALSE, row.names=FALSE, file=csv.file, append=TRUE, sep=",")
        }
      }
    })

  ) ## end of hooks list

## additional state variables for this plugin

csv.file = NULL  ## connection for CSV output
max.range = 0
scan.num = 0  ## index of current scan
init.time = NULL ## time at which this run started
max.target.id = 0 ## max id of targets seen so far
blip.deltas = NULL ## linear offsets for coordinates of blips when painting in scan and class matrices

targets = NULL ## target data frame

      
