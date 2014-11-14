##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2014 John Brzustowski
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


##           PNGARCH   PLUGIN
##                                                         
##  Read scans from a folder of PNG files.

MYCLASS = "pngarch"

about = function() {
  return(plugin.label)
}

get.ports = function() {

  rv <- list()
  
  make.port <- function(name, id, is.source, is.sink) {
    structure(strictenv(
                        name = name,
                        id = id,
                        is.source = is.source,
                        is.sink = is.sink,
                        is.live = FALSE,
                        is.file = TRUE,
                        is.seekable = TRUE,
                        file.ext = file.extensions,
                        can.specify.start.time = TRUE,
                        config = c(default, filename = NULL),
                        has.toc = TRUE,
                        cur.run = 0,
                        cur.scan = 0,
                        prev.scan = -1,
                        scan.data = NULL,                  # the data, pre-read by get.scan.info
                        contents = empty.TOC,
                        start.time = NULL,                 # the time of the first frame
                        duration = 0L,                     # how long the PNG folder lasts, in seconds
                        si = NULL                          # scan info
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- my.port <<- make.port("Reader", 1,  TRUE, FALSE)
  rv
}

load = function() {
    library(png)
    rss.dyn.load("ocr", in.dir.of=plugin.file)
}

unload = function(save.config) {
}

get.menus = function() {
  list(
       sources = list (
         titles = "PNG Image reader",
         menu = list (
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         ),
       plugin = list (
##         "Save geometry and frame rate parameters for this video in XXX.metadata.R..." = save.metafile,
##         "Load geometry and frame rate parameters for this video from another video's metdata.R file..." = load.metafile,
##         "---",
         list ("gauge",
               label = "image centre x coordinate offset, in image pixels",
               range = c(-10000, 10000),
               increment = 1,
               value = default$origin[1],
               on.set = function(x) { default$origin[1] <<- x
                                      if (inherits(RSS$source, MYCLASS)) {
                                        config(RSS$source, origin=default$origin)
                                        update()
                                      }
                                    }
               ),
         list ("gauge",
               label = "image centre y coordinate offset, in image pixels",
               range = c(-10000, 10000),
               increment = 1,
               value = default$origin[2],
               on.set = function(x) { default$origin[2] <<- x
                                      if (inherits(RSS$source, MYCLASS)) {
                                        config(RSS$source, origin=default$origin) 
                                        update()
                                      }
                                    }
               ),
         list ("gauge",
               label = "radius of image, in pixels",
               range = c(0, 10000),
               increment = 1,
               value = default$radius,
               on.set = function(x) { default$radius <<- x
                                      if (inherits(RSS$source, MYCLASS)) {
                                        config(RSS$source, scale=default$radius)
                                        update()
                                      }
                                    }
               ),
         list ("gauge",
               label = "maximum range shown, in km",
               range = c(0, 20),
               increment = 0.01,
               value = default$max.range,
               on.set = function(x) { default$max.range <<- x
                                      if (inherits(RSS$source, MYCLASS)) {
                                        config(RSS$source, max.range=default$max.range)
                                        update()
                                      }
                                    }
               ),
         list ("gauge",
               label = "rotation of coordinates, in degrees clockwise",
               range = c(-360, 360),
               increment = 0.1,
               value = default$rotation,
               on.set = function(x) { default$rotation <<- x
                                      if (inherits(RSS$source, MYCLASS)) {
                                        config(RSS$source, rotation=default$rotation)
                                        GUI$north.angle <- -default$rotation
                                        update()
                                      }
                                    }
               )
         
         
         )
       )
}

globals = list (
  
  as.character.pngarch = function(x, ...) {
    sprintf("radR interface port: %s: %s: %s",
            MYCLASS,
            x$name,
            if (is.null(x$config$filename)) "(no file)" else x$config$filename
            )
  },
  
  print.pngarch = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.pngarch = function(port, ...) {
    
    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 port$contents <- empty.TOC  ## mark the table of contents as needing regeneration
                 port$config$firstScan = get.seqno(port$config$filename)
                 port$config$template = get.template(port$config$filename)
               },
               origin = {
                 port$config$origin <- opts[[opt]]
               },
               radius = {
                 port$config$radius <- opts[[opt]]
                 port$config$scale <- port$config$max.range * 1000 / port$config$radius ## metres per pixel
               },
               max.range = {
                 port$config$max.range <- opts[[opt]]
                 port$config$scale <- port$config$max.range * 1000 / port$config$radius ## metres per pixel
               },
               rotation = {
                 port$config$rotation <- opts[[opt]]
               },
               {
                 rss.plugin.error("PNG: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },
  
  get.contents.pngarch = function(port, ...) {
    return(port$contents)
  },
  
  end.of.data.pngarch = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    port$cur.scan >= port$contents$num.scans[port$cur.run]
  },

  get.scan.info.pngarch = function(port, ...) {
    ## gets the header information for the next scan

    ## read the PNG frame, skipping any missing ones, into port$scan.data

    repeat {
        ## bump up the scan counter
        port$cur.scan = port$cur.scan + 1
        x = read.scan.file(port, port$cur.scan, asNative = FALSE)[,,1]
        port$scan.data = as.integer(32767 * x)
        dims = dim(x)
        if (!is.null(port$scan.data)) break
        if (end.of.data(port))
            return (NULL)
    }

    port$si <- list(pulses = dims[2],
                    
                    samples.per.pulse = dims[1],
                    
                    bits.per.sample = 15, ## even though the gray channel is only 8 bits, we shift up to improve stats
                    
                    timestamp = structure(round(as.numeric(port$start.time) + (port$cur.scan - 1) * port$duration), class=c("POSIXct", "POSIXt")),
                    
                    duration = port$duration,

                    sample.dist = port$config$scale,
                    
                    first.sample.dist = 0,
                    
                    bearing = 0,
                    
                    orientation = +1,

                    is.rectangular = TRUE,

                    origin = port$config$origin
                    
                    )

    return(port$si)
  },
    
  get.scan.data.pngarch = function(port, extmat, ...) {
    ## copies the data for the current scan into the extmat
    ## the data have already been read by get.scan.info

    if (is.null(port$scan.data))
      return (NULL)
    dim <- c(port$si$samples.per.pulse, port$si$pulses)
    
    if (is.null(dim))
      stop("calling get.scan.data when RSS$scan.info has NULL dimension info")
    
    dim(extmat) <- dim
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim

    extmat[1:prod(dim)] <- port$scan.data
    return(extmat)
  },

  seek.scan.pngarch = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run; but we only allow one run per png archive
    if (scan == port$cur.scan + 1)
      return()
    port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
  },

  seek.time.pngarch = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)

    ## FIXME: do this

    return (NULL)
  },


  start.up.pngarch = function(port, ...) {
    ## determine how many sweeps are in the archive at the current
    ## frame interval
    
    ## The contents will be treated as a single run.
    ##
    ## Returns a one-row dataframe with three elements:
    ## num.scans:   number of scans (frames) in the run
    ## start.time:  timestamp first scan
    ## end.time:    timestamp of last scan

    f = port$config$filename
    fs = port$config$firstScan
    
    files = dir(dirname(f), pattern=".*.(png|PNG)$", full.names=TRUE)
    files = files[get.seqno(files) >= fs]
    num.scans = length(files)

    ## image timestamps are rounded (truncated?) to the nearest second
    ## we need to estimate duration, and actual starting timestamp so that we can
    ## match the image timestamps throughout, to make it easy for users to compare
    ## radR's sweep at time T to the image for time T
    
    ts = as.numeric(c(get.image.timestamp(port, read.scan.file(port, 1)), get.image.timestamp(port, read.scan.file(port, num.scans))))

    ## unbiased duration estimate, for a reasonable number of sweeps
    duration = diff(as.numeric(ts)) / (num.scans - 1)

    ## given the duration estimate and number of intervening sweeps, how many fractional
    ## seconds do we have to alter the first timestamp by to get the correct value for
    ## the rounded final timestamp?  Getting this right should make timestamps align
    ## more or less correctly throughout the sequence.

    i = 0
    repeat {
      tsEnd = round(ts[1] + duration * (num.scans - 1))
      if (tsEnd == ts[2])
        break
      if (tsEnd < ts[2]) {
        ts[1] = ts[1] + 0.1
        if (i > 10)
          break
      } else {
        ts[1] = ts[1] - 0.1
        if (i > 10)
          break
      }
      i = i + 1
    }
    class(ts) = class(Sys.time())

    gui.set.coord.tx(plot.to.matrix = tx.plot.to.matrix, plot.to.spatial = tx.plot.to.spatial, matrix.to.spatial = tx.matrix.to.spatial)
    rss.enable.hook("PATCH_STATS", MYCLASS)

    
    port$duration <- duration
    port$start.time <- ts[1]
##    gui.png.start.time(round(port$start.time))
    port$contents <- list (
                          num.scans = num.scans,
                          start.time = structure(as.numeric(port$start.time), class="POSIXct"),
                          end.time = structure(as.numeric(port$start.time + num.scans * port$duration), class="POSIXct")
                           )

    port$cur.run <- 1
    port$scan.data <- NULL
    return (TRUE)
  },

  shut.down.pngarch = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.
    rss.disable.hook("PATCH_STATS", MYCLASS)

##    GUI$plot.title.date.format <- old.plot.title.date.format
    ## restore old coord tx
    gui.set.coord.tx()
    ## drop the current file data
    port$start.time <- NULL
    port$scan.data <- NULL
    return(TRUE)
  },

  new.play.state.pngarch = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.
      return(NULL)
  }
  
  )  ## end of globals

hooks = list (
  PATCH_STATS = list (enabled = FALSE, read.only = 2,
    f = function(keep) {
      ## adjust patch coordinates to take into account image rotation
      ## and origin offset; the patch coordinates have been calculated
      ## simply as matrix coordinates, so we convert them to spatial
      ## This hook function must be called before any other hook function
      ## that might use the x, y, and range patch properties, so we
      ## kludge this with read.only=2 to force this hook first.
      if (length(keep) > 0) {
        new.coords <- tx.matrix.to.spatial(cbind(RSS$patches$x[], RSS$patches$y[]))
        RSS$patches$x[]<- new.coords[,1]
        RSS$patches$y[]<- new.coords[,2]
        RSS$patches$range[] <- sqrt(RSS$patches$x[]^2 + RSS$patches$y[]^2)
      }
      return(keep)
    }
    )
  )

## return a suitable metafile name for an archive specified by the name of a data file
## get.metafile.name = function(datafilename) {
##   file.path(dirname(datafilename), "_metadata.R")
## }

read.scan.file = function(port, n, asNative=TRUE) {
  ## read the n'th scan
  ## returns a nativeRaster with the n'th image,
  ## where n is the image selected by the user

  f = sprintf(port$config$template, port$config$firstScan + n - 1)
  if (! file.exists(f))
      return (NULL)
  
  rv = readPNG(f, native=asNative)
  ## swap data storage orientation, but retain current dimensions
  ## first dimension is raster width, second is height
  ## we allow for a possible 3rd dimension for channels
  ## (when asNative is FALSE) which we leave alone\
  if (asNative) {
      ## swap dimensions without changing data order
      dim(rv)[1:2] = dim(rv)[2:1]
      return(rv)
  } else {
      ## swap dimensions *and* change data order!
      nrv = array(0, dim=dim(rv)[c(2, 1, 3)])
      for (i in 1:dim(rv)[3])
          nrv[,,i] = t(rv[,,i])
      return(nrv)
  }
}
  
get.image.timestamp = function(port, img) {
  ## img is a nativeRaster
    tsi = img[port$config$tsbox.h, port$config$tsbox.v]
    ts = strptime(
        .Call(
            "ocr",
            tsi,
            as.integer(
                c(length(port$config$tsbox.v), length(port$config$tsbox.h),
                  1,  ## invert - timestamps are white on background, not black on white
                  3,  ## scale - required for proper recognition
                  2,  ## threshold 
                  port$config$date.ocr.bitmask)))[[1]],
        port$config$date.ocr.format)
    return (as.POSIXct(ts))
}

update = function() {
  if (RSS$play.state < RSS$PS$PLAYING) {
    RSS$scan.info$sample.dist <- my.port$config$scale
    RSS$scan.info$origin <- my.port$config$origin
    rss.process.scan(put.scan = FALSE,
                     calculate.scores = FALSE,
                     convert.scan = TRUE,
                     is.preview = TRUE)
  }
}

## coordinate transform functions for rectangular PNG coordinates

tx.plot.to.matrix <- function(coords) {
  ## convert xy coordinates in the plot window
  ## to row/column coordinates in the raw data
  ## in origin 1
  ## coords: an n x 2 matrix (or a vector of length 2)
  ## returns: an n x 2 matrix (or a vector of length 2)

  if (is.matrix(coords))
    offsets <- t(t(coords) - GUI$plot.origin)
  else
    offsets <- t(coords - GUI$plot.origin)
  sample <- 1 + trunc(RSS$scan.info$samples.per.pulse / 2 + offsets[,1] / gui.pps() - RSS$scan.info$origin[1])
  pulse <-  1 + trunc(RSS$scan.info$pulses / 2 + offsets[,2] / gui.pps() + RSS$scan.info$origin[2])
  rv <- cbind(ifelse(sample >= 1 & sample <= RSS$scan.info$samples.per.pulse, sample, NA), ifelse(pulse >= 1 & pulse <= RSS$scan.info$pulses, pulse, NA))
  return (if (is.matrix(coords)) rv else c(rv))
}

tx.plot.to.spatial <- function(coords) {
  ## Basically the same as for radar data, but drop z coordinate and use constant time

  ## given plot coordinates coords (pixels in the x and y directions)
  ## return a two element list:
  ## rv$rb: a ground range, axial range, bearing vector (metres, metres, degrees)
  ## rv$xyz: cartesian coordinates, in metres
  ## rv$t:   time, the time corresponding to the location's pulse
  ##         if there is no sample data, the time returned is NA
  ## range is planar; range per sample is measured along the antenna axis,
  ## so we must adjust range according to the antenna angle
  offsets <- coords - GUI$plot.origin
  x <- offsets[c(TRUE, FALSE)] * GUI$mpp  ## in planar metres
  y <- - offsets[c(FALSE, TRUE)] * GUI$mpp
  ground.range <- sqrt(x^2+y^2)
  z <- 0
  axial.range <- ground.range
  bearing <- (atan2(offsets[c(TRUE, FALSE)], -offsets[c(FALSE, TRUE)]) * 180 / pi + RSS$source$config$rotation) %% 360
  theta <- (90 - bearing) * pi / 180
  if (RSS$have.valid$scan.data) {
    time <- as.numeric(RSS$scan.info$timestamp)
  } else {
    time <- NA
  }
  return(list(rb=c(ground.range, axial.range, bearing), xyz=c(ground.range * cos(theta), ground.range * sin(theta), z), t=time))
}

tx.matrix.to.spatial <- function(coords) {

  ## coords: an n x 2 matrix of data coordinates (sample, pulse)
  ##         (need not be integers)
  ##
  ## Value: an n x 3 matrix of spatial coordinates given the
  ## current origin, scale, and bearing offset

  x <- (coords[,1] - RSS$scan.info$samples.per.pulse / 2 + RSS$scan.info$origin[1]) * RSS$scan.info$sample.dist
  y <- (RSS$scan.info$pulses / 2 - coords[,2] + RSS$scan.info$origin[2]) * RSS$scan.info$sample.dist
  r <- sqrt(x^2+y^2)
  th <- atan2(y, x) - RSS$source$config$rotation * pi/180
  return (cbind(r * cos(th), r * sin(th), 0))
}

## save metadata for this archive

## save.metafile = function() {
##   if (!inherits(RSS$source, MYCLASS)) {
##     rss.gui(POPUP_MESSAGEBOX, "Select a video file first", "The current source is not a video file, so I can't save its metadata.")
##     return()
##   }
##   mf <- get.metafile.name(RSS$source$config$filename)
##   cat (sprintf("## meta data for archive %s\n\n## timestamp of first sweep (GMT)\nstart.time = %.3f\n\n## duration of sweeps (milliseconds)\nduration = %.3f\n", dirname(RSS$source$config$filename), as.numeric(default.start.time), 60 / antenna.rpm * 1000), file=mf)
##   rss.gui(POPUP_MESSAGEBOX, "Metadata saved", "Sweep start time and sweep duration were saved to file '_metadata.R'\nThey will be used each time this archive is opened, unless you delete the file.")
## }

## additional plugin variables

## what a table of contents looks like, initially
empty.TOC = list(
  start.time = structure(double(0), class = "POSIXct"),
  end.time = structure(double(0), class = "POSIXct"),
  num.scans = integer(0)
  )

## the one and only port
my.port = NULL

## a place to store the default (old) plot title date format,
## which we replace with our own
old.plot.title.date.format = NULL
