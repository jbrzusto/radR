##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2011 John Brzustowski
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


##           VIDEO   PLUGIN
##
##  Read images from a video file.  Any input format supported by ffmpeg
##  is allowed.

MYCLASS = "video"

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
            file.ext = video.extensions,
            can.specify.start.time = TRUE,
            config = list(
                filename = NULL,
                frame.rate=default.frame.rate,
                width=default.width,
                height=default.height,

                origin = default.origin,
                scale = default.scale,
                rotation = default.rotation,
                ffmpeg.options.before.input = default.ffmpeg.options.before.input,
                ffmpeg.options.before.output = default.ffmpeg.options.before.output
            ),

            has.toc = TRUE,
            cur.run = 0,
            cur.scan = 0,
            prev.scan = -1,
            scan.data = NULL,                  # the data, pre-read by get.scan.info
            contents = empty.TOC,
            start.time = NULL,                 # the time of the first frame
            duration = 0L,                     # how long the video file lasts, in seconds
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
  default.start.time <<- Sys.time()
}

unload = function(save.config) {
}

get.menus = function() {
    list(
        sources = list (
            titles = "Video reader",
            menu = list (
                options = "no-tearoff",
                "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
            )
        ),
        plugin = list (
            ##         "Save geometry and frame rate parameters for this video in XXX.metadata.R..." = save.metafile,
            ##         "Load geometry and frame rate parameters for this video from another video's metdata.R file..." = load.metafile,
            ##         "---",
            list ("datetime",
                  label = "date and time at start of first video frame",
                  value = as.numeric(default.start.time),
                  on.set = function(x) {
                      t <- structure(x, class="POSIXct")
                      default.start.time <<- t
                      if (inherits(RSS$source, MYCLASS))
                          RSS$source$start.time <- t
                  },
                  set.or.get = "gui.video.start.time"
                  ),
            list ("gauge",
                  label = "desired frame rate, in frames per seconds" ,
                  range = c(0.001, 1000),
                  increment = 1,
                  value = default.frame.rate,
                  on.set = function(x) { default.frame.rate <<- x
                      if (inherits(RSS$source, MYCLASS))
                          config(RSS$source, frame.rate=x)
                  }
                  ),
            list ("gauge",
                  label = "desired image width, in pixels",
                  range = c(100, 2000),
                  increment = 10,
                  value = default.width,
                  on.set = function(x) { default.width <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, width=x)
                          update()
                      }
                  }
                  ),
            list ("gauge",
                  label = "desired image height, in pixels",
                  range = c(100, 2000),
                  increment = 10,
                  value = default.height,
                  on.set = function(x) { default.height <<- x;
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, height=x)
                          update()
                      }
                  }
                  ),
            list ("gauge",
                  label = "image centre x coordinate offset, in image pixels",
                  range = c(-10000, 10000),
                  increment = 1,
                  value = default.origin[1],
                  on.set = function(x) { default.origin[1] <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, origin=default.origin)
                          update()
                      }
                  }
                  ),
            list ("gauge",
                  label = "image centre y coordinate offset, in image pixels",
                  range = c(-10000, 10000),
                  increment = 1,
                  value = default.origin[2],
                  on.set = function(x) { default.origin[2] <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, origin=default.origin)
                          update()
                      }
                  }
                  ),
            list ("gauge",
                  label = "scale of image, in metres per pixel",
                  range = c(0, 10000),
                  increment = 0.1,
                  value = default.scale,
                  on.set = function(x) { default.scale <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, scale=default.scale)
                          update()
                      }
                  }
                  ),
            list ("gauge",
                  label = "rotation of coordinates, in degrees clockwise",
                  range = c(-360, 360),
                  increment = 0.1,
                  value = default.rotation,
                  on.set = function(x) { default.rotation <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, rotation=default.rotation)
                          GUI$north.angle <- -default.rotation
                          update()
                      }
                  }
                  ),
            list ("string",
                  label = "extra ffmpeg options before '-i' (input) option",
                  width = 40,
                  height = 2,
                  value = default.ffmpeg.options.before.input,
                  val.check = function(x)TRUE,
                  on.set = function(x) {
                      default.ffmpeg.options.before.input <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, ffmpeg.options.before.input=x)
                      }
                  }
                  ),
            list ("string",
                  label = "extra ffmpeg options before 'pipe:1' (output) option",
                  width = 40,
                  height = 2,
                  value = default.ffmpeg.options.before.output,
                  val.check = function(x)TRUE,
                  on.set = function(x) {
                      default.ffmpeg.options.before.output <<- x
                      if (inherits(RSS$source, MYCLASS)) {
                          config(RSS$source, ffmpeg.options.before.output=x)
                      }
                  }
                  )
            )
    )
}

globals = list (

  as.character.video = function(x, ...) {
    sprintf("radR interface port: %s: %s: %s",
            MYCLASS,
            x$name,
            if (is.null(x$config$filename)) "(no file)" else x$config$filename
            )
  },

  print.video = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.video = function(port, ...) {

    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 port$contents <- empty.TOC  ## mark the table of contents as needing regeneration
               },
               width = {
                 port$config$width <- opts[[opt]]
               },
               height = {
                 port$config$height <- opts[[opt]]
               },
               frame.rate = {
                 port$config$frame.rate <- opts[[opt]]
               },
               scale = {
                 port$config$scale <- opts[[opt]]
               },
               origin = {
                 port$config$origin <- opts[[opt]]
               },
               rotation = {
                 port$config$rotation <- opts[[opt]]
               },
               ffmpeg.options.before.input = {
                   port$config$ffmpeg.options.before.input <- opts[[opt]]
               },
               ffmpeg.options.before.output = {
                   port$config$ffmpeg.options.before.output <- opts[[opt]]
               },
               {
                 rss.plugin.error("video: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

  get.contents.video = function(port, ...) {
    return(port$contents)
  },

  end.of.data.video = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    port$cur.scan >= port$contents$num.scans[port$cur.run]
  },

  get.scan.info.video = function(port, ...) {
    ## gets the header information for the next scan

    ## bump up the scan counter
    port$cur.scan <- port$cur.scan + 1

    ## make sure the video pipe is open
    if (is.null(video.pipe))
      open.video.at.cur.scan(port)

    ## to be safe, we verify that the next bit
    ## of the video pipe contains a proper header for a PGM file.

    hdr <- readLines(video.pipe, 3)

    if (length(hdr) != 3 || hdr[1] != "P5" || hdr[3] != "255")
      return (NULL)

    dims <- as.integer(strsplit(hdr[2], " ")[[1]])

    port$scan.data <- readBin(video.pipe, "integer", size=1, signed=FALSE, n=prod(dims)) * 128L
    port$si <- list(pulses = dims[2],

                    samples.per.pulse = dims[1],

                    bits.per.sample = 15, ## even though the gray channel is only 8 bits, we shift up to improve stats

                    timestamp = port$start.time + (port$cur.scan - 1) / port$config$frame.rate,

                    duration = 1000 / port$config$frame.rate,

                    ## FIXME: the rest of this needs rethinking for video data
                    ## In principal, each sample is a polyhedron extending to infinity,
                    ## or some kind of projective version of that.

                    sample.dist = port$config$scale,

                    first.sample.dist = 0,

                    bearing = 0,

                    orientation = +1,

                    is.rectangular = TRUE,

                    origin = port$config$origin,
                    image.rotation = port$config$rotation

                    )

    return(port$si)
  },

  get.scan.data.video = function(port, extmat, ...) {
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

    extmat[] <- port$scan.data
    return(extmat)
  },

  seek.scan.video = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run; but we only allow one run per video file
    if (scan == port$cur.scan + 1)
      return()
    port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
    open.video.at.cur.scan(port)
  },

  seek.time.video = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)

    ## FIXME: do this

    return (NULL)
  },

  start.up.video = function(port, ...) {
    ## determine how many frames are in the archive at the current
    ## frame interval

    ## The contents will be treated as a single run.
    ##
    ## Returns a one-row dataframe with three elements:
    ## num.scans:   number of scans (frames) in the run
    ## start.time:  timestamp first scan
    ## end.time:    timestamp of last scan

    old.plot.title.date.format <<- GUI$plot.title.date.format
    GUI$plot.title.date.format <- plot.title.date.format
    gui.set.coord.tx(plot.to.matrix = tx.plot.to.matrix, plot.to.spatial = tx.plot.to.spatial, matrix.to.spatial = tx.matrix.to.spatial)
    rss.enable.hook("PATCH_STATS", MYCLASS)
    fn = port$config$filename
    port$duration <- get.video.duration(fn)
    if (is.null(port$start.time)) {
      split <- regexpr(paste("(?=", date.guess.regexp, ")", sep=""), fn, perl=TRUE)
      if (split[1] != -1) {
        fn = sub("T", " ", fn)
        port$start.time <- as.POSIXct(strptime(substring(fn, split), date.guess.format))
      } else {
        port$start.time <- file.info(fn)$ctime
      }
      if (exists("gui.video.start.time"))
          gui.video.start.time(port$start.time)
    }
    ns = as.integer(floor(as.numeric(port$duration) * port$config$frame.rate))

    port$contents <- list (
                          num.scans = ns,
                          start.time = structure(as.numeric(port$start.time), class="POSIXct"),
                          end.time = structure(as.numeric(port$start.time + ns / port$config$frame.rate), class="POSIXct")
                           )

    port$cur.run <- 1
    port$scan.data <- NULL
    return (TRUE)
  },

  shut.down.video = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.
    rss.disable.hook("PATCH_STATS", MYCLASS)

    GUI$plot.title.date.format <- old.plot.title.date.format
    ## restore old coord tx
    gui.set.coord.tx()
    ## drop the current file data
    port$start.time <- NULL
    port$scan.data <- NULL
    if (!is.null(video.pipe))
      close(video.pipe)
    video.pipe <<- NULL
    return(TRUE)
  },

  new.play.state.video = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }

  )  ## end of globals

## return a suitable metafile name for an archive specified by the name of a data file
## get.metafile.name = function(datafilename) {
##   file.path(dirname(datafilename), "_metadata.R")
## }

## get the video duration in seconds
get.video.duration = function(f) {
  as.numeric(as.difftime(strsplit(grep("Duration", readLines(pipe(paste(ffmpeg.path, "-i", paste("\"", f, "\"", sep=""), "-vframes 0 2>&1"))), value=TRUE), ",")[[1]][1], format="  Duration: %H:%M:%OS", units="secs"))
}

open.video.at.cur.scan = function(port) {
  ## returns a pipe connection to an ffmpeg process which dumps
  ## video starting from the current scan.  This is only called
  ## at startup and with seek.scan

  if (!is.null(video.pipe))
    close(video.pipe)

  cmd <- paste(ffmpeg.path,
               "-ss", round((port$cur.scan - 1) / port$config$frame.rate,2), ## seek to frame
               "-flags gray",
               port$config$ffmpeg.options.before.input,
               "-i",
               paste("\"", port$config$filename, "\"", sep=""),
               "-r ", port$config$frame.rate,  ## output at desired frame rate
               "-f rawvideo", ## output format is sequence of raw frames
               "-vcodec pgm", ## output codec is portable gray map
               "-pix_fmt gray", ## output pixel format is grayscale (8-bit)
               "-s", paste(port$config$width, port$config$height, sep="x"), ## ask for size at current setting
               port$config$ffmpeg.options.before.output,
               "pipe:1",      ## output to standard output
               paste("2>", RSS$null.device, sep=""))  ## hide ffmpeg splash & info

  video.pipe <<- pipe(cmd, "rb")
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

## coordinate transform functions for rectangular video coordinates

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

## what a video table of contents looks like, initially
empty.TOC = list(
  start.time = structure(double(0), class = "POSIXct"),
  end.time = structure(double(0), class = "POSIXct"),
  num.scans = integer(0)
  )

## pipe connection from which we read raw video frames
video.pipe = NULL

## the one and only port
my.port = NULL

## a place to store the default (old) plot title date format,
## which we replace with our own
old.plot.title.date.format = NULL
