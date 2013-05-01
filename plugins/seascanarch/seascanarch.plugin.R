########################################################################
###                                                                  ###
### seascanarch.plugin.radR                                          ###
###                                                                  ###
### the seascanarch plugin for reading/writing SeaScan archive files ###
###                                                                  ###
### indexes have origin 1 in R, origin 0 in C                        ###
###                                                                  ###
### By John Brzustowski, 2006  jbrzusto AT fastmail DOT fm           ###
########################################################################


## define the name of this interface plugin's class
## this will be the class of its ports
MYCLASS = "seascanarch"

about = function() {
  return("Interface for reading/writing Seascan archive files.")
}

get.menus = function() {
  list(
       plugin = list(
         list(option="choose.any",
              on.set = function(n, s) {
                if (n == 1) {
                  skip.changeover.pulse <<- s
                  if (inherits(RSS$source, MYCLASS))
                    config(RSS$source, skip.changeover.pulse=s)
                } else if (n == 2) {
                  use.pc.timestamp <<- s
                  if (inherits(RSS$source, MYCLASS))
                    config(RSS$source, use.pc.timestamp=s)
                }
              },
              "Skip angle changeover pulses in ungated data (removes black radial streaks)" = skip.changeover.pulse,
              "Use timestamp from recording PC instead of GPS (close then re-open source after changing)" = use.pc.timestamp
              )
         ),

       sources = list(
         titles = "Seascan archive reader",
         menu = list(
           options = "no-tearoff",
            "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         )
       ##  UNCOMMENT IF/WHEN SEASCAN ARCHIVE WRITING IS IMPLEMENTED
       ##            sinks = list(
       ##                     titles = "Seascan archive writer",
       ##                     menu = list(
       ##                         "Choose a file..." = gui.create.port.file.selector(get.ports()[[2]])
       ##                       )
       ##                     )
       )
}

load = function() {
  rss.dyn.load("seascanarch", in.dir.of=plugin.file, local=TRUE)
}

unload = function(save.config) {
  rss.dyn.unload("seascanarch")
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
                        can.specify.start.time = TRUE,
                        config = list(filename = NULL),
                        has.toc = TRUE,
                        is.gated = TRUE,
                        file.ext = "dat"
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- make.port("Reader", 1,  TRUE, FALSE)
  ##  rv[[2]] <- make.port("Writer", 2, FALSE,  TRUE)
  rv
}

## global methods and variables

globals = list (
  
  as.character.seascanarch = function(x, ...) {
    running <- .Call("is_started", as.integer(x$id - 1), PACKAGE=MYCLASS)
    sprintf("radR interface port: %s: %s: %s: %s",
            MYCLASS,
            x$name,
            ifelse (is.null(x$config$filename),
                    "(no file)",
                    x$config$filename),
            ifelse(running,
                   "Open",
                   "Closed")
            )
  },
  
  print.seascanarch = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.seascanarch = function(port, ...) {
    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 if (is.null(.Call("set_filename", as.integer(port$id - 1), port$config$filename, PACKAGE=MYCLASS)))
                   return(NULL)
               },
               desired.azimuths = {
                 port$config$desired.azimuths <- as.integer(opts[[opt]])
                 if (is.null(.Call("set_desired_azimuths", as.integer(port$id - 1), as.integer(port$config$desired.azimuths), PACKAGE=MYCLASS)))
                   return(NULL)
               },
               max.azimuth.err = {
                 port$config$max.azimuth.err <- as.real(opts[[opt]])
                 if (is.null(.Call("set_max_azimuth_err", as.integer(port$id - 1), as.real(port$config$max.azimuth.err), PACKAGE=MYCLASS)))
                   return(NULL)
               },
               skip.changeover.pulse = {
                 port$config$skip.changeover.pulse <- as.integer(opts[[opt]])
                 if (is.null(.Call("set_skip_changeover_pulse", as.integer(port$id - 1), as.integer(port$config$skip.changeover.pulse), PACKAGE=MYCLASS)))
                   return(NULL)
               },
               use.pc.timestamp = {
                 port$config$use.pc.timestamp <- as.integer(opts[[opt]])
                 if (is.null(.Call("set_use_pc_timestamp", as.integer(port$id - 1), as.integer(port$config$use.pc.timestamp), PACKAGE=MYCLASS)))
                   return(NULL)
               },
               {
                 rss.plugin.error("seascanarch: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

  get.contents.seascanarch = function(port, ...) {
    ## gets the contents of the port, namely
    ## a list with three elements
    ## num.images: number of images in each run
    ## run.start:  starting timestamp of each run
    ## run.end:    ending timestamp of each run

#    if (!port$has.toc)
#      rss.plugin.error("seascanarch:archive is ungated and does not have a table of contents")
    if (!is.null(x <- .Call("get_contents", as.integer(port$id - 1), PACKAGE=MYCLASS))) {
      num.runs <- length(x) / 3
      rv <- list(num.scans=  x[seq(along=x) %% 3 == 1],
                 start.time= x[seq(along=x) %% 3 == 2],
                 end.time=   x[seq(along=x) %% 3 == 0])
      class(rv$start.time) <- class(rv$end.time) <- "POSIXct"
      return(rv)
    }
    NULL
  },

  end.of.data.seascanarch = function(port, ...) {
    ## have we hit the end of data for this archive?
    .Call("end_of_data", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },
  
  get.scan.info.seascanarch = function(port, ...) {
    ## gets the header information for the most recently processed scan

    if (!is.null(x <- .Call("get_scan_info", as.integer(port$id - 1), PACKAGE=MYCLASS))) {
      ## duplicate element 4 for use as a timestamp and a fractional second offset
      names(x) <- RSS$default.scan.info.names
      class(x[[4]]) <- "POSIXct"
    }
    return(x)
  },

  get.scan.data.seascanarch = function(port, extmat, ...) {
    ## gets the data for the most recently processed scan into extmat
    ## and returns extmat
    dur <- .Call("get_scan_data", as.integer(port$id - 1), extmat,
                PACKAGE=MYCLASS)
    if (is.null(dur))
      return(NULL)
    RSS$scan.info$duration <- dur
    return(extmat)
  },

  put.scan.seascanarch = function(port, header, extmat, ...) {
    ## puts most recently read scan (including header information)
    ## to the current sink
    .Call("put_scan_data", as.integer(port$unit$id - 1), header, extmat)
  },

  seek.scan.seascanarch = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the port
    ## scan = which run, or -1 for the current scan
    ## run = which run, or -1 for the current run

    .Call("seek_scan", as.integer(port$id - 1), as.integer(run - 1), as.integer(scan - 1), PACKAGE=MYCLASS)
  },

  seek.time.seascanarch = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = NULL represents the current run

    .Call("seek_time", as.integer(port$id - 1), as.integer(time), PACKAGE=MYCLASS)
  },

  start.up.seascanarch = function(port, ...) {
    ## start up this port (open the archive)
    ## ssa_start_up returns TRUE for gated data, FALSE for ungated
    rv <- .Call("start_up", as.integer(port$id - 1), PACKAGE=MYCLASS)
    if (!is.null(rv)) {
      ## set flags that depend on whether the data is gated
      port$is.gated <- rv
      ## port$is.seekable <- rv
      ## port$has.toc <- rv
      port$is.seekable <- TRUE
      port$has.toc <- TRUE
      config(port, use.pc.timestamp = use.pc.timestamp)
      if (!port$is.gated)
        config(port, desired.azimuths = desired.azimuths, max.azimuth.err = max.azimuth.err, skip.changeover.pulse = skip.changeover.pulse)
      return(TRUE)
    }
    return(NULL)
  },

  shut.down.seascanarch = function(port, ...) {
    ## shut down this port (flush buffers and close the archive)
    .Call("shut_down", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },

  start.run.seascanarch = function(port, start.time, ...) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls
    .Call("start_run", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },

  end.run.seascanarch = function(port, end.time, ...) {
    ## end the current sink run and make any
    ## required changes to its directory
    .Call("end_run", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },

  new.play.state.seascanarch = function(port, new.state, old.state, ...) {
    ## if we are "stopping" playback of an ungated archive
    ## then shut down and restart the port, so it can
    ## be played back again
    if (! port$is.gated && new.state == RSS$PS$STOPPED && port$is.source) {
      shut.down(port)
      start.up(port)
    }
  }

  
  ) ## end of globals

hooks = list(
  
  ) ## end of hooks


