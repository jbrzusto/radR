########################################################################
###                                                                  ###
### blipmoviearch.plugin.radR                                        ###
###                                                                  ###
### the blipmoviearch plugin for reading/writing blipmovies          ###
###                                                                  ###
### indexes have origin 1 in R, origin 0 in C                        ###
###                                                                  ###
### By John Brzustowski, 2006  jbrzusto AT fastmail DOT fm           ###
########################################################################

## define the name of this interface plugin's class
## this will be the class of its ports
MYCLASS = "blipmoviearch"

about = function() {
  return("Interface for reading/writing blipmovie archive files (original version).")
}

load = function() {
  rss.dyn.load("blipmoviearch", in.dir.of = plugin.file, local=TRUE)
}

unload = function(save.config) {
  rss.dyn.unload("blipmoviearch")
}

## no plugin menu so far

get.menus = function() {
  list(
       sources = list(
         titles = "Blipmovie archive reader",
         menu = list(
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         ),

       sinks = list(
         titles = "Blipmovie archive writer",
         menu = list(
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[2]])
           )
         )
       )
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
                        file.ext = "bma",
                        movie.basename = "bmovie"
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- make.port("Reader", 1,  TRUE, FALSE)
  rv[[2]] <- make.port("Writer", 2, FALSE,  TRUE)
  rv
}

## global methods and variables

globals = list (
  
  as.character.blipmoviearch = function(x, ...) {
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
  
  print.blipmoviearch = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.blipmoviearch = function(port, ...) {
    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 port$movie.basename <- strsplit(opts$filename, "\\.[a-zA-Z]*$")[[1]]
                 if (is.null(.Call("set_filename", as.integer(port$id - 1), port$movie.basename, PACKAGE=MYCLASS)))
                   return(NULL)
               },
               {
                 rss.plugin.error("blipmoviearch: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

  get.contents.blipmoviearch = function(port, ...) {
    ## gets the contents of the port, namely
    ## a list with three elements
    ## num.scans: number of scans in each run
    ## run.start:  starting timestamp of each run
    ## run.end:    ending timestamp of each run

    
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

  end.of.data.blipmoviearch = function(port, ...) {
    ## have we hit the end of blipmovie data?
    ## return TRUE on end of data or on errors
    rv <- .Call("end_of_data", as.integer(port$id - 1), PACKAGE=MYCLASS)
    if (is.null(rv)) {
      rv <- TRUE
    }
    rv
  },
  
  get.scan.info.blipmoviearch = function(port, ...) {
    ## gets the header information for the most recently processed scan
    
    if (!is.null(x <- .Call("get_scan_info", as.integer(port$id - 1), PACKAGE=MYCLASS))) {
      ## duplicate element 4 for use as a timestamp and a fractional second offset
      x <- as.list(x)
      names(x) <- scan.info.names
      class(x[[4]]) <- "POSIXct"
    }
    x
  },

  get.scan.data.blipmoviearch = function(port, scanmat, ...) {
    ## paint the blips from the blip movie into the scan matrix
    ## and the class matrix

    ## gets the data for the most recently processed scan
    ## it is an extmat object, so make that class here
    rv <- .Call("get_scan_data", as.integer(port$id - 1), scanmat, RSS$class.mat, RSS$patch.buffer, RSS$score.mat,
                PACKAGE=MYCLASS)
    if (is.null(rv)) {
      RSS$num.blips <- 0
      RSS$num.hot.samples <- 0
      return(NULL)
    }
    RSS$num.blips <- rv[1]
    RSS$num.hot.samples <- rv[2]
    RSS$new.have.valid$stats <- FALSE
    RSS$new.have.valid$scores <- .Call("have_scores", as.integer(port$id - 1))
    RSS$new.have.valid$classification <- TRUE
    return(scanmat)
  },

  put.scan.blipmoviearch = function(port, header, scanmat, ...) {

    ## compute the floating-point timestamp
    .Call("put_scan",
          as.integer(port$id - 1),
          scanmat,
          RSS$patch.buffer,
          as.double(header[c("timestamp",
                             "duration",
                             "pulses",
                             "samples.per.pulse",
                             "orientation",
                             "bits.per.sample",
                             "sample.dist",
                             "first.sample.dist",
                             "bearing")]),
          as.integer(RSS$num.blips),
          RSS$score.mat,
          PACKAGE=MYCLASS)
  },

  seek.scan.blipmoviearch = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the port
    ## scan = which run, or -1 for the current scan
    ## run = which run, or -1 for the current run; we ignore this value here
    ## since we treat the archive as having one run

    .Call("seek_scan", as.integer(port$id - 1), as.integer(run - 1), as.integer(scan - 1), PACKAGE=MYCLASS)
  },

  seek.time.blipmoviearch = function(port, time, ...) {
    ## seek to the first scan at or after time

    .Call("seek_time", as.integer(port$id - 1), as.integer(time), PACKAGE=MYCLASS)
  },

  start.up.blipmoviearch = function(port, ...) {
    ## start up the blipmovie port by opening files
    if (!is.null(.Call("start_up", as.integer(port$id - 1), PACKAGE=MYCLASS))) {
      switch(port$id,
             "1" = {
               ## disable the blip finding controls
               rss.gui(ENABLE_CONTROLS, "blip.finding", FALSE)
               ## make it look like we are blip finding
               RSS$blip.finding <- TRUE
               ## To indicate we are not learning, set the number of scans to learn to zero.
               RSS$scans.to.learn <- 0
               ## tell the scan processor to skip the sample classifying 
               ## steps, since we are reading pre-digested data
               save.restart.learning <<- RSS$restart.learning.after.stop
               RSS$restart.learning.after.stop <- FALSE
               RSS$skip$classify.samples <- TRUE
               RSS$skip$update.stats <- TRUE
               ## add a patch finding hook, which replaces the builtin patch finder
               rss.add.hook("FIND_PATCHES", MYCLASS,
                            list(f=find.patches.blipmoviearch, enabled=TRUE, read.only=FALSE))
             },
             "2" = {
               ## the writer; anything to do here?
             })
      return(TRUE);
    }
    return (FALSE)
  },

  shut.down.blipmoviearch = function(port, ...) {
    ## shut down the blipmovie port by flushing buffers and closing files

    .Call("shut_down", as.integer(port$id - 1), PACKAGE=MYCLASS)
    switch(port$id,
           "1" = {
             RSS$skip$classify.samples <- FALSE
             RSS$skip$update.stats <- FALSE
             RSS$scans.to.learn <- RSS$default.scans.to.learn
             if (!is.null(save.restart.learning))
               RSS$restart.learning.after.stop <- save.restart.learning 
             
             ## drop the patch finding hook
             rss.drop.hook("FIND_PATCHES", MYCLASS)
             ## re-enable the blipping controls
             rss.gui(ENABLE_CONTROLS, "blip.finding", TRUE)
           },
           "2" = {
             ## the writer; anything to do here?
           })
    return(TRUE)
  },

  start.run.blipmoviearch = function(port, start.time, ...) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls
    .Call("start_run", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },

  end.run.blipmoviearch = function(port, end.time, ...) {
    ## end the current sink run and make any
    ## required changes to its directory
    .Call("end_run", as.integer(port$id - 1), PACKAGE=MYCLASS)
  },

  find.patches.blipmoviearch = function(...) {
    ## do nothing, since get.scan.data has already constructed
    ## the patches.
    ## This hook function simply prevents the usual patch
    ## finder from being called
  },

  new.play.state.blipmoviearch = function(port, new.state, old.state, ...) {
    ## Nothing to be done here.
  }

  )   ## end of globals

hooks = list(
  
  ) ## end of hooks

## additional interface plugin variables

save.restart.learning = NULL

## (old) blipmovies have the following scan info items
scan.info.names = c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp",
  "duration", "sample.dist", "first.sample.dist",
  "bearing", "orientation")


