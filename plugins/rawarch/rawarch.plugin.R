##  svn $Id: rawarch.plugin.R 751 2011-03-07 18:38:01Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006, 2007, 2008 John Brzustowski        
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
###########################################################################
###                                                                     ###
### rawarch.plugin.R                                                    ###
###                                                                     ###
### A simple plugin for reading/writing raw data in native R format.    ###
### This provides a reader port and a writer port.                      ###
###                                                                     ###
###########################################################################

MYCLASS="rawarch"

about = function() {
  return("This simple plugin provides ports for reading and writing raw data in native R format.")
}

get.ports = function() {

  rv <- list()
  
  make.port <- function(name, id, is.source, is.sink) {
    structure(strictenv(
                        name = name,
                        id = id,
                        is.source = is.source,
                        is.sink = is.sink,
                        is.file = TRUE,
                        is.live = FALSE,
                        is.seekable = TRUE,
                        can.specify.start.time = TRUE,
                        config = list(filename = NULL),
                        file.ext = "raw.biglist",
                        has.toc = TRUE,
                        cur.run = 0,
                        cur.scan = 0,
                        contents = NULL,
                        first.scan = NULL,
                        file.basename = NULL,
                        bl = NULL,
                        si = NULL
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- make.port("Reader", 1,  TRUE, FALSE)
  rv[[2]] <- make.port("Writer", 2, FALSE,  TRUE)
  rv
}

load = function() {
  rss.dyn.load("rawarch", in.dir.of=plugin.file, local=TRUE)
}

unload = function(save.config) {
  rss.dyn.unload("rawarch")
}


get.menus = function() {
  list(
       sources = list(
         titles = "Raw archive reader",
         menu = list(
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         ),

       sinks = list(
         titles = "Raw archive writer",
         menu = list(
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[2]])
           )
         ),

       plugin = list(
         "compress" = 
          list(option="choose.any",
               on.set = function(n, v) {
                 compress <<- v
               },
               set.or.get = ".compress",
               "compress data when writing archive" = compress
               )
          )
       )
}


globals = list (
  as.character.rawarch = function(x, ...) {
    running <- !is.null(x$bl)
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
  
  print.rawarch = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.rawarch = function(port, ...) {
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

  get.contents.rawarch = function(port, ...) {
    ## gets the contents of the current (filetype)
    ## source, namely
    ## a list with three elements
    ## num.images: number of images in each run
    ## start.time:  starting timestamp of each run
    ## end.time:    ending timestamp of each run

    port$contents <- port$bl[[1]]

    ## correct names that had an extra "s" due to typos
    ## in documentation

    if ("start.times" %in% names(port$contents)) {
      port$contents$start.time <- port$contents$start.times
      port$contents$end.time <- port$contents$end.times
      port$contents[c("start.times", "end.times")] <- NULL
    }
      
    class(port$contents$start.time) <- "POSIXct"
    class(port$contents$end.time) <- "POSIXct"
    port$first.scan <- cumsum(c(1, port$contents$num.scans))
    return(port$contents)
  },

  end.of.data.rawarch = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    port$cur.scan > port$contents$num.scans[port$cur.run]
  },

  get.scan.info.rawarch = function(port, ...) {
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
    ## timestamp: POSIXct-style timestamp of start of scan
    ##
    ## time.msec: milliseconds past timestamp of start of scan
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
    ## rotation.direction: +1 = clockwise, -1 = counterclockwise

    port$cur.scan <- port$cur.scan + 1
    i <- port$cur.scan + port$first.scan[port$cur.run] - 1
    port$si <- port$bl[[2 *i]]
  },

  get.scan.data.rawarch = function(port, extmat, ...) {
    ## reads the scan data from the radar module into storage given by extmat
    ## returns extmat (with data filled) on success
    ## returns NULL on error

    i <- port$cur.scan + port$first.scan[port$cur.run] - 1
    dim(extmat) <- c(port$si$samples.per.pulse, port$si$pulses)
    if (isTRUE(port$si$compressed)) {
      .Call("raw_unpack", .Call("zlib_decompress", port$bl[[2 * i + 1]], (port$si$pulses * port$si$samples.per.pulse * port$si$bits.per.sample) %/% 8, NULL), pointer(extmat),
            as.integer(c(
                         switch(RSS$types$sample, short=2, char=1, int=4),
                         port$si$pulses * port$si$samples.per.pulse,
                         port$si$bits.per.sample,
                         .Platform$endian != "little")))
    } else {
      .Call("raw_unpack", port$bl[[2 * i + 1]], pointer(extmat), 
            as.integer(c(
                         switch(RSS$types$sample, short=2, char=1, int=4),
                         port$si$pulses * port$si$samples.per.pulse,
                         port$si$bits.per.sample,
                         .Platform$endian != "little")))
    }
  },

  put.scan.rawarch = function(port, header, extmat, ...) {
    ## puts most recently read scan (including header information) to the current sink
    ## returns NULL on error, TRUE otherwise
    ## header will be what was returned by get.scan.info() for the scan whose
    ## data are in extmat
    port$cur.scan <- port$cur.scan + 1
    i <- port$cur.scan + port$first.scan[port$cur.run] - 1
    port$bl[[2 * i]] <- port$si <- c(RSS$scan.info, compressed=compress)
    if (isTRUE(port$si$compressed)) {
      port$bl[[2 * i + 1]] <- .Call("zlib_compress", .Call("raw_pack", pointer(extmat),
                                                          as.integer(c(
                                                                       switch(RSS$types$sample, short=2, char=1, int=4),
                                                                       port$si$pulses * port$si$samples.per.pulse,
                                                                       port$si$bits.per.sample,
                                                                       .Platform$endian != "little"))), NULL)
    } else {
      port$bl[[2 * i + 1]] <- .Call("raw_pack", pointer(extmat),
                                    as.integer(c(
                                                 switch(RSS$types$sample, short=2, char=1, int=4),
                                                 port$si$pulses * port$si$samples.per.pulse,
                                                 port$si$bits.per.sample,
                                                 .Platform$endian != "little")))
    }
    if (!port$contents$start.time[port$cur.run])
      port$contents$start.time[port$cur.run] <- as.integer(RSS$scan.info$timestamp)
    port$contents$end.time[port$cur.run] <- as.integer(RSS$scan.info$timestamp)
    port$contents$num.scans[port$cur.run] <- 1 + port$contents$num.scans[port$cur.run]
  },

  seek.scan.rawarch = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run
    if (run)
      port$cur.run <- run
    port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
  },

  seek.time.rawarch = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)

    for (i in 1:length(port$contents$num.scans)) {
      if (time >= port$contents$start.time[i] && time <= port$contents$end.time[i] + 5) {
        port$cur.run <- i
        port$cur.scan <- floor(as.numeric(time - port$contents$start.time[i]) / as.numeric(port$contents$end.time[i] - port$contents$start.time[i]) * port$contents$num.scans[i])
        return(TRUE)
      }
    }
    return (NULL)
  },

  start.up.rawarch = function(port, ...) {
    ## do whatever is required to initialize
    ## a given object.  Return TRUE on sucess,
    ## NULL on error

    ## make sure the archive is valid
    verify.rawarch(port)
    
    port$bl <- biglist(port$config$filename, overwrite=port$is.sink, names=c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp", "time.msec", "duration", "sample.dist", "first.sample.dist", "bearing", "orientation", "latitude", "longitude"), cache.size=1, read.only=port$is.source)
  },

  shut.down.rawarch = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.
    if (port$is.sink && !identical(port$bl[[1]], port$contents)) {
      port$bl[[1]] <- port$contents
    }
    close(port$bl)

    ## Because we really only have one writer port and one reader port
    ## (as strictenv objects), we need to reset various properties to their initial
    ## values upon shutting down, so that the next time a file is selected for the port,
    ## these properties are not carried over.  Carrying over the $contents item was
    ## the cause of a bug wherein the table of contents kept getting larger each
    ## time a new raw archive was created and written.

    port$cur.run <- 0
    port$cur.scan <- 0
    port$contents <- NULL
    port$first.scan <- NULL
    port$file.basename <- NULL
    port$bl <- NULL
    port$si <- NULL
  },

  start.run.rawarch = function(port, start.time, ...) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls

    port$contents$num.scans <- c(port$contents$num.scans, 0)
    port$first.scan <- c(port$first.scan, 1+sum(port$contents$num.scans))
    port$contents$start.time <- c(port$contents$start.time, as.integer(0))
    port$contents$end.time <- c(port$contents$end.time, as.integer(0))
    port$cur.run <- 1 + port$cur.run
    port$cur.scan <- 0
  },

  end.run.rawarch = function(port, end.time, ...) {
    ## end the current sink run and make any
    ## required changes to its directory

    ## write the table of contents
    ## we leak a bit of filespace this way if there is more
    ## than one run, but it's a tiny amount
    port$bl[[1]] <- port$contents

    flush(port$bl)
  },

  new.play.state.rawarch = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }
  )  ## end of globals

verify.rawarch = function(port) {
  ## If the file corresponding to this port already exists,
  ## verify that the table of contents is not NULL.
  ## If the TOC is NULL, we rebuild it.
  ## This can happen when the the rawarch writer was not shut down cleanly,
  ## e.g. if radR ran out of disk space.

  fn <- port$config$filename
  
  if (!file.exists(fn))
    return()
  
  bl<-biglist(fn)

  toc.ok <- TRUE
  if (is.null(bl[[1]]) || class(bl[[1]]) != "list" || !all(c("num.scans", "start.time", "end.time") %in% names(bl[[1]]))) {
    ## The table of contents appears to be missing or corrupt

    toc.ok <- FALSE
  } else {
    ## There is an apparent toc there - we check it for correctness
    
    toc <- bl[[1]]
    n <- length(bl)
    if (sum(toc$num.scans) * 2 + 1 != n) {
      ## the total number of scans is incorrect
      toc.ok <- FALSE
    } else {
      irun <- 2
      for (i in 1:length(toc$num.scans)) {
        si <- bl[[irun]]
        toc.start.time <- as.numeric(toc$start.time[i])
        scan1.time <- floor(as.numeric(si$timestamp))
        toc.end.time <- as.numeric(toc$end.time[i])
        si <- bl[[irun + 2 * (toc$num.scans[i] - 1)]]
        scann.time <- floor(as.numeric(si$timestamp))
        if ( toc.start.time != scan1.time)
          toc.ok <- FALSE
        irun <- irun + 2 * (toc$num.scans[i] - 1)
        if ( toc.end.time != scann.time)
          toc.ok <- FALSE
        irun <- irun + 2
      }
    }
  }
  if (!toc.ok) {
    ##
    ## For safety, we assume the last item in the biglist
    ## is corrupt. 
    ## What we do depends on the parity of the biglist length:
    ##
    ## length(bl) == 2n + 1
    ##    - the last item is scan.data, but may be corrupt, so
    ##      we delete the last two items
    ## length(bl) == 2n
    ##    - the last item is scan.info, su we just drop it
    ## In both cases, we're left with a list of length
    ## 2n - 1, i.e.  n - 1 each of scan.info and scan.data items
    ##
    ## We then create the TOC item at list index 1.
    ##
    ## FIXME: This process only works for a raw archive with a single
    ## run of scans, because otherwise the TOC record will not
    ## be NULL.

    ## msg <- "The table of contents for file '" %:% fn %:% "' is missing or corrupt."
    ## rv <- rss.gui(POPUP_DIALOG,
    ##                   title="Fix RAW table of contents?",
    ##                   msg=msg %:% "\nShould I try to fix it?",
    ##                   buttons=c("Yes", "No"))
    
    ##     if (rv == 2)
    ##       stop("rawarch: " %:% msg)
    
    n <- floor(length(bl) / 2)
    first.ent <- bl[[2]]
    last.ent <- bl[[2 * n - 2]]
    first.ent <- bl[[2]]
    last.ent <- bl[[2 * n - 2]]
    toc <- list(num.scans = as.integer(n - 1),
                    start.time = structure(as.integer(first.ent$timestamp), class="POSIXct"),
                    end.time = structure(as.integer(last.ent$timestamp), class="POSIXct"))
    ## FIXME: why does this dialogue not accept clicks?
    ##     rv <- rss.gui(POPUP_DIALOG,
    ##                   title="Write new table of contents?",
    ##                   msg="For file '" %:% fn %:% "'\nI've inferred this table of contents:\n" %:% paste(capture.output(toc), collapse="\n") %:% "\nShould I write this to disk?",
    ##                   buttons=c("Yes", "No"))
    ##     if (rv == 2)
    ##       stop("rawarch: " %:% msg)
    
    if (length(bl) == 2*n + 1)
      bl[[2*n+1]] <- NULL
    bl[[2*n]] <- NULL
    bl[[1]] <- toc
  }
  close(bl)
}
    


