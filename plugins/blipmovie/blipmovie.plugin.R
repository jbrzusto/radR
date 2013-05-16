########
##
## blipmovie.plugin.R - the new biglist-based blipmovie archive
##
## a radR plugin for reading/writing raw data from blips and metadata in an archive
##
## a short machine-readable description of this plugin is available
## in the file blipmovie.desc.R
##
########

MYCLASS = "blipmovie"

about = function() {
  return(plugin.label %:% "\n\nVersion " %:% version %:%
         "\n\nThe NEW blipmovie format.\n" %:%
         "\n") 
}

set.bl.length = function(bl, len) {
  dim(bl$ndx) <- c(len, 2)
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
                        file.ext = "bm",
                        has.toc = TRUE,
                        cur.run = 0,
                        cur.scan = 0,
                        contents = empty.TOC,
                        file.basename = NULL,
                        bl = NULL,                         # biglist object holding blipmovie contents
                        si = NULL,                         # scan info
                        si.defaults = NULL,                # default values for scan info
                        si.order = NULL,                   # integer vector for getting scan.info in alphabetical order
                        run.hdr = list()                   # header for each run, with items data.items, n.slot
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
}

unload = function(save.config) {
}

slot.of.scan <- function(p, scan=p$cur.scan) {
  ## return the index in the biglist of the 1st slot corresponding
  ## to the given scan in the current run; that slot is for the scan.info,
  ## and the following p$run.hdr$n.slot-1 slots are for scan data items
  ## Note that each run begins with a header slot, which is why
  ## we add 1 here.
  
  return (p$contents$first.slot[p$cur.run] + 1 + (scan - 1) * p$run.hdr$n.slot)
}

get.menus = function() {
  list(
       sources = list (
         titles = "Blipmovie reader",
         menu = list(
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         ),

       sinks = list (
         titles = "Blipmovie writer",
         menu = list (
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[2]])
           )
         )
       )
}


globals = list (
  as.character.blipmovie = function(x, ...) {
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
  
  print.blipmovie = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.blipmovie = function(port, ...) {
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
                 f <- opts[[opt]]
                 if (length(grep("\\.bm$", f)) == 0)
                   f <- f %:% ".bm"
                 port$config$filename <- f
                 port$file.basename <- strsplit(f, "\\.[a-zA-Z]*$")[[1]]
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

  get.contents.blipmovie = function(port, ...) {
    ## gets the contents of the current (filetype)
    ## source, namely
    ## a dataframe with three elements
    ## num.images: number of images in each run
    ## start.time:  starting timestamp of each run
    ## end.time:    ending timestamp of each run

    port$contents <- port$bl[[1]]
    port$contents
  },

  end.of.data.blipmovie = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    port$cur.scan >= port$contents$num.scans[port$cur.run]
  },

  get.scan.info.blipmovie = function(port, ...) {
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
    ## rotation.direction: +1 = clockwise, -1 = counterclockwise

    port$cur.scan <- port$cur.scan + 1
    i <- slot.of.scan(port)
    if (port$cur.scan == 1) {
      ## the first scan in a run has its full metadata written out
      ## in unsorted order, and these become defaults for the whole run
      si <- port$bl[[i]]
      port$si.order <- order(names(si))
      port$si.defaults <- si[port$si.order]
    } else {
      ## otherwise, only metadata different from the default were written,
      ## preceded by a raw vector indicating (as bit flags) which metadata
      ## were actually written.  We don't bother re-ordering the list we
      ## return because all code is supposed to index it by names, not by order.
      items <- port$bl[[i]]
      which <- which(as.logical(rawToBits(items[[1]])))
      si <- port$si.defaults
      for (i in seq(along=which))
        ## for saved items, restore them with the class of the default value
        ## we use attr(x, "class") rather than class(x) because we don't want
        ## spurious "integer" and "numeric" class attributes created for
        ## plain integer and numeric variables.
        si[[which[i]]] <- structure(items[[i+1]], class=attr(si[[which[i]]], "class"))
    }
    port$si<-si
    return(si)
  },

  get.scan.data.blipmovie = function(port, extmat, ...) {
    ## reads the data items for the current scan.  Any raw scan data
    ## is stored in "extmat", which is typically RSS$prev.scan.mat
    ## returns extmat (typically containing new data) on success
    ## returns NULL on error

    ## make sure data matrices all have the same correct size,
    ## whether or not we will read anything from them
    dim <- c(port$si$samples.per.pulse, port$si$pulses)

    if (is.null(dim))
      stop("calling get.scan.data when RSS$scan.info has NULL dimension info")
    
    dim(extmat) <- dim
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim
    zero(extmat)
    zero(RSS$class.mat)
    zero(RSS$score.mat)
    
    i <- slot.of.scan(port)
    err <- FALSE
    for (p in names(port$run.hdr$data.items))
      rss.call.plugin.hook(RSS$GET_SCAN_DATA_HOOK, p, port$bl, port$run.hdr$data.items[[p]]$names, i + port$run.hdr$data.items[[p]]$offset, extmat)
    if (err)
      return(NULL)
    return(extmat)
  },

  put.scan.blipmovie = function(port, header, extmat, ...) {
    ## puts most recently read scan (including header information) to the current sink
    ## returns NULL on error, TRUE otherwise
    ## header will be what was returned by get.scan.info() for the scan whose
    ## data are in extmat
    port$cur.scan <- port$cur.scan + 1
    i <- slot.of.scan(port)

    ## record scan info items in alphabetical order
    if (is.null(port$si.defaults)) {
      ## if this run has no default scan info yet, then record the full scan info
      ## (whose values will be the defaults for this run).
      si <- RSS$scan.info[port$si.order <- order(names(RSS$scan.info))]
      port$bl[[i]] <- port$si.defaults <- si
    } else {
      ## otherwise, record only those items which are different from the default
      si <- RSS$scan.info[port$si.order]
      which <- ! mapply(identical, si, port$si.defaults)
      if (pad <- length(which) %% 8)
        ## pad with FALSE up to a multiple of 8 in length
        which <- c(which, rep(FALSE, 8 - pad))
      ## record the bitflags of locations of items, and the items themselves, with classes dropped
      port$bl[[i]] <- structure(c(list(packBits(as.integer(which))), lapply(si[which], unclass)), names=NULL)
    }

    ## for each plugin with a PUT_SCAN_DATA hook, call the hook with
    ## the correct slot for that plugin's first data item
    
    for (p in names(RSS$hooks$PUT_SCAN_DATA))
      rss.call.plugin.hook(RSS$PUT_SCAN_DATA_HOOK, p, port$bl, i + port$run.hdr$data.items[[p]]$offset)

    if (!port$contents$start.time[port$cur.run])
      port$contents$start.time[port$cur.run] <- RSS$scan.info$timestamp

    port$contents$end.time[port$cur.run] <- RSS$scan.info$timestamp
    port$contents$num.scans[port$cur.run] <- 1 + port$contents$num.scans[port$cur.run]
  },

  seek.scan.blipmovie = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run
    if (run && run != port$cur.run) {
      ## load the header
      port$run.hdr <- port$bl[[port$contents$first.slot[run]]]
      port$cur.run <- run
    }
    port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
  },

  seek.time.blipmovie = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)

    for (i in 1:length(port$contents$num.scans)) {
      if (time >= port$contents$start.time[i] && time <= port$contents$end.time[i] + 5) {
        seek.scan.blipmovie(port, i, floor(as.numeric(time - port$contents$start.time[i]) / as.numeric(port$contents$end.time[i] - port$contents$start.time[i]) * port$contents$num.scans[i]))
        return(TRUE)
      }
    }
    return (NULL)
  },

  start.up.blipmovie = function(port, ...) {
    ## do whatever is required to initialize
    ## a given object.  Return TRUE on sucess,
    ## NULL on error

    ## make sure the archive is valid
    if (!verify.blipmovie(port)) {
      return(NULL)
    }

    ## enable hooks
    for (h in c("GET_SCAN_DATA_NAMES", "PUT_SCAN_DATA", "GET_SCAN_DATA"))
      rss.enable.hook(h, name)

    ## open the file
    port$bl <- biglist(port$config$filename, overwrite=port$is.sink, cache.size=1, read.only=port$is.source)
    
    if (port$is.source) {
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
                   list(f=function(...){}, enabled=TRUE, read.only=FALSE))
    } else {
      ## discard any information from a previous use of this port

      port$contents <- empty.TOC
      port$run.hdr <- NULL
      port$cur.run <- 0
      port$cur.scan <- 0
    }
  },

  shut.down.blipmovie = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.

    end.run.blipmovie(port, ...)
    
    for (h in names(hooks))
      rss.disable.hook(h, name)

    if (port$is.source) {
      RSS$skip$classify.samples <- FALSE
      RSS$skip$update.stats <- FALSE
      RSS$scans.to.learn <- RSS$default.scans.to.learn
      if (!is.null(save.restart.learning))
        RSS$restart.learning.after.stop <- save.restart.learning 
      
      ## drop the patch finding hook
      rss.drop.hook("FIND_PATCHES", MYCLASS)
      ## re-enable the blipping controls
      rss.gui(ENABLE_CONTROLS, "blip.finding", TRUE)
    }
    close(port$bl)
    port$bl <- NULL
  },

  start.run.blipmovie = function(port, start.time, ...) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls
    ## We write a header in the first slot for this run, recording
    ## what kind of data items are stored in its scans.

    data.items <- list()
    i <- 1 ## the zero-offset item is scan.info; the first data item is at offset 1
    
    ## call each plugin's get_scan_data_names_hook, recording the index
    ## of the first data item for each plugin in 'i'
    for (p in names(RSS$hooks$GET_SCAN_DATA_NAMES)) {
      names <- rss.call.plugin.hook(RSS$GET_SCAN_DATA_NAMES_HOOK, p)
      data.items[[p]] <- list(names=names, offset = i)
      i <- i + length(names)
    }
    ## the number of slots per scan, including the one for scan.info
    port$run.hdr <- list(data.items=data.items, n.slot=i)

    ## the first slot for this run:

    first.slot <- length(port$bl) + 1

    ## allow for a table of contents in the first slot
    ## (it has not been written yet)
    
    if (first.slot == 1)
      first.slot <- 2
    
    port$bl[[first.slot]] <- port$run.hdr

    ## append an entry for this run to the table of contents
    ## the entry's fields will be modified as we write data to the run

    port$contents <- rbind(port$contents, data.frame(start.time=structure(0, class="POSIXct"), end.time=structure(0, class="POSIXct"), num.scans=0, first.slot=first.slot))

    ## set position indicator to indicate we're starting a new run
    port$cur.run <- 1 + port$cur.run
    port$cur.scan <- 0
    
    ## mark the default scan info as unset for this run
    port$si.defaults <- NULL
  },

  end.run.blipmovie = function(port, ...) {
    ## end the current sink run and make any
    ## required changes to its directory

    ## write the table of contents
    ## we leak a bit of filespace this way if there is more
    ## than one run, but it's a tiny amount
    if (port$is.sink && !identical(port$bl[[1]], port$contents)) {
      port$bl[[1]] <- port$contents
    }
    flush(port$bl)
  },

  new.play.state.blipmovie = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }
  )  ## end of globals

fixing.message = function(msg, preamble=TRUE) {
  ## Notify the user that the blipmovie is being repaired
  if (!fixing.notification.given) {
    if (preamble)
      rss.gui(POPUP_MESSAGEBOX, "Fixing blipmovie",
              "The blipmovie table of contents is corrupted.\nThis could be due to a power failure or full disk.\nI will try to reconstruct the TOC.\nSee the console window for details.")
    fixing.notification.given <<- TRUE
    rss.gui(CONSOLE_MESSAGE, "\nFixing blipmovie '" %:% fixing.filename %:% "'...\n")
  }
  rss.gui(CONSOLE_MESSAGE, msg %:% "\n")
}

verify.blipmovie = function(port) {
  
  ## If the file corresponding to this port already exists,
  ## verify that the table of contents is not NULL.
  ## If the TOC is NULL, we rebuild it.
  ## This can happen when the the blipmovie writer was not shut down cleanly,
  ## e.g. if radR ran out of disk space.

  if (!file.exists(port$config$filename))
    return(port$is.sink)

  fixing.notification.given <<- FALSE
  fixing.filename <<- port$config$filename
  
  bl <- biglist(port$config$filename, read.only=FALSE)

  if (length(bl) == 0) {
    if (port$is.source)
      fixing.message ("This blipmovie is empty.", FALSE)
    close(bl)
    return(port$is.sink)
  }
  
  ## if the table of contents is missing or incomplete, reconstruct it

  toc <- bl[[1]]

  if (is.null(toc)) {
    toc <- empty.TOC
    first.slot <- 2
  } else {
    ## check whether the last TOC entry is correct
    nrun <- dim(toc)[1]
    if (nrun > 0) {
      k <- bl[[toc$first.slot[nrun]]]$n.slot
      if (is.null(k)) {
        ## the last run described is not valid
        if (nrun == 1) {
          toc <- empty.TOC
          fixing.message("The TOC has only one entry and its first slot is not valid.\nThe blipmovie is effectively empty.\n")
          set.bl.length(bl, 0)
          close(bl)
          return(TRUE)
        }
        toc <- empty.TOC
        fixing.message("The TOC's last entry is not valid.\nThe corresponding run will be dropped.\n")
        toc <- toc[-nrun,]
      } else {
        diff <- length(bl) - (toc$first.slot[nrun] + k * toc$num.scans[nrun])
        if (diff == 0)
          ## the blipmovie TOC is correct
          return(TRUE)
        if (diff < 0) {
          ## we're missing runs
          fixing.message("The blipmovie's last run is missing some or all data from its last " %:% ceiling( -diff / toc$num.scans[nrun]) %:% " scans.  I'm adjusting the TOC to match this.")
          toc$num.scans[nrun] <- toc$num.scans[nrun] - ceiling( -diff / k)
          set.bl.length(bl, toc$first.slot[nrun] + k * toc$num.scans[nrun])
          bl[[1]] <- toc
          close(bl)
          return(TRUE)
        }
        ## there are extra scans
        ## we indicate the first existing slot not accounted for by the TOC
        ## and fall through to the table reconstruction code
        first.slot <- 1 + toc$first.slot[nrun] + k * toc$num.scans[nrun] 
      }
    } else {
      fixing.message("This blipmovie is empty.\n", FALSE)
      set.bl.length(bl, 0)
      close(bl)
      return(FALSE)
    }
  }

  ## There are extra slots in the biglist not accounted for by the TOC
  ## Assume they are all from a single run, and that the last scan in that
  ## run is corrupt.

  last.slot <- length(bl)
  k <- bl[[first.slot]]$n.slot
  if (is.null(k)) {
    resp <- rss.gui(POPUP_DIALOG, title="Delete last blipmovie run?", msg="The header for the last run (" %:% nrun %:% ") is invalid.\nI don't know how to fix this, but I can delete this run so you can use the blipmovie.\nThis run appears to have at most " %:% ((last.slot - first.slot) / 4) %:% " scans, which will be lost if I delete it.\nShould I delete this run?\n", buttons=c("Yes", "No"), default=2)
    if (resp == 2) {
      close(bl)
      return(FALSE)
    }
    if(nrun > 1) {
      toc <- toc[-dim(toc)[1],]
      bl[[1]] <- toc
      fixing.message("The last run of this blipmovie has been deleted.")
      close(bl)
      return(TRUE)
    }
    fixing.message("The only run of this blipmovie has been deleted.  It is now empty.")
    set.bl.length(bl, 0)
    close(bl)
    return(FALSE)
  }

  ## Each scan takes k slots.  Assume the last scan has a problem, and
  ## determine the number of other scans.  Note that the first slot
  ## is a header, not a scan.
  num.scans <- ceiling((length(bl) - first.slot) / k) - 1

  if (num.scans == 0) {
    fixing.message("The blipmovie's last (only?) run had only one scan,\n which might be corrupt.  I'm deleting that run.")
    set.bl.length(bl, first.slot - 1)
    close(bl)
    return(TRUE)
  }
  last.slot <- first.slot + num.scans * k

  ## The last (only?) scan had more than one run, so set the last TOC entry accordingly

  ## retrieve the default scan info for that run
  si <- bl[[first.slot + 1]]
  start.time <- si$timestamp

  ## update it with the differential info for the last scan
  items <- bl[[first.slot + 1 + (num.scans - 1)*k]]
  which <- which(as.logical(rawToBits(items[[1]])))
  for (i in seq(along=which))
    si[[which[i]]] <- structure(items[[i+1]], class=attr(si[[which[i]]], "class"))

  toc <- rbind(toc, data.frame(start.time=structure(start.time, class="POSIXct"), end.time=structure(si$timestamp, class="POSIXct"), num.scans=num.scans, first.slot=first.slot))
  bl[[1]] <- toc
  set.bl.length(bl, last.slot)
  fixing.message("I've added a run to the end of the TOC, which now looks like:\n" %:% paste(capture.output(toc), collapse="\n"))
    
  ## Close the biglist
  close(bl)
  return(TRUE)
}
    
hooks = list(

  GET_SCAN_DATA_NAMES = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      return(c("blip.runbuf", "blip.samples", "blip.scores"))
    }),

  PUT_SCAN_DATA = list( enabled = FALSE, read.only = TRUE,
    f = function(bl, i) {
      if (any(to.save) && RSS$num.blips > 0) {
        runs <- .Call("get_active_runbuf", RSS$patch.buffer)
        if (!is.null(runs)) {
          if (to.save$blip.runbuf)
            bl[[i]] <- runs
          if (to.save$blip.samples)
            bl[[i+1]] <- .Call("index_extmat_by_runbuf", RSS$scan.mat, runs)
          if (to.save$blip.scores) {
            if (RSS$have.valid$scores)
              bl[[i+2]] <- .Call("index_extmat_by_runbuf", RSS$score.mat, runs)
            else
              bl[[i+2]] <- NULL
          }
        }
      }
    }),

  GET_SCAN_DATA = list( enabled = FALSE, read.only = TRUE,
    f = function(bl, names, i, scan.mat) {
      if (to.load$blip.runbuf) {
        runs <- bl[[i]]
        ## call set_runbuf even if runs is NULL, in which
        ## case the image runbuffer is marked empty
        .Call("set_runbuf", RSS$patch.buffer, runs)
        if (!is.null(runs)) {
          RSS$class.mat[.Call("get_indexes_from_runbuf", runs)] <- as.integer(RSS$CLASS.VAL$hot)
          RSS$new.have.valid$classification <- TRUE
          run.info <- .Call("get_runbuf_info", runs)
          RSS$num.blips <-run.info[1]
          RSS$num.hot.samples <- run.info[2]
          
          ## the following items can only be used if
          ## runs were loaded
          if (to.load$blip.samples) {
            .Call("assign_extmat_by_runbuf", scan.mat, runs, bl[[i+1]])
          }
          if (to.load$blip.scores) {
            sc <- bl[[i+2]]
            if (!is.null(sc)) {
              .Call("assign_extmat_by_runbuf", RSS$score.mat, runs, sc)
              RSS$new.have.valid$scores <- TRUE
            }
          }
        } else {
          RSS$num.blips <- RSS$num.hot.samples <- 0
          ## FIXME: this is redundant; we should always have length(RSS$blips) = RSS$num.blips, so
          ## eliminate the latter.
          RSS$blips <- NULL
        }
      }
    })
  
  ) ## end of hooks

## additional plugin variables

save.restart.learning = NULL
          
fixing.notification.given = FALSE
fixing.filename = ""      
          
## what a blipmovie table of contents looks like, initially
empty.TOC = NULL


  
