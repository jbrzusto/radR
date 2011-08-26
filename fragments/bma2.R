##
## bma2.R - R module for playing back movies saved by the blipmovie plugin
##
## by John Brzustowski, 2006 (jbrzusto AT fastmail DOT fm)
##
## indexes have origin 1 in R, origin 0 in C
##
## Version 00.20 - new flexible high-level format using object serialization

BMA2.PACKAGE <- "bma2"
rss.dyn.load(BMA2.PACKAGE)

## Documentation of the file format:
## This is an uncompressed seekable binary file.

## A raw magic 16 byte string, identifying this as a radR bma file.
## "radR BMA V00.20\n" 

## A raw number:
##   toc.offset: a double giving the absolute offset in the file of the first TOC record
##               a -1 here means no TOC has yet been written

## The table of contents is two raw items followed by a serialized object.
## The raw items are:

##   toc.magic:   16-byte raw magic string "radR BMA TOC rec" 
##   toc.offset:  raw double gives absolute offset in file to next TOC record
##                0 here means there are no further TOC records

## and the serialized object is a dataframe with these columns
  
##   $run.id:     integer vector of run IDs
##   $radar.id:   integer vector of radar IDs
##   $avail.data: integer vector of flags indicating what data are available for the scan
##                 1: full raw      (raw data for all samples)
##                 2: full score    (score for all samples)
##                 4: blip geometry (which samples are in which blips)
##                 8: blip raw      (raw data for samples in blips)
##                16: blip score    (raw data for scores in blips)
##                ... room for expansion
##   $timestamp:  double vector of scan start times, including fractional seconds
##   $duration:   integer vector of scan lengths in milliseconds
##   $latitude:   double vector of latitude of radar (in degrees) at start of scan
##   $longitude:  double vector of longitude of radar (in degrees) at start of scan
##   $elevation:  double vector of elevation (in metres) of radar at start of scan
##   $radius:     double vector of scan radius, in metres
##   $scan.offset:double vector of file offsets for start of data for scan

## The purpose of the table of contents is to allow quick filtering and seeking of
## scan records according to location in time and space and radar id.

## The data for a scan is two independently serialized objects:

## - a list of scan information with these elements:

##   $radar.id:           an integer identifying the radar from which this scan comes
##   $timestamp:          a double giving the time stamp
##   $duration:           integer length of scan in milliseconds
##   $latitude:           double vector of latitude of radar (in degrees) at start of scan
##   $longitude:          double vector of longitude of radar (in degrees) at start of scan
##   $elevation:          double vector of elevation (in metres) of radar at start of scan
##   $pulses:             integer number of pulses in this scan
##   $samples.per.pulse:  integer
##   $bits.per.sample:    integer
##   $sample.dist:        double distance spanned by each sample
##   $first.sample.dist:  double distance to start of first sample
##   $bearing:            double degrees clockwise from north of first pulse
##   $orientation:        integer (+/- 1) direction of antenna rotation +1=clockwise, -1=CCW
##   $num.blips:          number of blips in this scan

## - a list of scan data with elements corresponding to each bit flag in the avail.data field
##   above:
  
##   $blip.geometry:     a raw vector giving blip locations
##   $blip.raw:          a raw vector of blip sample data
##   $blip.score:        a raw vector of blip score data

## Creating a blip movie archive:

## - write the raw magic string
## - write -1 as the TOC offset

## - write scans, growing the in-memory TOC until SCANS_PER_TOC
##   have been written, then:
##   - remember file offset as TOC.start
##   - write TOC magic
##   - write 0 as TOC offset
##   - write serialized TOC
##   - push file location
##   - seek back to location of last TOC offset
##   - write TOC.start as a raw double
##   - pop file location

## When a run ends, write the TOC as above.

## When opening a BMA file for read:

## - check for magic header string
## loop:
##   - read the raw next TOC.offset
##   - if TOC.offset == -1 break
##   - append the TOC to the existing in-memory frame
##   - if TOC.offset == 0 break
##   - seek to the TOC.offset
## endloop
## - if not at end of file, then warn user of missing (final) TOC
##   and if requested, perform a scan to build it.

## create the class object, which is really just an environment
## with an appropriate class attribute

## In memory, we keep track of two table of contents:
##
## port$scan.toc is the full scan-wise table of contents, sorted by
##        timestamp within run.id; this is read from the linked (by TOC.offset fields)
##        of TOC pieces in the file.
##
## port$run.toc is the radR-level list of runs with these elements:
##        $start.time: timestamp of first scan in run
##        $end.time:   timestamp of last scan in run
##        $num.scans:  number of scans in this run
##        $first.scan.offset: (index - 1) into the port$scan.toc of the first scan in this run
##              
## While writing, the location of the previous TOC.offset field is kept in
## last.toc.offset.location, so that when the next TOC piece is written, that offset
## can be made to point to it.
##

BMA2 <<- new.env(hash=TRUE, .GlobalEnv)
class(BMA2) <- BMA2.PACKAGE

evalq( {
  name <- "Blip movie archive V2"
  type <- "module"
  id <- 1 + length(RSS$modules)
  has.units <- FALSE
  file.magic <- "radR BMA V00.20\n"
  toc.magic <-  "radR BMA TOC rec"
  max.toc.entries <- 10  ## FIXME for exercising the program now; make this 1024 later
  data.types <- list(full.raw = 1, full.score = 2, blip.geometry = 4, blip.raw = 8, blip.score = 16)
  reader <- new.env(hash=TRUE)
  writer <- new.env(hash=TRUE)
}, BMA2)

## the reader and writer objects

evalq( {
  ## generic vars
  type <- "port"
  name <- "Reader"
  id <- 1
  module <- BMA2
  is.source <- TRUE
  is.sink <- FALSE
  is.file <- TRUE
  is.seekable <- TRUE
  has.toc <- TRUE
  file.ext <- "bma"
  ## specific vars
  scan.info.waiting <- FALSE
  scan.data.waiting <- FALSE
  current.run <- 0
  current.scan <- 0
  run.toc <- NULL
  scan.toc <- NULL
  filename <- NULL
  file <- NULL
}, BMA2$reader)

class(BMA2$reader) <- BMA2.PACKAGE

evalq( {
  ## generic vars
  type <- "port"
  name <- "Writer"
  id <- 2
  module <- BMA2
  is.source <- FALSE
  is.sink <- TRUE
  is.file <- TRUE
  is.seekable <- TRUE
  has.toc <- TRUE
  file.ext <- "bma"
  ## specific vars
  current.run <- 0
  current.scan <- 0
  run.toc <- NULL
  scan.toc <- NULL
  filename <- NULL
  file <- NULL
}, BMA2$writer)

class(BMA2$writer) <- BMA2.PACKAGE

## BMA2-local methods
##
## In these definitions, `<<-` assigns in the BMA2 environment

evalq({

  ## utility functions for file I/O
  
  get.double <- function(f, n=1) {
    readBin(f, "double", n, size=8, endian="little")
  }
  
  put.double <- function(x, f) {
    writeBin(x, f, size=8, endian="little")
  }
  
  get.int <- function(f, n=1) {
    readBin(f, "int",    n, size=4, endian="little")
  }
  
  put.int <- function(x, f) {
    writeBin(x, f, size=4, endian="little")
  }

  put.string <- function(x, f) {
    writeBin(charToRaw(x), f)
  }

  get.string <- function(f, n) {
    rawToChar(readBin(f, "raw", n))
  }
  
  get.object <- function(f) {
    .Call("R_unserialize", f, NULL)
  }
  
  put.object <- function(x, f) {
    .Call("R_serialize", x, f, FALSE, NULL)
  }
  
  eof <- function(f) {
    rv <- 0 == nchar(get.string(f, 1))
    if (!rv)
      seek(f, -1, "current", rw="r")
    return(rv)
  }

  seek.write.to.read.pos <- function(f) {
    ## seek write position to read position
    ## for a file
    seek(f, seek(f, NA, rw="r"), rw="w")
  }

  check.for.magic <- function(file, magic) {
    ## return TRUE if file has a sequence of bytes
    ## equal to the (ascii) magic at the current location
    ## return FALSE otherwise;
    ## The file position changes to
    ## seek(file, NA) + nchar(magic)
    
    magic == get.string(file, nchar(magic))
  }

  as.character.bma2 <- function(obj) {
    switch(obj$type,
           module = "radR blip movie archive module V2",
           port = paste ("radR blip movie archive V2",
             c("reader", "writer")[obj$id],
             if (is.null(obj$filename) || obj$filename == "") {
               "(no file selected)"
             } else {
               paste("using file '", obj$filename, "'", sep="")
             }),
           "radR blip movie internal object")
  }
  
  print.bma2 <- function(obj) {
    ## print a description of this object
    cat(as.character(obj) %:% "\n")
  }

  get.units.bma2 <- function(mod) {
    ## This module does not have units.
    NULL
  }

  config.bma2 <- function(obj, opts) {
    ## Get/set port options.
    ## Currently, the only option is "filename"
    ## This opens the file and checks for file.magic
    ## if the file exists, and writes file.magic and a NULL TOC offset if
    ## the file does not exist and the port is the writer.
    ## In all cases, if the file is successfully opened,
    ## the read file position is immediately after file.magic, at the first byte of
    ## the TOC offset.
    ## For the writer, the write file position is end of file (which is the
    ## same position, if the file is new).
    
    if (obj$type != "port")
      return(TRUE)
    
    opts <- as.list(opts)
    switch(obj$type,
           port = if (is.null(opts)) {
             return(list(filename=obj$filename))
           } else {
             obj$filename <- opts$filename
             if (!is.null(obj$file))
               try (close(obj$file), silent=TRUE)
             obj$file <- NULL
             new <- ! file.exists(opts$filename)
             try (obj$file <- file(opts$filename, switch(obj$id, "rb", if(new) "w+b" else "r+b")), silent=TRUE)
             if (is.null(obj$file)) {
               error <<- RSS$errors$CANT_OPEN_ARCHIVE
               return(NULL)
             } else {
               if (obj$id == 1 || !new) {
                 if (!check.for.magic(obj$file, file.magic)) {
                   error <<- RSS$errors$INVALID_ARCHIVE
                   return(NULL)
                 }
               } else {
                 put.string(file.magic, obj$file)
                 obj$last.toc.offset.location <- seek(obj$file, NA, rw="W")
                 put.double(-1, obj$file)
               }
               get.scan.toc(obj)
             }
             return(obj)
           },
           
           module = if (is.null(opts)) {
             return(list())
           } else {
             return (obj)
           },
           
           return(NULL))
  }

  get.error.bma2 <- function(mod) {
    ## return the latest error associated with this port
    ## generally, we return the error set within this
    ## R file
    if (!is.null(error)) {
      rv <- error
      error <<- RSS$errors$NONE
      return (rv)
    } else {
      return (RSS$errors$NONE)
    }
  }

  get.error.msg.bma2 <- function(mod) {
    ## return the latest error message associated with this module
    paste(rss.get.message.for.error(get.error.bma2(mod)), sep="")
  }

  get.ports.bma2 <- function(mod) {
    ## return a list of sources and sinks for this unit
    return(list(reader, writer))
  }

  put.scan.toc <- function(port, toc=port$scan.toc, pos=seek(port$file, NA, rw="w")) {
    ## Write a table of contents fragment to the port's file
    ## at position pos.
    ## On entry, the value last.toc.offset.location must
    ## be the location of the offset which will point to this
    ## toc fragment.
    ## On exit, the file write position will be immediately
    ## after this toc fragment, which will be the end of file,
    ## and last.toc.offset.location will point to this toc
    ## fragment's offset field.
    ## If the port is a reader, the file is reopened
    ## for writing before writing the toc, then reopened
    ## readonly after.  In this case, the port's file read position
    ## is used.
    ## returns TRUE on success,
    ## FALSE on failure
    
    if (port$id == 1) {
      init.read.loc <- seek(port$file, NA, rw="w")
      ## reopen this file for writing
      close(port$file)
      port$file <- NULL
      try(port$file <- file(port$filename, "r+b"), silent=TRUE)
      if (is.null(port$file)) {
        error <- RSS$errors$CANT_OPEN_ARCHIVE
        return (FALSE)
      }
    }

    seek(port$file, pos, rw="w")

    ## write the magic
    put.string(toc.magic, port$file)
    ## remember where the TOC fragment offset will be
    offset.loc <- seek(port$file, NA, rw="w")
    ## write the "I'm the last TOC fragment" offset
    put.double(0, port$file)
    ## write the TOC fragment
    put.object(toc, port$file)
    ## correct the offset from the previous TOC fragment
    seek(port$file, last.toc.offset.location, rw="w")
    put.double(offset.loc, port$file)
    ## seek to the end of the file
    seek(port$file, 0, "end", rw="w")
    ## record where the offset field for this TOC fragment is
    last.toc.offset.location <<- offset.loc

    if (port$id == 1) {
      ## reopen this file readonly, restoring the location of the read position
      close(port$file)
      port$file <- NULL
      try(port$file <- file(port$filename, "rb"), silent=TRUE)
      if (is.null(port$file)) {
        error <- RSS$errors$CANT_OPEN_ARCHIVE
        return (FALSE)
      }
      seek(port$file, init.read.loc)
    }
    flush(port$file)
  }

  get.scan.toc <- function(port) {
    ## read the full table of contents from the port's file
    ## regardless of where the file read position is.
    ## The file read position is left at the end of file.
    
    toc <- NULL

    ## seek to the location of the first TOC offset field
    offset.location <- nchar(file.magic)
    seek(port$file, offset.location, rw="r")
    raw.toc.offset <- get.double(port$file)
    if (raw.toc.offset != -1) {
      ## a toc has been written, so seek to it
      seek(port$file, raw.toc.offset, rw="r")
      while (TRUE) {
        ## keep recording the locations of offset fields
        offset.location <- seek(port$file, NA, rw="r")
        raw.toc.offset <- get.double(port$file)
        toc <- rbind(toc, get.object(port$file))
        if (raw.toc.offset == 0)
          ## no further toc fragments
          break
        seek(port$file, raw.toc.offset, rw="r")
      }
    }
    ## in case this is the writer, we record the location
    ## of the last toc offset field, because this offset will be
    ## modified if when the next new TOC piece is written
    last.toc.offset.location <<- offset.location
    if (!eof(port$file)) {
      ## assemble and write a missing TOC
      rss.gui(POPUP_DIALOG, "Missing final table of contents", "The selected file is missing part of its table of contents,\npossibly because radR was not shutdown cleanly.\n\nThere will be a short delay while the table of contents is rebuilt.")
      missing.toc <- assemble.toc.from.scans(port)
      ## we're at the end of scan data, so write the reconstructed toc
      put.scan.toc(port, missing.toc, pos=seek(port$file, NA, rw="r"))
      toc <- rbind(toc, missing.toc)
    }

    if (!is.null(toc)) {
      ## sort according to run id then timestamp
      toc <- toc[order(toc$run.id, toc$timestamp),]
      
      ## store in the global BMA2 reader object
      port$scan.toc <- toc
      
      ## extract the simplified TOC
      num.scans <-  tapply(toc$run.id,    toc$run.id, length)
      start.time <- tapply(toc$timestamp, toc$run.id, min)
      end.time   <- tapply(toc$timestamp, toc$run.id, max)
      first.scan.offset <- c(0, cumsum(num.scans[-length(num.scans)]))
      port$run.toc <- list(num.scans=num.scans, start.time=start.time, end.time=end.time, first.scan.offset=first.scan.offset)
    } else {
      port$scan.toc <- NULL
      port$run.toc <- list(num.scans=c(), start.time=c(), end.time=c(), first.scan.offset=c())
    }
  }

  assemble.toc.from.scans <- function(port) {
    ## build a TOC for the remaining scans in the file
    ## On enter, the read position of port$file must
    ## be immediately after the last TOC fragment
    ## Because the TOC might have been partly written,
    ## we must examine each segment of the disk for
    ## toc magic, so we know where to truncate
    ## Scans found are appended to the last run.
    ## On exit, the read position of the file
    ## is at the first byte after the last valid scan record
    
    init.read.loc  <- seek(port$file, NA, rw="r")
    init.write.loc <- seek(port$file, NA, rw="w")
    run.id <- length(port$run.toc$num.scans)
    
    toc <- NULL
    magic.written <- FALSE
    while (TRUE) {
      if (check.for.magic(port$file, toc.magic)) {
        ## we've found an unlinked and possibly incomplete TOC
        ## we seek to just before the magic number before writing
        ## the replacement TOC
        seek(port$file, nchar(toc.magic), "current", rw="r")
        break
      }
      scan.info <- scan.data <- NULL
      prescan.loc <- seek(port$file, NA, rw="r")
      try(scan.info <- get.object(port$file), silent=TRUE)
      try(scan.data <- get.object(port$file), silent=TRUE)
      if (is.null(scan.data) || is.null(scan.info)) {
        ## this scan was not completely written, so we truncate the
        ## file here
        seek(port$file, prescan.loc, rw="r")
        break
      }
      ## append the TOC record to the table so far
      toc <- rbind(toc,
                   data.frame(c(run.id = run.id,
                                avail.data = sum(unlist(data.types[names(scan.data)])),
                                scan.info[c("radar.id",
                                            "timestamp",
                                            "duration",
                                            "latitude",
                                            "longitude",
                                            "elevation",
                                            "radius")],
                                scan.offset = prescan.loc,
                                radius = scan.info$samples.per.pulse * scan.info$sample.dist + scan.info$first.sample.dist
                                )))
      if (eof(port$file))
        break
    }
    return(toc)
  }
       
  get.contents.bma2 <- function(port) {
    return(port$run.toc)
  }

  scan.info.waiting.bma2 <- function(port) {
    if (port$id != 1)
      return (FALSE)
    return (port$current.run > 0 && port$current.scan <= port$run.toc$num.scans[port$current.run])
  }

  scan.data.waiting.bma2 <- function(port) {
    if (port$id != 1)
      return (FALSE)
    return (port$current.run > 0 && port$current.scan <= port$run.toc$num.scans[port$current.run])
  }

  next.scan.bma2 <- function(port) {
    ## the nomenclature is poor.
    ## This grabs the scan info for the current scan, if within range,
    ## increments the current scan counter, and returns TRUE
    ## radR storage

    if (port$id != 1)
      return (FALSE)
    
    if (port$current.run <= 0 || port$current.scan >= port$run.toc$num.scans[port$current.run])
      return (FALSE)

    port$current.scan <- 1 + port$current.scan
    seek.scan(port, 0, port$current.scan)
    port$scan.info.waiting <- TRUE
    port$scan.data.waiting <- FALSE
    return (TRUE)
  }

  end.of.data.bma2 <- function(port) {
    ## have we hit the end of data for this port?
    ## segment

    if (port$id != 1)
      return (FALSE)
    
    if (port$current.run <= 0 || port$current.scan >= port$run.toc$num.scans[port$current.run])
      return (TRUE)

    return (FALSE)
  }

  get.scan.info.bma2 <- function(port) {
  ## gets the header information for the next scan to be read

    scan.info <- NULL
    try(scan.info <- get.object(port$file), silent=TRUE)
    if (is.null(scan.info)) {
      error <<- RSS$errors$eof_ON_PORT
      return (NULL)
    }
    int.time <- floor(scan.info$timestamp)
    scan.info$time.offset <- scan.info$timestamp - int.time
    scan.info$timestamp <- int.time
    class(scan.info$timestamp) <- "POSIXct"
    port$scan.info.waiting <- FALSE
    port$scan.data.waiting <- TRUE
    port$scan.info <- scan.info
    return (scan.info)
  }

  get.scan.data.bma2 <- function(port, ...) {
    ## paint the blips from the blip movie into the scan matrix
    ## and the class matrix, and the score matrix

    scan.dat <- NULL
    try(scan.dat <- get.object(port$file), silent=TRUE)
    if (is.null(scan.dat)) {
      error <<- RSS$errors$eof_ON_PORT
      RSS$num.blips <<- 0
      RSS$num.hot.samples <<- 0
      return (NULL)
    }
    RSS$have.valid$scan.data <<- FALSE
    RSS$have.valid$classification <<- FALSE
    RSS$have.valid$stats <<- FALSE
    RSS$have.valid$scores <<- FALSE
    RSS$have.valid$patches <<- FALSE

    mat.dim <- c(RSS$scan.info$pulses, RSS$scan.info$samples.per.pulse)
    if (!is.null(scan.dat$blip.geometry)) {
      rv <- .Call("raw_to_patch_buff", scan.dat$blip.geometry, RSS$patch.buffer, PACKAGE=BMA2.PACKAGE)
      RSS$num.blips <<- rv[1]
      RSS$num.hot.samples <<- rv[2]
      RSS$have.valid$classification <<- TRUE
      RSS$have.valid$patches <<- TRUE

      if (!is.null(scan.dat$blip.raw)) {
        .Call("raw_to_scan_mat", scan.dat$blip.raw, mat.dim, RSS$scan.mat, RSS$patch.buffer, PACKAGE=BMA2.PACKAGE)
        RSS$have.valid$scan.data <<- TRUE
      }

      if (!is.null(scan.dat$blip.score)) {
        .Call("raw_to_score_mat", scan.dat$blip.score, mat.dim, RSS$score.mat, RSS$patch.buffer, PACKAGE=BMA2.PACKAGE)
        RSS$have.valid$stats <<- TRUE
        RSS$have.valid$scores <<- TRUE
      }
    }
    return(RSS$scan.mat)
  }

  put.scan.bma2 <- function(port, scanmat) {
    
    ## compute the floating-point timestamp
    scan.info <- RSS$scan.info
    scan.info$timestamp <- scan.info$timestamp + scan.info$time.offset
    scan.info$time.offset <- NULL
    if (is.null(scan.info$elevation))
      scan.info$elevation <- 0
    if (is.null(RSS$source$radar.id))
      RSS$source$radar.id <- 0
    ## record the file offset for this scan
    scan.offset <- seek(port$file, NA, rw="w")

    ## record the scan info
    
    put.object(scan.info, port$file)

    ## record which data are available
    avail <- sum(unlist(data.types[c("blip.raw", "blip.geometry", "blip.score")[c(RSS$have.valid$sample.data, RSS$have.valid$classification, RSS$have.valid$stats && RSS$have.valid$scores)]]))

    ## append info for this scan to the scan.toc
    port$scan.toc <- rbind(port$scan.toc,
                           data.frame(run.id = port$current.run,
                                      radar.id = RSS$source$radar.id,
                                      avail.data = avail,
                                      scan.info[c("timestamp","duration","latitude","longitude","elevation")],
                                      radius = scan.info$samples.per.pulse * scan.info$sample.dist + scan.info$first.sample.dist,
                                      scan.offset = scan.offset))

    ## record the scan data
    data <- list()
    if (RSS$have.valid$patches)
      data$blip.geometry <- .Call("patch_buff_to_raw", RSS$patch.buffer, PACKAGE=BMA2.PACKAGE)
    if (RSS$have.valid$scan.data)
      data$blip.raw <-      .Call("scan_mat_to_raw",  RSS$patch.buffer, RSS$scan.mat, PACKAGE=BMA2.PACKAGE)
    if (RSS$have.valid$scores)
      data$blip.score <-    .Call("score_mat_to_raw",  RSS$patch.buffer, RSS$score.mat, PACKAGE=BMA2.PACKAGE)

    put.object(data, port$file)

    ## if necessary, flush the TOC
    if (dim(port$scan.toc)[1] >= max.toc.entries) {
      put.scan.toc(port)
      port$scan.toc <- NULL
    }
    flush(port$file)
    port$current.scan <- 1 + port$current.scan
  }
  
  seek.scan.bma2 <- function(port, run, scan) {
    ## seek to a particular run and scan on the port
    ## scan = which run, or 0 for the current scan
    ## run = which run, or 0 for the current run
    
    ## only implemented for the reader
    
    if (port$id != 1)
      return (TRUE)

    if (run > 0) {
      if (run > length(port$run.toc$first.scan.offset)) {
        error <<- RSS$errors$SEEK_BEYOND_ARCHIVE
        return (NULL)
      }
      if (scan < 1) {
        error <<- RSS$errors$INVALID_SCAN
        return (NULL)
      }
    } 
    if (scan < 1)
      scan <- port$current.scan
    if (run < 1)
      run <- port$current.run

    if (scan > port$run.toc$num.scans[run]) {
      error <<- RSS$errors$INVALID_SCAN
      return (NULL)
    }
    port$current.scan <- scan
    port$current.run <- run
    
    seek(port$file, port$scan.toc$scan.offset[port$run.toc$first.scan.offset[port$current.run] + port$current.scan], rw="r")
    return (TRUE)
  }    

  
  seek.time.bma2 <- function(port, time) {
    ## seek to the first scan at or after time
    ## FIXME: assumes that port$run.toc$start.time is in increasing order

    if (port$id != 1)
      return (TRUE)
    
    run <- which(port$run.toc$start.time > time)[1] - 1
    if (is.finite(run) && run > 0) {
      scan <- which(port$scan.toc$run.id == run && port$scan.toc$timestamp > time)[1] - 1
      if (is.finite(scan) && scan > 0) {
        seek.scan(port, run, scan)
        return (TRUE)
      }
    }
    error <<- RSS$errors$SEEK_BEYOND_ARCHIVE
    return (NULL)
  }

  
  start.up.bma2 <- function(port) {
    ## for a port, 
    if (port$type != "port")
      return (TRUE)

    if (port$id == 1) {
      ## disable the blip finding controls
      rss.gui(ENABLE_CONTROLS, "blip.finding", FALSE)
      ## make it look like we are blip finding
      RSS$blip.finding <- TRUE
      ## To indicate we are not learning, set the number of scans to learn to zero.
      RSS$scans.to.learn <<- 0
      ## tell the scan processor to skip the sapmle classifying 
      ## steps, since we are reading pre-digested data
      BMA2$save.restart.learning.after.stop <<- RSS$restart.learning.after.stop
      RSS$restart.learning.after.stop <<- FALSE
      RSS$skip$classify.samples <<- TRUE
      RSS$skip$update.stats <<- TRUE
      ## add a patch finding hook, which replaces the builtin patch finder
      rss.add.hook("FIND_PATCHES", BMA2.PACKAGE,
                   list(f=find.patches.bma2, enabled=TRUE, read.only=FALSE))
    } else {

    }
    return(TRUE);
  }

  shut.down.bma2 <- function(port) {
    ## shut down this port

    switch(port$id,
           "1" = {
             ## the reader
             RSS$skip$classify.samples <<- FALSE
             RSS$skip$update.stats <<- FALSE
             RSS$scans.to.learn <<- RSS$default.scans.to.learn
             RSS$restart.learning.after.stop <<- BMA2$save.restart.learning.after.stop 
             
             ## drop the patch finding hook
             rss.drop.hook("FIND_PATCHES", BMA2.PACKAGE)
             ## re-enable the blipping controls
             rss.gui(ENABLE_CONTROLS, "blip.finding", TRUE)
           },
           "2" = {
             ## the writer
             put.scan.toc(port)
           })
    close(port$file)
    return(TRUE)
  }
  
  start.run.bma2 <- function(port, start.time) {
    ## add a new run to the current sink
    ## and use it for subsequent put.scan calls
    if (port$id==2)
      port$current.run <- port$current.run + 1
    return(TRUE)
  }

  end.run.bma2 <- function(port, end.time) {
    ## end the current sink run and make any
    ## required changes to its directory
    if (port$id==2)
      put.scan.toc(port)
  }

  find.patches.bma2 <- function() {
    ## do nothing, since get.scan.data has already constructed
    ## the patches.
    ## This hook function simply prevents the usual patch
    ## finder from being called
  } 

  get.extra.methods.bma2 <- function(...) {
    ## so far, no extra methods for this module
    NULL
  }

  ## end of method definitions
  
}, BMA2)

## create global level BMA2 S3-methods from the methods in the BMA2 environment

for (n in ls(BMA2, pattern="\\.bma2$"))
  assign(n, BMA2[[n]])

## return the class object for this module

BMA2
