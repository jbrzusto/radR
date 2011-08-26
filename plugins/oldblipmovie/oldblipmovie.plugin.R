########
##
## OBSOLETE:  blipmovie.plugin.R
## SEE:       plugins/interfaces/blipmoviearch.plugin.R instead
##
## a radR plugin for saving raw data from blips in a playable archive
##
## a short machine-readable description of this plugin is available
## in the file blipmovie.desc.R
##
########

about = function() {
  if (num.scans > 0) {
    first <- first.timestamp
    class(first) <- "POSIXct"
    first <- paste("\nFirst scan: ",
                   format(first, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
    last <- last.timestamp
    class(last) <- "POSIXct"
    last <- paste("\nLatest scan: ",
                  format(last, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
  } else {
    first <- last <- ""
  }
  return(plugin.label %:% "\n\nVersion " %:% version %:%
         "\n\nNOTE:  this plugin, although it should still work, only supports a single run\n" %:%
         "per blipmovie archive.  You should use the C version using the To: and RECORD buttons" %:%
         "\n\nThis plugin obtains the timestamp for each scan from the SCAN_INFO hook.\n" %:%
         "Raw data for each blip is saved by the BLIP_HOOK.\n" %:%
         "The number of blips saved in each scan is written by the DONE_SCAN hook.\n" %:%
         "\nCurrent output files:\n" %:%
         ifelse(length(movie.basename) > 0,
                paste(movie.basename, c(".bma", ".blp", ".smp"), collapse="\n", sep=""),
                "not defined") %:%
         "\nNumber of scans written: " %:% num.scans %:%
         "\nNumber of blips written: " %:% num.blips %:%
         "\nNumber of samples written: " %:% num.samps %:%
         first %:% last %:%
         "\n\nCurrently " %:% ifelse(enabled, "", "not ") %:% "recording the blip movie." %:%
         "\n")
  
}

get.menus = function() {
  list(
       plugin = list(
         `Choose output file...` = list(
                          "file",
                          type="save",
                          title = "Choose a file for saving scans",
                          file.types = list(".bma" = "Blip movie archive", ".*" = "All files"),
                          init.file = function() movie.basename %:% ".bma",
                          init.dir =  function() movie.basename,
                          on.set = function(f) {
                            try.open.files(f, new=TRUE)
                          })
         )
       )
}

try.open.files = function(f, new = TRUE) {
  ## try open the given file, in append mode if new is FALSE
  ## otherwise, overwrite any existing file
  ## Also try to open the two accompanying files, "XXX_blip.YYY"
  ## and "XXX_samp.YYY"

  close.files()
  movie.basename <- basef <- strsplit(f, "\\.[a-zA-Z]*$")[[1]]
  scan.file <<- blip.file <<- samp.file <<- NULL
  ## if the file doesn't exist, treat this as a new open
  new <- new || !file.exists(f)
  try(scan.file <<- file(f, ifelse(new, "w+b", "r+b")), silent=TRUE)
  try(blip.file <<- file(basef %:% ".blp", ifelse(new, "w+b", "r+b")), silent=TRUE)
  try(samp.file <<- file(basef %:% ".smp", ifelse(new, "w+b", "r+b")), silent=TRUE)
  if (!is.null(scan.file) && !is.null(blip.file) && !is.null(samp.file)) {
    ## if these are new files, record zeroes in their count fields
    ## (the 8 byte double at the start of the file)
    if (new) {
      ## write the descriptive header
      writeBin(scan.file.header, scan.file)
      ## seek past the space alloted to the descriptive header
      seek(scan.file, 4096, rw="w")
      ## write the initial zero-count headers for the three files
      num.scans <<- 0
      first.timestamp <<- .Machine$integer.max
      last.timestamp <<- -.Machine$integer.max
      writeBin(as.integer(c(num.scans, first.timestamp, last.timestamp)), scan.file, 4, "little")
      num.blips <<- 0
      num.samps <<- 0
      writeBin(as.real(num.blips), blip.file, 8, "little")
      writeBin(as.real(num.samps), samp.file, 8, "little")
    } else {
      ## read the totals written to each file so far
      seek(scan.file, 4096, rw="r")
      header <- readBin(scan.file, "int", 3, 4, endian="little")
      num.scans <<- header[1]
      first.timestamp <<- header[2]
      last.timestamp <<- header[3]
      seek(scan.file, 0, origin="end", rw="w")
      
      seek(blip.file, 0, rw="r")
      num.blips <<- readBin(blip.file, "double", 1, 8, endian="little")
      seek(blip.file, 0, origin="end", rw="w")

      seek(samp.file, 0, rw="r")
      num.samps <<- readBin(samp.file, "double", 1, 8, endian="little")
      seek(samp.file, 0, origin="end", rw="w")
    }
    rss.enable.plugin("blipmovie")
  } else {
    try(close(scan.file), silent=TRUE)
    try(close(blip.file), silent=TRUE)
    try(close(samp.file), silent=TRUE)
    scan.file <<- blip.file <<- samp.file <<- NULL
    gui.error(paste("Unable to open one or more of files", paste(f, basef %:% ".blp", basef %:% ".smp", sep=", ")))
  }
}

close.files = function() {
  ## if any files are open,
  ## seek to the beginning
  ## to write the item counts
  ## then close them

  if (!is.null(scan.file)) {
    ## seek to the start, but past the alloted header space
    seek(scan.file, 4096, rw="w")
    writeBin(as.integer(c(BLIPMOVIE[c("num.scans", "first.timestamp", "last.timestamp")])), scan.file, 4, "little")
    close(scan.file)
    
    seek(blip.file, 0, rw="w")
    writeBin(as.real(num.blips), blip.file, 8, "little")
    close(blip.file)

    seek(samp.file, 0, rw="w")
    writeBin(as.real(num.samps), samp.file, 8, "little")
    close(samp.file)

    scan.file <<- blip.file <<- samp.file <<- NULL
  }         
}

load = function() {
  ## initialize state variables here
  num.scans <<- 0
  num.blips <<- 0
  num.samps <<- 0
  scan.file <<- blip.file <<- samp.file <<- NULL
}

unload = function(save.config) {
  close.files()
}

hooks = list(
  
  SCAN_INFO = list( enabled = FALSE, read.only = TRUE,
    f = function(info) {
      
      ## don't output blips unless we are playing

      if (RSS$play.state < RSS$PS$PLAYING)
        return (FALSE)
      
      scan.info <<- info

      ## adjust useful state variables
      
      first.timestamp <<- min(first.timestamp, info$timestamp)
      last.timestamp <<- max(last.timestamp, info$timestamp)

      ## keep track of where the blips for this scan begin
      first.blip.in.scan <<- num.blips
    }),
  
  BLIP = list( enabled = FALSE, read.only = TRUE,
    f = function(pulse, sample, value, z.score) {
      
      ## don't output blips unless we are playing
      ## but still flag the blip as accepted
      
      if (RSS$play.state < RSS$PS$PLAYING)
        return (TRUE)
      
      ## This hook requires the scan_info hook be enabled too.
      ## "unpack" the extmats into regular R vectors:
      pulse <- c(pulse[])
      sample <- c(sample[])
      value <- c(value[])

      ## write a record to the blip file
      ## index of first sample in this blip (as a double)
      writeBin(as.real(num.samps), blip.file, 8, "little")

      ## number of samples in this blip (as an int)
      writeBin(length(sample), blip.file, 4, "little")
      
      ## write the columns to the sample file
      
      writeBin(as.integer(c(pulse, sample, value)),  samp.file, 2, "little")

      ## keep track of what's been recorded
      
      num.blips <<- num.blips + 1
      num.samps <<- num.samps + length(sample)
      
      ## indicate that we are accepting the blip
      return(TRUE)
    }),

  DONE_SCAN = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      if (RSS$play.state < RSS$PS$PLAYING)
        return (TRUE)
      
      info <- scan.info
      ## record information about this scan

      ## DOUBLE: the timestamp, including fractional seconds
      writeBin(as.double(info$timestamp), scan.file, 8, "little")

      ## SHORT: the scan duration, (in milliseconds)
      ## SHORT: the number of pulses
      ## SHORT: the number of samples per pulse
      ## SHORT: the rotation direction (+1 = clockwise, -1 = counterclockwise)
      ## SHORT: the number of bits per sample
      
      writeBin(as.integer(c(info[c("duration", "pulses", "samples.per.pulse", "orientation")], 12)),
               scan.file, 2, "little")
      ## DOUBLE: the range per sample, in metres
      ## DOUBLE: the distance to the first sample, in metres
      ## DOUBLE: the angular offset (degrees) from north of the first pulse in the scan
      ## DOUBLE: the index of the first blip in this scan

      writeBin(as.double(c(info[c("sample.dist", "first.sample.dist", "bearing")], first.blip.in.scan)),
               scan.file, 8, "little")

      ## INT: the number of blips written for this scan in the scan file
      writeBin(as.integer(num.blips - first.blip.in.scan), scan.file, 4, "little")

      num.scans <<- 1 + num.scans
    }),

  ONPLAY = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## if there is no file open and there is a movie filename
      ## then open it for appending

      if (is.null(blip.file) && !is.null(movie.basename))
        try.open.files(movie.basename %:% ".bma", new=FALSE)
    }),

  ONPAUSE = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## close the blipmovie files when we pause.
      close.files()
    }),
  
  ONSTOP = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## close the blipmovie files when we stop.
      close.files()
    })

  )  ## END OF HOOKS

## state variables

blip.file          = NULL
samp.file          = NULL
scan.file          = NULL
first.blip.in.scan = NULL
first.timestamp    = NULL
last.timestamp     = NULL
num.blips          = NULL
num.samps          = NULL
num.scans          = NULL
scan.info          = NULL

## the .bma file preamble, which describes how to read blipmovie archives

scan.file.header = '## radR blipmovie scan file v 00.10  Binary data - do not edit!
## This is file 1 of 3:  you also need the XXX.blp and XXX.smp files

## sample R code for reading the scan information stored in this file:

## utility functions:

DOUBLE  <- function(f, n=1) readBin(f, "double", n, size=8, endian="little")
INT     <- function(f, n=1) readBin(f, "int",    n, size=4, endian="little")
SHORT   <- function(f, n=1) readBin(f, "int",    n, size=2, endian="little")
USHORT  <- function(f, n=1) readBin(f, "int",    n, size=2, signed=FALSE, endian="little")
DATE    <- function(x) {class(x)<-"POSIXct"; x}

## constants:
scan.preamble.size <- 4096
scan.header.size <- 12
scan.record.size <- 54  ## 5 DOUBLE + 5 SHORT + 1 INT
blip.header.size <- 8
blip.record.size <- 12
samp.header.size <- 8
samp.record.size <- 6
## set the timezone to GMT
Sys.setenv(TZ="GMT")

openFiles <- function(f) {
  ## f is the basename of the blip movie files
  ## i.e. the part before the ".bma"

  f1 <<- file(paste(f, ".bma", sep=""), "rb")
  f2 <<- file(paste(f, ".blp", sep=""), "rb")
  f3 <<- file(paste(f, ".smp", sep=""), "rb")

  ## skip the preamble (this text)
  seek(f1, scan.preamble.size)

  ## read the header details
  ## from the scan file:
  return(list(
              num.scans = INT(f1),
              first.scan.timestamp = DATE(INT(f1)),
              last.scan.timestamp = DATE(INT(f1)),
              ## from the blip file:
              total.num.blips = DOUBLE(f2),
              ## and from the samples file:
              total.num.samples = DOUBLE(f3)))
}

## a function to read data for the nth scan, n=1, 2, ..., num.scans
getScan <- function(n) {
## seek to the nth scan, skipping the descriptive and machine headers
  seek(f1, scan.preamble.size + scan.header.size + (n-1) * scan.record.size)
  return(list(
   timestamp           = DOUBLE(f1),
   duration            = SHORT(f1),
   pulses              = SHORT(f1),
   samples.per.pulse   = SHORT(f1),
   orientation  = SHORT(f1),
   bits.per.sample     = SHORT(f1),
   sample.dist         = DOUBLE(f1),
   first.sample.dist   = DOUBLE(f1),
   bearing        = DOUBLE(f1),
   first.blip          = DOUBLE(f1),
   num.blips           = INT(f1)
 ))
}

## a function to read data for the nth blip, n=1, 2, ..., total.num.blips
getBlip <- function(n) {
## seek to the nth blip, skipping the header
  seek(f2, blip.header.size + (n-1) * blip.record.size)
  return(list(first.sample=DOUBLE(f2),
              num.samples=INT(f2)
    ))
}

## a function to read the samples data for the nth blip, n=1, 2, ... total.num.blips
## returns a three column matrix: pulse, sample, value
getBlipSamples <- function(n) {
## get the nth blip
  b <- getBlip(n)

## seek to the first sample, skipping the header

  seek(f3, samp.header.size + b$first.sample * samp.record.size)
## read the pulse and samples numbers and the
## values of the corresponding samples
  pulse.num <- USHORT(f3, b$num.samples)
  sample.num <- USHORT(f3, b$num.samples)
  value <- USHORT(f3, b$num.samples)
  return(cbind(pulse.num, sample.num, value))
}

cat(\"\n\nFunctions for blipmovie archives: \n
openFiles(f)      opens the three blip movie archive files with basename f\n
getScan(n)        gets info on the n-th scan in the archive\n
getBlip(n)        gets info on the n-th blip in the archive\n
getBlipSamples(n) returns the raw sample data for the n-th blip\n

e.g.

openFiles("bmovie")

## to get the samples for the all blips in the 100th scan:

scan <- getScan(100)
samps <- data.frame()
for(i in 1:scan$num.blips)
  samps <- rbind(samps, cbind(i, getBlipSamples(scan$first.blip + i -1)))

names(samps) <- c("blip.num", "pulse.num", "sample.num", "value")
samps$theta <- pi / 2 - pi * (scan$bearing + scan$orientation * 360 * (samps$pulse.num - 1) / scan$pulses) / 180
samps$range <- scan$first.sample.dist + scan$sample.dist * samps$sample.num
samps$x <- samps$range * cos(samps$theta)
samps$y <- samps$range * sin(samps$theta)
\n\")


' ## endquote for the header string
