##  svn $Id: xir3000arch.plugin.R 740 2011-02-10 16:30:50Z john $
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


##           XIR3000ARCH   PLUGIN
##                                                         
##  Read files recorded by Russell Technologies Inc.'s
##  radar digitizing software.       


MYCLASS = "xir3000arch"

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
                        can.specify.start.time = TRUE,
                        config = list(filename = NULL, version = "DSP9"),
                        file.ext = "rec",
                        has.toc = TRUE,
                        cur.run = 0,
                        cur.scan = 0,
                        prev.scan = -1,
                        contents = empty.TOC,
                        start.time = NULL,                 # the start time of the first scan
                        start.time.midnight = NULL,        # midnight of the day of the first scan (since the UTC field is just HH:MM:SS)
                        seqnos = integer(0),               # the sequence numbers of all files in the archive
                        seqno.digits = 8,                  # how many digits (including zero padding) when sequence numbers are embedded in file names?
                        si = NULL,                         # scan info
                        file.basename = "",                # path + file basename, to which XXXXXXXX.rec are appended
                        cur.file.data = NULL,              # raw vector of the contents of the REC file for the current scan
                        next.file.data = NULL,             # raw vector of the contents of the REC file for the next scan; needed to calculate durations
                        next.file.si = NULL,               # scan info vector for next REC file
                        next.scan = 0,                     # index of scan for next.file.data and next.file.si
                        default.duration = NULL,           # default duration of scan, in milliseconds
                        max.rangeind = 0                   # the maximum range index (in the range 0...8) for the current scan
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()

  ##                    name    id  source  sink
  rv[[1]] <- make.port("Reader", 1,  TRUE, FALSE)
  rv
}

load = function() {
  rss.dyn.load(MYCLASS, in.dir.of=plugin.file)
  default.start.time <<- structure(as.numeric(Sys.time()), class="POSIXct")
}

unload = function(save.config) {
  rss.dyn.unload(MYCLASS)
}

get.menus = function() {
  list(
       sources = list (
         titles = "Xir3000arch reader",
         menu = list (
           options = "no-tearoff",
           "Choose a file..." = gui.create.port.file.selector(get.ports()[[1]])
           )
         ),
       plugin = list (
         "Save start time and sweep duration for this archive in _metafile.R" = save.metafile,
         "---",
         list ("datetime",
               label = "date and time at start of first sweep",
               value = as.numeric(default.start.time),
               on.set = function(x) {
                 t <- structure(x, class="POSIXct")
                 default.start.time <<- t
                 if (inherits(RSS$source, MYCLASS))
                   RSS$source$start.time <- t
               }
               ),
         list ("gauge",
               label = "antenna speed in RPM" ,
               range = c(1, 200),
               increment = 0.5,
               value = antenna.rpm,
               on.set = function(x) { antenna.rpm <<- x; }
               ),
         "DSP version (for old files recorded with RIB hardware):",
         c(list(option="choose.one",
                on.set = function(n) {
                  current.DSP.version <<- available.DSP.versions[n]
                  if (RSS$play.state <= RSS$PS$PLAYING && inherits(RSS$source, MYCLASS)) {
                    ## update the current scan, since we're the source and the scale may have changed
                    RSS$scan.info$sample.dist <- get.sample.dist(RSS$source)
                    rss.gui(UPDATE_PLOT_WINDOW, TRUE)
                  }
                }),
           structure(current.DSP.version == available.DSP.versions, names=available.DSP.versions)
           )
         )
       )
}

get.sample.dist <- function(p) {
  ## return the range-cell size for the given port
  range.values[[current.DSP.version]][1 + p$max.rangeind] / RTI.defaults$samples.per.pulse
}

globals = list (

  as.character.xir3000arch = function(x, ...) {
    sprintf("radR interface port: %s: %s: %s",
            MYCLASS,
            x$name,
            if (is.null(x$config$filename)) "(no file)" else x$config$filename
            )
  },
  
  print.xir3000arch = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.xir3000arch = function(port, ...) {
    ## This port has only "filename" as a configuration option.
    ## Each radar scan is stored in a file in the same directory as
    ## "filename", but whose name ends in XXXXXXXX.rec,
    ## where XXXXXXXX is a maximal zero-padded digit sequence.
    
    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               filename = {
                 port$config$filename <- opts[[opt]]
                 ## separate the base filename part from the sequence number and file extension
                 ## we assume the sequence number is the longest string of digits immediately before
                 ## the ".rec" suffix.
                 split <- regexpr("(?i)(?=[0-9]+\\.rec)", opts[[opt]], perl=TRUE)
                 port$file.basename <- substring(opts[[opt]], 1, split - 1)
                 ## grab the first sequence number
                 seqno = sub("(?i)\\.rec$", "", substring(opts[[opt]], split), perl=TRUE)
                 port$seqno.digits = nchar(seqno)
                 port$seqnos <- as.integer(seqno)
                 port$contents <- empty.TOC  ## mark the table of contents as needing regeneration
               },
               {
                 rss.plugin.error("xir3000arch: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

  get.contents.xir3000arch = function(port, ...) {
    return(port$contents)
  },

  end.of.data.xir3000arch = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    port$cur.scan >= port$contents$num.scans[port$cur.run]
  },

  get.filename = function(port, scan=port$cur.scan) {
    ## get the filename for the given port and scan number
    sprintf(paste("%s%0", port$seqno.digits, "d.rec", sep=""), port$file.basename, port$seqnos[scan])
  },
    
  get.scan.info.xir3000arch = function(port, ...) {
    ## gets the header information for the next scan
    ## This can include NMEA data.

    ## For simplicity, we read the whole file corresponding to the
    ## next scan, which includes data, and cache it for the likely
    ## imminent call to get.scan.data

    ## Also, because other methods of calculating scan duration are
    ## proving unreliable, we now pre-read the following scan too, so
    ## that we can can diff the timestamps to get duration.

    ## We read either the next or the next two scans, depending on whether we
    ## already have the (appropriate) next scan.

    have.next = port$next.scan == port$cur.scan + 1 && ! is.null(port$next.file.data)

    if (!have.next)
      port$next.scan = port$cur.scan
    
    for (jj in 1:(1 + !have.next)) {
      port$cur.scan = port$next.scan
      port$next.scan = port$next.scan + 1
      port$cur.file.data = port$next.file.data
      port$si = port$next.file.si

      if (port$next.scan <= length(port$seqnos)) {
        f = get.filename(port, port$next.scan)
        port$next.file.data <- rss.read.file.as.raw(f)

        ## grab the file magic and recording type
        sig <- readBin(port$next.file.data, "integer", 2)
        if (sig[1] != file.magic) {
          warning("xir3000arch: the file ", f, " is not a valid RTI .REC file")
          return (NULL)
        }
        if (!sig[2] %in% supported.recording.types) {
          warning("xir3000arch: the file ", f, " has data with RECORDING_TYPE = ", sig[2], ", which is not supported by this plugin")
          return (NULL)
        }

        x = .Call("get_scan_info", port$next.file.data, PACKAGE=MYCLASS)

        port$max.rangeind = x[SIN$MaxRangeIndex]

        port$next.file.si = list (
          pulses = x[SIN$Pulses],  ## this should equal RTI.default$pulses, but we keep it flexible for now
          
          samples.per.pulse = if (!is.na(x[SIN$SamplesPerPulse])) {
            x[SIN$SamplesPerPulse]
          } else {
            RTI.defaults$samples.per.pulse
          },
          
          bits.per.sample = RTI.defaults$bits.per.sample,
          
          timestamp = structure(
            if (is.timestamp.ok(x[SIN$TimeStamp])) {
              x[SIN$TimeStamp]
            } else if (is.timestamp.ok(port$start.time.midnight + x[SIN$UTC])) {
              port$start.time.midnight + x[SIN$UTC]
            } else {
              port$start.time + (port$next.scan - 1) * port$default.duration / 1000
            },
            class="POSIXct"),
          
          duration = 0, ## filled in below, after we have two consecutive scans

          sample.dist =
          if (!is.na(x[SIN$SampleDist])) {
            x[SIN$SampleDist]
          } else {
            distget.sample.dist(port)
          },
          
          first.sample.dist = 0,
          
          bearing = if (!is.na(x[SIN$TrueHeading])) x[SIN$TrueHeading] else default.heading,
          
          orientation = +1,

          antenna.lat = if (!is.na(x[SIN$Latitude])) x[SIN$Latitude] / 1e7,
          
          antenna.long = if (!is.na(x[SIN$Longitude])) x[SIN$Longitude] / 1e7
          
          )
      } else {
        ## there is no next scan, so set timestamp of (bogus) next scan so that
        ## current scan gets default duration
        
        port$next.file.si$timestamp = port$si$timestamp + port$default.duration / 1000
      }
    }
    port$si$duration = diff(as.numeric(c(port$si$timestamp, port$next.file.si$timestamp))) * 1000
    return(port$si)
  },

  get.scan.data.xir3000arch = function(port, extmat, ...) {
    ## reads the data for the current scan
    ## This is the second element of port$cur.file.data
    ## which is assigned by get.scan.info

    dim <- c(port$si$samples.per.pulse, port$si$pulses)

    if (is.null(dim))
      stop("calling get.scan.data when RSS$scan.info has NULL dimension info")
    
    dim(extmat) <- dim
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim

    .Call("get_scan_data", port$cur.file.data, port$max.rangeind, extmat, PACKAGE=MYCLASS)
    return(extmat)
  },

  seek.scan.xir3000arch = function(port, run, scan, ...) {
    ## seek to a particular run and scan on the current source
    ## scan = integer NAN requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)
    ## run = 0 represents the current run; but we only allow one run per xir3000 archive for now
    port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
  },

  seek.time.xir3000arch = function(port, time, ...) {
    ## seek to the first scan at or after time in the current source
    ## scan = +Inf requests a seek past the last scan in
    ##        the current run (which must be the last run
    ##        for most modules)

    ## FIXME: do this

    return (NULL)
  },

  start.up.xir3000arch = function(port, ...) {
    ## determine how many scans are in the archive, and
    ## create a table of contents

    ## The contents will be treated as a single run.  Depending on
    ## the configuration option use.all.folder.files, we take either
    ## the entire folder or all scans starting with the selected file
    ## as the run.
    ##
    ## Returns a one-row dataframe with three elements:
    ## num.scans:   number of scans in the run
    ## start.time:  timestamp first scan
    ## end.time:    timestamp of last scan
    ##
    ## We open the first, second and last files in the run to extract their
    ## timestamps and compute total run time and scan duration.
    ## If no timestamps are available, 

    run.files <- dir(path=dirname(port$config$filename), pattern=basename(port$file.basename) %:% "[0-9]+\\.rec$")
    
    ## get all the sequence numbers so we don't have to worry about missing ones later on

    split <- regexpr("(?i)(?=[0-9]+\\.rec)", run.files[1], perl=TRUE)
    seqnos <- as.integer(sub("(?i)\\.rec$", "", substring(run.files, split[1]), perl=TRUE))

    ## if configured to, drop those before the first scan (i.e. before
    ## the file given by port$config$filename)
    
    port$seqnos <- seqnos[seqnos >= port$seqnos | use.all.folder.files]
  
    ns <- length(port$seqnos)
    
    seek.scan(port, 1, 1)
    port$default.duration <- 60 / antenna.rpm * 1000
    si.first <- get.scan.info(port)

    bad = 0

    if (! is.timestamp.ok(si.first$timestamp)) {
      ## there are no (valid) timestamps in these files, we try to guess one from the file basename
      ## except that if we've already recorded this info in a "metadata.R" file, we use that
      metafile <- get.metafile.name(port$config$filename)
      if (file.exists(metafile)) {
        vars <- rss.source.as.list(metafile)
        port$start.time <- structure(vars$start.time, class="POSIXct")
        port$default.duration <- vars$duration
      } else {
        split <- regexpr(paste("(?=", date.guess.regexp, ")", sep=""), port$file.basename, perl=TRUE)
        if (split[1] != -1) {
          port$start.time <- strptime(substring(port$file.basename, split), date.guess.format)
        } else {
          rss.gui(POPUP_MESSAGEBOX, "Unable to determine timestamps", "The RTI .REC files in this archive do not contain timestamps,\nand I'm unable to guess the date of the first file from its name.\nI will use the default start date and antenna rotation speed to assign timestamps to sweeps.\nYou can set these from the menu:\n Plugins | xir3000arch | Controls...\nand they will take effect the next time you open this archive.")
          port$start.time <- default.start.time
        }
      }
      si.first$timestamp <- port$start.time
      si.last <- list(timestamp = port$start.time + (ns - 1) * port$default.duration / 1000)
    } else {
      if (ns > 1) {
        seek.scan(port, 1, 2)
        si.2nd <- get.scan.info(port)
        if (is.null(si.2nd)) {
          si.last = si.first
        } else {
          port$default.duration <- as.numeric(si.2nd$timestamp - si.first$timestamp) * 1000
          repeat {
            seek.scan(port, 1, ns)
            si.last <- get.scan.info(port)
            if (! is.null(si.last))
              break;
            ns = ns - 1
            bad = bad + 1
            if (ns == 1) {
              si.last = si.first
              break;
            }
          };
        }
      } else {
        si.last <- si.first
      }
    }
    if (bad > 0)
      warning(sprintf("The last %d .REC files in this folder are invalid; I'm ignoring them\n", bad))
    
    port$seqnos = port$seqnos[1:ns]
    ## workaround R bug: as.POSIXct leaves "POSIXt" as part of the class.
    port$contents <- list (
                          num.scans = ns,
                          start.time = structure(as.POSIXct(si.first$timestamp),class="POSIXct"),
                          end.time = structure(as.POSIXct(si.last$timestamp), class="POSIXct")
                           )

    port$start.time.midnight <- structure(as.POSIXct(trunc.POSIXt(si.first$timestamp, "days")), class="POSIXct")
    port$cur.run <- 1
    return (TRUE)
  },

  shut.down.xir3000arch = function(port, ...) {
    ## do whatever is required to minimize
    ## resource consumption by this port
    ## eg. stopping digitization and playback,
    ## closing files, etc.

    ## drop the current file data
    port$cur.file.data <- NULL
    port$default.duration <- NULL
    port$start.time <- NULL
    return(TRUE)
  },

  new.play.state.xir3000arch = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }
  
  )  ## end of globals

## additional plugin variables

## return a suitable metafile name for an archive specified by the name of a data file
get.metafile.name = function(datafilename) {
  file.path(dirname(datafilename), "_metadata.R")
}

## see whether a purported timestamp is reasonable
## i.e. is a valid number and on or after 1 Jan 1990
is.timestamp.ok = function(x) {
  x <- as.numeric(x)
  return (length(x) == 1 && is.finite(x) && x >= 631152000)
}

## save metadata for this archive

save.metafile = function() {
  if (!inherits(RSS$source, MYCLASS)) {
    rss.gui(POPUP_MESSAGEBOX, "Select an xir3000 archive first", "The current source is not an xir3000 archive, so I can't save its metadata.")
    return()
  }
  mf <- get.metafile.name(RSS$source$config$filename)
  cat (sprintf("## meta data for archive %s\n\n## timestamp of first sweep (GMT)\nstart.time = %.3f\n\n## duration of sweeps (milliseconds)\nduration = %.3f\n", dirname(RSS$source$config$filename), as.numeric(default.start.time), 60 / antenna.rpm * 1000), file=mf)
  rss.gui(POPUP_MESSAGEBOX, "Metadata saved", "Sweep start time and sweep duration were saved to file '_metadata.R'\nThey will be used each time this archive is opened, unless you delete the file.")
}

## what a xir3000arch table of contents looks like, initially
empty.TOC = list(
  start.time = structure(double(0), class = "POSIXct"),
  end.time = structure(double(0), class = "POSIXct"),
  num.scans = integer(0)
  )

## the ranges associated with the "range index".  This depends on which
## DSP version the file was recorded with, which is apparently not saved
## in the file, and so must be selected by the user.
## The values are from the documentation of CSAPI_GetRange, and are converted
## from nautical miles to metres by the factor of 1852.  
## Because R arrays are indexed from 1, we must add 1 to the CSAPI range index
## to get the index into the appropriate vector here:
## e.g. with DSP9 and a CSAPI range index of 2, the true range is range.values$DSP9[3]

range.values = list (
  DSP7 = 0.125 * 2^(0:8) * 1852,
  DSP9 = 0.375 * 2^(0:8) * 1852
  )

## the recording type(s) supported by this plugin. We support types:
##
##   REC_TYPE_RLC_2 = 4
##   REC_TYPE_RLC_3 = 5
##   REC_TYPE_RLC_4 = 6

supported.recording.types = 4:6

## the indexes of items returned by the C function get_scan_info

SIN = list (
  ## these come from the sensor data structure
  Longitude                         =  1,
  Latitude                          =  2,
  Depth                             =  3,
  DistTransToWaterLine              =  4,
  DistTransToKeel                   =  5,
  TrueHeading                       =  6,
  MagnHeading                       =  7,
  MagnVariation                     =  8,
  MagDeviation                      =  9,
  TrueTrackGround                   = 10,
  MagnTrackGround                   = 11,
  SpeedWater                        = 12,
  SpeedGround                       = 13,
  DriftSpeed_Water                  = 14,
  DriftSpeedGround                  = 15,
  UTC                               = 16,
  WaterTemperature                  = 17,
  GPSHorizontalDiluation            = 18,
  GPSAntennaAltitude                = 19,
  GPSGeoidalSeparation              = 20,
  GPSAgeOfDifferentialData          = 21,
  GPSNumberOfSatellites             = 22,
  GPSDifferentialReferenceStationID = 23,
  AntennaPulseLength                = 24,
  AntennaPowerInPort                = 25,
  AntennaPowerOutPort               = 26,
  AntennaMagnetronCurrent           = 27,
  AntennaRMonitor                   = 28,
  AntennaState                      = 29,
  AntennaAlarm                      = 30,
  SpeedWind                         = 31,
  WindAngleRel                      = 32,
  WindAngleTrueInDeg                = 33,
  ## these are inferred from the data segments
  MaxRangeIndex                     = 34,
  ## this is inferred from the data file, or explicitly given there in later recording types
  Pulses                            = 35,
  ## this is 512 for earlier recording types, or specified in the file for later types
  SamplesPerPulse                   = 36,
  ## Timestamp for start of scan (for REC_TYPE >= RLC_4)
  TimeStamp                         = 37,
  ## Duration in milliseconds (for REC_TYPE >= RLC_4)
  Duration                          = 38,
  ## Range cell size, in m (for REC_TYPE >= RLC_4)
  SampleDist                        = 39
  )

## when we can't get the time from a UTC value, and can't guess it from the file.basename,
## use the following variable as the default starting time; there is a control to change
## it in the plugin's menu.

default.start.time = NULL
