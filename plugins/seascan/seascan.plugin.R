########################################################################
###                                                                  ###
### seascan.plugin.radR                                              ###
###                                                                  ###
### the seascan plugin, for interfacing to the SeaScan radar control ###
### server via its Client12.dll                                      ###
###                                                                  ###
### indexes have origin 1 in R, origin 0 in C                        ###
###                                                                  ###
### By John Brzustowski, 2006  jbrzusto AT fastmail DOT fm           ###
########################################################################

## define the name of this interface plugin's class
## this will be the class of its ports
MYCLASS = "seascan"

about = function() {
  rv <- "Interface for the Rutter Seascan radar data server.\n\nSeaScan server " %:% (if (is.null(server.handle)) "NOT") %:% " started."
  rv <- rv
  bin <- seascan.dump.BIN()
  x <- bin["ACPs"]
  names(x)[1] <- "Azimuth resolution"
  if (!is.null(RSS$source) && inherits(RSS$source, "seascan")) {
    x <- c(x, seascan.get.proc(RSS$source))
  } else {
    rv <- rv %:% "\n\nNote: To see more SeaScan settings in this box,\nuse player:From to select a SeaScan server."
  }
  rv <- rv %:% "\n\n    Current Settings\n(assuming SeaScan was started with the SeaScan0.bin in the radR folder)\n\n" %:%
    paste(sprintf("   %30s = ", names(x)), x, sep="", collapse="\n")
  return(rv)
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "Seascan",
               menu = list (
                 options = "no-tearoff",
                 list (
                       "choose.one",
                       on.set = function(i) {
                         rss.set.port(all.ports[[i]])
                       },
                       group = "group.play.source.menu"
                       ))
               ),
             plugin = list (
               list (gain = "gauge",
                     label = "gain", 
                     range = c(0, 255),
                     increment = 5,
                     value = ANTENNA$parms$antenna.plugins$seascan$gain,
                     on.set = function(x) { ANTENNA$parms$antenna.plugins$seascan$gain <<- x; update.agc() }
                     ),
               list (offset = "gauge",
                     label = "offset",
                     range = c(0, 255),
                     increment = 5,
                     value = ANTENNA$parms$antenna.plugins$seascan$offset,
                     on.set = function(x) { ANTENNA$parms$antenna.plugins$seascan$offset <<- x; update.agc() }
                     ),
               "Choose range and resolution..." = seascan.choose.modeline,
               "---",
               "(Re)start SeaScan server now" = restart.server,
               list("choose.any",
                    on.set = function(n, v) {
                      start.and.stop.server <<- v
                    },
                    "Start/stop SeaScan server when radR starts/stops" = start.and.stop.server
                    ),
               "---",
               "Create SeaScan0.bin from radar description file..." = seascan.gui.create.binfile
               )
             )
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], sapply(all.ports, function(x) x$name))
  return(rv)
}

load = function(...) {
  client.lib <- file.path(client.lib.dir, client.lib.name)
  if (!file.exists(client.lib)) {
    client.lib <- rss.gui(FILE_DIALOG, "open.one", "SeaScan plugin: Where is " %:% client.lib.name %:% "?",
                          types = list(".dll" = "Dynamic libs", ".*" = "All files"),
                          init.file = client.lib)
    if (length(client.lib) == 1 && nchar(client.lib[1]) > 0) {
      client.lib.dir <<- dirname(client.lib)
      if (tolower(basename(client.lib)) != tolower(client.lib.name)) {
        file.copy(client.lib, file.path(dirname(client.lib), client.lib.name))
        rss.gui(POPUP_MESSAGEBOX, "Client library cloned",
                "The seascan plugin requires that the SeaScan client library be called '" %:% client.lib.name %:%
                "'\n, so I have cloned the library you indicated, giving the new copy this name.", time.to.live=30)
      }
    } else {
      return(NA)
    }
  }
  Sys.setenv(PATH=paste(Sys.getenv("PATH"), client.lib.dir, sep=";"))
  rss.gui(POPUP_MESSAGEBOX, "Loading Seascan Client", "This may take a minute, as the client library\nprobes the network for servers...",
          "seascanstart", 0)
  rss.dyn.load("seascan", in.dir.of=plugin.file, local=TRUE)
  rss.gui(DELETE_MESSAGEBOX, "seascanstart")
  if (start.and.stop.server)
    restart.server()
  all.ports <<- get.ports()
}

unload = function(...) {
  rss.dyn.unload("seascan")
  if (!is.null(server.handle) && start.and.stop.server) {
    .Call("end_process", server.handle, PACKAGE="radR")
    server.handle <<- NULL
  }
}

restart.server = function() {
  ## start the seascan server, shutting down the existing instance, if any
  
  .Call("end_process", server.handle, PACKAGE="radR")
  server.handle <<- NULL

  if (inherits(RSS$source, "seascan"))
    rss.set.no.port("source")
  
  rss.gui(UPDATE_GUI)
  if (!file.exists(server.path)) 
    server.path <<- rss.gui(FILE_DIALOG, "open.one", "SeaScan plugin: Where is SeaScan.exe?",
                           types = list(".exe" = "Programs", ".*" = "All files"),
                           init.file = server.path)

  if (!is.null(server.path))
    try (server.handle <<- .Call("start_process",
                                 c(server.path, file.path(getwd(), "plugins/seascan")),
                                 PACKAGE="radR"))
  if (is.null(server.handle)) {
    rss.gui(POPUP_MESSAGEBOX, "Unable to start the server SeaScan.exe",
    "SeaScan.exe did not start.\nDid you specify the correct value for server.path in the file seascan.conf.R?\nThe current value is '" %:% server.path %:% "'\nIf you reload this plugin, you can browse for the location of SeaScan.exe.")
  }
}

update.agc = function(p = RSS$source) {
  ## inform the server of (possibly) new gain and/or offset values
  if (inherits(p, "seascan"))
    .Call("set_agc_params", as.integer(p$host.id - 1), as.integer(c(FALSE, ANTENNA$parms$antenna.plugins$seascan[c("gain", "offset")])), PACKAGE=MYCLASS)
}
          
get.hosts = function() {
  ## return a list of possible SeaScan server hosts
  ## this is a character vector
  tolower(.Call("get_hosts", as.integer(ignore.unknown.hosts), known.hosts, PACKAGE=MYCLASS))
}

get.ports = function() {

  ## return a list of port objects for this interface
  ## each port is a strictenv with at least these symbols
  ## name - a printable label describing this port
  ## id - to distinguish multiple ports from the same interface class
  ## is.source
  ## is.sink
  ## is.file
  ## is.seekable  ## random-access is possible
  ## can.specify.start.time  ## start.up can specify a timestamp
  ## has.toc ## source has list of playback sequences

  make.port <- function(name, host.id, id, is.source, is.sink, is.file, is.live,
                        is.seekable, can.specify.start.time, has.toc) {
    structure(strictenv(
                        name = name,
                        host.id = host.id,
                        id = id,
                        is.source = is.source,
                        is.sink = is.sink,
                        is.live = is.live,
                        is.file = is.file,
                        is.seekable = is.seekable,
                        can.specify.start.time = can.specify.start.time,
                        has.toc = has.toc,
                        config = list(start.time=0)
                        ),
              class = c(MYCLASS, "strictenv"))
  }

  rv <- list()
  hosts <- get.hosts()
  ## if necessary, mark unknown hosts as NA
  if (ignore.unknown.hosts)
    hosts [! hosts %in% tolower(known.hosts)] <- NA

  n <- 0
  for (h in seq(along=hosts)) {
    if (!is.na(hosts[h])) {
      ##                               name                   host.id  id source  sink  file   live   seek  spec.start, TOC
      rv[[n+1]] <- make.port(hosts[h] %:% ": Live Radar-0",    h,      1,  TRUE, FALSE, FALSE, TRUE,  FALSE, FALSE,     FALSE )
      rv[[n+2]] <- make.port(hosts[h] %:% ": Live Radar-1",    h,      2,  TRUE, FALSE, FALSE, TRUE,  FALSE, FALSE,     FALSE )
      rv[[n+3]] <- make.port(hosts[h] %:% ": Tape Playback-0", h,      3,  TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,      TRUE  )
      rv[[n+4]] <- make.port(hosts[h] %:% ": Tape Playback-1", h,      4,  TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,      TRUE  )
      n <- n + 4
    }
  }
  rv
}

hooks = list(               
  ONPLAY = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      if (inherits(RSS$source, "seascan"))
        .Call("flush_seascan_buffers", RSS$source$host.id - 1)
    })
  )
  


## global methods and variables

globals = list (
  
  as.character.seascan = function(x, ...) {
    "radR interface port: " %:% MYCLASS %:% ": " %:% x$name
  },
  
  print.seascan = function(x, ...) {
    ## print a description of this object
    cat (as.character(x) %:% "\n")
  },

  config.seascan = function(port, ...) {
    opts <- list(...)
    if (length(opts) != 0) {
      for (opt in names(opts)) {
        switch(opt,
               start.time = {
                 if (port$id < 3) {
                   rss.plugin.error("seascan: cannot set start time for live radar")
                   return(NULL)
                 }
                 port$config$start.time <- opts[[opt]]
                 ## for the tape playbacks, setting the start time requires
                 ## stopping then restarting playback.
                 shut.down(port)
                 start.up(port)
               },
               {
                 rss.plugin.error("seascan: unknown configuration option for port: " %:% opt)
                 return(NULL)
               }
               )
      }
    }
    return(port$config)
  },

  get.contents.seascan = function(port, ...) {
    ## gets the contents of the port, namely
    ## a list with three elements
    ## num.images: number of images in each run
    ## run.start:  starting timestamp of each run
    ## run.end:    ending timestamp of each run

    if (!is.null(x <- .Call("get_contents", as.integer(port$host.id - 1), as.integer(port$id - 1), PACKAGE=MYCLASS))) {
      num.runs <- length(x) / 3
      rv <- list(num.scans=  x[seq(along=x) %% 3 == 1],
                 start.time= x[seq(along=x) %% 3 == 2],
                 end.time=   x[seq(along=x) %% 3 == 0])
      class(rv$start.time) <- class(rv$end.time) <- "POSIXct"
      return(rv)
    }
    return(NULL)
  },

  end.of.data.seascan = function(port, ...) {
    ## have we hit the end of data for this port?
    ## this will usually mean the end of playback on a tape
    ## segment
    .Call("end_of_data", as.integer(port$host.id - 1), as.integer(port$id - 1), PACKAGE=MYCLASS)
  },
  
  get.scan.info.seascan = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next available scan
    
    return(.Call("get_scan_info", as.integer(port$host.id - 1), as.integer(port$id - 1), scan.info.names, scan.mat, trv, trv.index, PACKAGE=MYCLASS))
  },

  get.scan.data.seascan = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does all the work, so we just pass the port

    if (is.null(.Call("get_scan_data", as.integer(port$host.id - 1), PACKAGE=MYCLASS)))
      return (NULL)

    return (extmat)
  },
  
  put.scan.seascan = function(port, header, extmat, ...) {
    ## not implemented yet; in fact, never will be.  The
    ## server does not accept data from clients for recording.
    return(NULL)
  },

  seek.scan.seascan = function(port, run, scan, ...) {
    ## We don't implement seeking on seascan.
    ## Although archive playback can be set to start
    ## at a particular time, there is too much time slop
    ## to allow clean seeking via this mechanism.
    ## Therefore, playback start time can be set using
    ## config(..., start.time=), before doing start.up
    ## For accurate playback of seascan archives, use the
    ## seascanarch plugin, which reads the files directly.
    return(NULL)
  },

  seek.time.seascan = function(port, time, ...) {
    ## We don't implement seeking on seascan.
    return(NULL)
  },

  start.up.seascan = function(port, ...) {
    ## start up digitizing or playback on the appropriate host
    ## using the configured start time
    rv <- .Call("start_up", as.integer(port$host.id - 1), as.integer(port$id - 1), as.integer(port$config$start.time),
                PACKAGE=MYCLASS)
    if (port$id < 3) {
      ## for a live radar, update mode settings 
      seascan.set.modeline(port, current.modeline)
    }
    return(rv)
  },

  shut.down.seascan = function(port, ...) {
    ## shut down the operation on the specified port or host
    ## Since only one port can be active per host (i.e. either playback or live)
    ## we just pass the host id, and the library will shut down whatever
    ## port is active on it.
    
    .Call("shut_down", as.integer(port$host.id - 1), PACKAGE=MYCLASS)
  },

  start.run.seascan = function(port, start.time, ...) {
    ## unimplemented
  },

  end.run.seascan = function(port, end.time, ...) {
    ## unimplemented
  },

  new.play.state.seascan = function(port, new.state, old.state, ...) {
    ## if we are "stopping" seascan tape playback,
    ## then shut down and restart the port, so it can
    ## be played back again
    if (new.state == RSS$PS$STOPPED && port$id > 2) {
      shut.down(port)
      start.up(port)
    }
  },

  seascan.get.proc = function(port) {
    ## return the full processing information of the seascan server
    rv <- .Call("get_proc", as.integer(port$host.id - 1), PACKAGE="seascan")
    if (is.null(rv))
      return (NULL)

    names(rv) <- c( "Lines", "SamplesPerLine", "ScanConverted", "StartBearing", "AngleCoverage", "StartRange", "RangeCoverage",
                   "PRF", "AntennaSpeed", "NorthAligned", "RangePerSample", "SelectedModeNdx", "ModeNdx", "SelectedPlenNdx",
                   "PlenNdx", "PulseLength", "CFARed", "PulseFiltered", "MotionCompensated", "ScansAveraged", "FTCNdx", "State",
                   "XpolImage", "AutoGainControl", "Gain", "Offset")
    
    return(rv)
  },

  seascan.set.modeline = function(port, modeline) {
    ## set the modeline 
    ## modeline is an integer index into the mode.table
    ## used in creating the current SeaScan0.bin file
    if (0 == .Call("set_plen", port$host.id - 1, modeline, PACKAGE="seascan")) {
      current.modeline <<- modeline
    } else {
      current.modeline <<- seascan.get.proc(port)["ModeNdx"]
    }
    update.agc(port)
  },

  seascan.get.freqs = function() {
    ## get the available sampling frequencies, in MHz
    .Call("get_sampling_freqs", PACKAGE="seascan")
  },

  seascan.get.prfs = function(filename) {
    ## get the nominal PRFs from a SeaScan .ini file
    prfs <- as.integer(readLines(filename) %~-% "^NominalHz[ \t]*=[ \t]*")
    ## weird, the guard value is 16834, not 16384
    return(sort(prfs[prfs != 16834 & prfs > 1], decreasing=TRUE))
  },
  
  seascan.get.mode.table = function(filename=binary.config.file)
  {
    ## return the table used to create the SeaScan .bin file
    ## We assume that the Seascan .ini file has the same name
    ## but ending in ".ini" instead of ".bin"
    
    x <- seascan.dump.BIN(filename)
    prf <- seascan.get.prfs(gsub("\\.bin$", ".ini", filename, perl=TRUE))
    
    ## FIXME: assumes the number of AcquireModes and PulseLengths are equal!
    ## (as is the case with SeaScan.bin files produced by seascan.create.binfile)
    nmode <- x["AcquireModes"]
    nprf <- x["PRFs"]
    nplen <- x["PulseLengths"]
    sr.MHz <- x[paste("SamplingRate", 1:nmode, sep="_")]
    spp <- x[paste("SamplesPerPulse", 1:nmode, sep="_")]
    plens <- x[paste("nsPulseLength", 1:nplen, sep="_")]
    prfis <- x[paste("minPRFIndex", 1:nplen, sep="_")]
    res.m <- 3.0e8 / (sr.MHz * 1.0e6) / 2
    range.m <- spp * res.m
    rv <- data.frame(prf.Hz=prf[prfis], plen.ns=plens, res.m=res.m, range.m=range.m)
    rownames(rv) <- 1:(dim(rv)[1])
    return(rv)
  },

  seascan.get.radar.table = function(file=radar.desc.file) {
    eval(parse(file), e <- new.env(emptyenv()))
    tab <- read.table(textConnection(e$mode.table.string), header=TRUE)
    return(tab[order(tab$PRF.Hz, decreasing=TRUE),])
  },
  
  seascan.create.binfile = function(file=binary.config.file, new.binary.file, mode.table.file=radar.desc.file ) {
    ## create a SeaScan0.bin file from an existing one, with a new set
    ## of modes to allow selection of sampling frequency, number of samples per pulse,
    ## and telling seascan which pulse length (if there is more than one for the
    ## current PRF) the radar is using.

    ## filename: full path to the SeaScan0.ini file.  The existing file will be
    ##           renamed to paste(filename, ".saved", sep=""), and the
    ##           new .bin contents will be written to filename.
    ##
    ## mode.table: a data.frame (or list) with columns (or elements) in
    ##             this order:
    ##
    ##  1  PRF.Hz:     nominal pulse repetition frequency in Hz
    ##  2  PLEN.ns:    pulse length in nanoseconds
    ##  3  NPULSE:     number of pulses digitized per scan
    ##  4  RES.m:      resolution (i.e. range cell size) in metres
    ##  5  NCELLS:     number of range cells per pulse
    ##
    ##         This data frame holds all desired and allowed combinations of 
    ##         these variables.
    ##
    ##         Radar hardware will limit the combinations of pulse length and prf allowed here:
    ##            - there are typically only a few preset pulselengths
    ##            - the longer the pulse, the lower the maximum allowed prf, 
    ##              to prevent the magnetron duty cycle from exceeding safety limits, presumably
    ##            - only a small number of nominal prfs are typically available
    ##
    ##         There can be multiple rows in the table with the same values of pulse length
    ##         and prf.  Seascan empirically estimates the prf.  The user can
    ##         then select among all rows having that prf.  The user should select a row
    ##         that has the current actual pulse length, in those cases where more than one pulse length
    ##         is available for a given prf, because SeaScan can't determine this.  In cases where
    ##         more than one row matches the current prf and plen, the user can choose any of these rows;
    ##         i.e. choose any listed combination of sample rate and sample count for that (prf, plen) pair.
    ##         Different PRF modes can have different NPULSE values: it doesn't make sense to
    ##         ask for more pulses per scan than are actually transmitted by the antenna.
    ##
    ##         A given SeaScan0.bin file (and hence a given session of SeaScan) only allows
    ##         one value of azimuth count for each different PRF.Hz value.  You can create
    ##         several SeaScan0.bin files and move them around manually.  SeaScan always reads
    ##         the SeaScan0.bin file in the directory from which you start it (not the directory
    ##         in which SeaScan.exe resides).
    
    if (!all(file.exists(c(file, mode.table.file))))
      return (FALSE)
    mode.table = seascan.get.radar.table(mode.table.file)
    old.bytes <- readBin(file, "raw", n=file.info(file)$size)
    ## convert range cell size  in metres to sampling rate in Mhz
    mode.table[[4]] <- round(2.99792458E8 / (mode.table[[4]] * 2) / 1.0e6, 1)
    ## extract unique prfs and sort in decreasing order
    prfs <- sort(unique(mode.table[[1]]), decreasing=TRUE)
    ## create the new SeaScan0.bin file
    new.bytes <- .Call("create_BIN_bytes", old.bytes, prfs, mode.table, binfile.ACPs.field)
    ## file.rename(filename, paste(filename, ".saved", sep=""))
    writeBin(new.bytes, new.binary.file)
    return(TRUE)
  },

  seascan.gui.create.binfile = function() {
    rss.gui(FILE_DIALOG,
           mode = "open.one",
           title = "Choose a radar description file from which to make newSeaScan0.bin",
           types = list(".radar.R" = "radR radar description file", ".*" = "All files"),
           init.file = radar.desc.file,
           on.done = function(f) {
             if (nchar(f) != 0) {
               new.file <- file.path(dirname(binary.config.file), "newSeaScan0.bin")
               seascan.create.binfile(binary.config.file, new.file, f)
               i <- 1
               repeat {
                 tmp.file <- file.path(dirname(binary.config.file), sprintf("SeaScan0_%d.bin", i))
                 if (! file.exists(tmp.file))
                   break
                 i <- 1 + i
               }
               choice <- rss.gui(POPUP_DIALOG, "Created " %:% basename(new.file),
                                 paste("The file", basename(new.file),
                                       "has been created.\n\nTo make this active, radR must rename it to SeaScan0.bin\n(the current SeaScan0.bin will be renamed to",
                                       basename(tmp.file), ") and reload the seascan plugin.\n\nShould radR do these things now?"),
                                 buttons=c(" No - I'll do them manually ", " Yes, rename files and reload the seascan plugin "))
               if (choice > 1) {
                 file.rename(binary.config.file, tmp.file)
                 file.rename(new.file, binary.config.file)
                 id <- rss.gui(POPUP_MESSAGEBOX, "Reloading Seascan", "The SeaScan0.bin files have been renamed.\nThe Seascan plugin is reloading, which may take a minute...")
                 rss.unload.plugin("seascan", save.config=FALSE)
                 rss.load.plugin("seascan")
                 rss.gui(DELETE_MESSAGEBOX, id)
               }
             }
           })
  },

  seascan.choose.modeline = function() {
    ## popup a dialog box for choosing a modeline compatible
    ## with the current PRF

    if (is.null(RSS$source) || !inherits(RSS$source, "seascan")) {
      rss.gui(POPUP_DIALOG, title="No server selected", msg="Select a SeaScan server using player:From first.")
      return()
    }
    ## get the current mode
    proc <- seascan.get.proc(RSS$source)
    modendx <- proc["ModeNdx"]
    prf <- proc["PRF"]
    modetab <- seascan.get.mode.table()
    prfs <- sort(unique(modetab$prf.Hz))
    ## keep the modelines corresponding to the current prf (i.e. with the closest prf value)
    nom.prf <- prfs[which.min(abs(prf - prfs))]
    keep <- modetab$prf.Hz == nom.prf
    
    choice <- rss.gui(POPUP_DIALOG,
                      title="Choose a SeaScan sampling mode",
                      msg=sprintf("These modes from the SeaScan0.bin file are compatible\nwith the current PRF of %d hz\n", nom.prf),
                      buttons=apply(subset(modetab, keep, res.m:range.m), 1, function(x) sprintf("Resolution %.1f m    Range: %.2f km",
                        x[1], x[2]/1000)),
                      default=sum(keep[1:modendx]),
                      drop.down = TRUE)
    if (!is.na(choice)) {
      current.modeline <<- which(choice == cumsum(keep))[1]
      ## we don't actually change the mode here, but flag a mode change
      ## request, which will be done at a time when it is safe (e.g. not between
      ## calls to get.scan.info and get.scan.data)
      ##
      if (inherits(RSS$source, MYCLASS))
        rss.defer.call(seascan.set.modeline(RSS$source, current.modeline))
    }
  },
    
  seascan.dump.BIN = function(filename=binary.config.file) {
    ## dump the full information from a SeaScan0.bin file.
    ## This is a simple hack which uses the fact that all values are stored
    ## as 4 byte ints.
    ## FIXME: the interpretation of the MODE sections are not complete (see below).
    
    x <- readBin(filename, "int", n=file.info(filename)$size / 4)
    nmode <- x[5]
    nprf <- x[6]
    nplen <- x[7]

    names(x) <- rep("", length(x))
    
    ## the table for converting the PixelConfig value to a sampling rate, in MHz
    
    pc.2.mhz <- c(
                  40.0, ## 0x01
                  10.0, ## 0x02
                  NA,   ## 0x03
                  20.0, ## 0x04
                  NA,   ## 0x05
                  NA,   ## 0x06
                  7.5,  ## 0x07
                  2.5,  ## 0x08
                  5.0,  ## 0x09
                  30.0, ## 0x0a
                  60.0  ## 0x0b
                  )

    ## The RADARSITE and GPS structures are each present twice(!), interleaved.

    i <- 0
    names(x)[i + 1:13] <-
      c("SyncReg",
        "ConfigReg",
        "FTCReg",
        "VideoReg",
        "AcquireModes",
        "PRFs",
        "PulseLengths",
        "ACPs",
        "Gain",
        "Offset",
        "EnableGyro",
        "HeadingAdjust",
        "VideoDelay")
    i <- i + 13

    names(x)[i + 1:13] <- 
      c("LongDeg",
        "LongMin",
        "LongSec",
        "LongHundredthsSec",
        "LongHemisphere",
        "LatDeg",
        "LatMin",
        "LatSec",
        "LatHundredthsSec",
        "LatHemisphere",
        "GPSBaud",
        "GPSPort",
        "GPSSentence")
    i <- i + 13
    names(x)[i + 1:13] <- names(x)[1:13]
    i <- i + 13 
    names(x)[i + 1:13] <- names(x)[14:26]
    i <- i + 13

    ## The PLEN structure occurs with indexes from 0 to nplen

    for (j in 0:nplen) {
      names(x)[i + 1:4] <-
        paste(
              c("nsPulseLength",
                "minPRFIndex",
                "maxPRFIndex",
                "modeIndex"),
              j,
              sep = "_")
      i <- i + 4
    }

    ## The PRF structure occurs with indexes from 0 to nprf

    for (j in c(0, nprf:1)) {
      names(x)[i + 1:8] <-
        paste(
              c("LowerLimit",
                "UpperLimit",
                "Decimation",
                "MaxPulseBytes",
                "SegmentSize",
                "QuadrantSize",
                "DefaultPLEN",
                "WindowLength"),
              j,
              sep = "_")
      i <- i + 8
    }

    ## The MODE structure occurs with indexes from 0 to nmode
    ## We translate PixelConfig to Sampling rate using the pc.2.mhz table
    ## FIXME: RangeCollapse and Decimation might be swapped.  What do they do, anyway?
    
    for (j in 0:nmode) {
      names(x)[i + 1:4] <-
        paste(
              c("SamplingRate",
                "RangeCollapse",
                "SamplesPerPulse",
                "Decimation"
                ),
              j,
              sep = "_")
      x[i+1] <- pc.2.mhz[x[i+1]]
      i <- i + 4
    }

    ## The GYRO structure occurs once

    names(x)[i + 1:3] <-
      c("GyroBaud",
        "GyroPort",
        "GyroPresent")

    return(x)
  }
  
  ) ## end of globals

## additional interface plugin variables

all.ports = NULL
scan.info.names = c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp",
        "duration", "sample.dist", "first.sample.dist",
        "bearing", "orientation", "pulse.length", "PRF")

server.handle = NULL  ## the Windows process handle for the server we started (if any)

## the library name is compiled into seascan.dll, so this belongs here, and not in seascan.conf.R:
client.lib.name = "Client12.dll"  
  
## Seascan client library error codes

SEASCAN_NO_ERROR               = 0
SEASCAN_INVALID_ARGUMENT       = 1
SEASCAN_FUNCTION_NOT_SUPPORTED = 2
SEASCAN_NETWORK_ERROR	       = 3
SEASCAN_SERVER_BUSY	       = 4
SEASCAN_FUNCTION_FAILED	       = 5
SEASCAN_QUAD_ERROR	       = 6
SEASCAN_MAX_CONNECTIONS        = 7
SEASCAN_ERROR_CONNECTED        = 8
SEASCAN_NO_CONNECTION	       = 9
SEASCAN_INVALID_ID	       = 10

