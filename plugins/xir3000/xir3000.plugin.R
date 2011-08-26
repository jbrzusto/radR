##  svn $Id: xir3000.plugin.R 655 2010-10-05 19:02:42Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2010 John Brzustowski
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

##  Read data from Russell Technologies Inc's XIR3000 USB video processor.

MYCLASS = "xir3000"

about = function() {
  return(paste(plugin.label,
               "\nThe SDK subfolder of this plugin consists of files which are (C) Russell Technologies Inc.",
               "redistributed with the kind permission of Harvey Russell of RTI.",
               "\nCurrent configuration:\n",
                     sprintf("\
  Antenna:         %s\n\
  Pulses per scan: %d\n\
  Sample rate:     %d MHz\n\
  Range cell size: %.1f m\n\
  Total range:     %.1f km\n\
  Trigger offset:  %d samples\n\
  Video gain:      %d\n",
                                antenna, pulses, sample.rate, range.cell.size(), total.range(), port$config$trig.offset, port$config$vid.gain),
                 sep="\n")
         )
  
}

get.ports = function() {
  list(structure(strictenv(
                           name                   = "XIR3000 USB Video Processor",
                           is.source              = TRUE,
                           is.sink                = FALSE,
                           is.live                = TRUE,
                           is.file                = FALSE,
                           is.seekable            = FALSE,
                           is.open                = FALSE,
                           can.specify.start.time = FALSE,
                           config                 = structure(.Call("get_curr_params"), names = RTI_ANT_PARAM_NAMES),
                           has.toc                = FALSE,
                           si                     = NULL,
                           cur.run                = 0,
                           cur.scan               = 0,
                           prev.scan              = -1
                           ),
                 class = c(MYCLASS, "strictenv")))
}

load = function() {
  ## load the client library, which our library depends on
  new.path <- rss.get.foreign.path(client.lib, "For the XIR3000 plugin, where is the RTI Library CANRib.dll?", keep.basename=TRUE)
  
  if (length(new.path) == 0)
    return(NA)
  
  client.lib <<- new.path

  rss.dyn.load(client.lib)
  rss.dyn.load("libs/pthreadGC2.dll")
  rss.dyn.load(MYCLASS, in.dir.of=plugin.file)
  
  ## get the list of known antennas
  allowed$antenna <<- get.all.antennas()
  
  ## get the one and only port
  port <<- get.ports()[[1]]

  if (! have.USB()) {
    rss.gui(POPUP_MESSAGEBOX, "Device not found", "I cannot find a XIR3000 USB video processsor.\n  Please ensure the device is powered up and plugged into a USB port.")
    return(NA)
  }

  ## set non-database parameters from configuration file defaults
  config(port, antenna=antenna, sample.rate=sample.rate, pulses=pulses)
}

unload = function(save.config) {
  ## cleanly close the connection to the XIR3000
  try ({
    shut.down(port)
  }, silent=TRUE)
  rss.dyn.unload(MYCLASS)
  rss.dyn.unload(client.lib)
  ## note: we don't unload the pthreads library as another plugin
  ## might need it.
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "XIR3000",
               menu = list (
                 options = "no-tearoff",
                 list (
                       "choose.one",
                       on.set = function(i) {
                         rss.set.port(port)
                       },
                       group = "group.play.source.menu"
                       )
                 )
               ),
             plugin = list(
               "Antenna selection..." = list(
                 "dyn.menu",
                 function() {
                   ## the list of antennas
                   list(ant.menu="Choose an antenna (restarts device if already selected as source)",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 reconfig(antenna = allowed$antenna[n])
                               }
                               ),
                          structure(port$config$antenna == allowed$antenna,
                                    names = allowed$antenna)
                          )
                        )
                 }
                 ),
               "Azimuth resolution..." = list(
                 "dyn.menu",
                 function() {
                   list(pulses.menu="Choose azimuth resolution in pulses per scan",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 reconfig(pulses = allowed$pulses[n])
                               }
                               ),
                          structure(port$config$pulses == allowed$pulses,
                                    names = allowed$pulses)
                          )
                        )
                 }
                 ),
               "Sampling rate..." = list(
                 "dyn.menu",
                 function() {
                   ## the list of sampling rates
                   list(rate.menu="Choose sampling rate in MHz (restarts device)",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 reconfig(sample.rate = allowed$sample.rate[n])
                               }
                               ),
                          structure(port$config$sample.rate == allowed$sample.rate,
                                    names = allowed$sample.rate)
                          )
                        )
                 }
                 ),
               "Range cell size..." = list(
                 "dyn.menu",
                 function() {
                   ## we know the clock rate and give a choice of ranges based on vid.divs
                   list(range.menu="Range cell size (and maximum range) - choices here reflect sampling rate",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 ## set the default USB vid.div and that of the source
                                 reconfig(vid.div = allowed$vid.div[n])
                               }
                               ),
                          structure(port$config$vid.div == allowed$vid.div,
                                    names = sprintf("%5.1f m    (%5.1f km)",
                                      range.cell.size(vid.div=allowed$vid.div),
                                      total.range(vid.div=allowed$vid.div)))
                          )
                        )
                 }
                 ),
               list (gain = "gauge",
                     label = "video gain", 
                     range = c(0, 255),
                     increment = 5,
                     value = port$config$vid.gain,
                     on.set = function(x) reconfig(vid.gain = x),
                     set.or.get = "vid.gain.gui"
                     ),
               list (trig.offset = "gauge",
                     label = "trigger offset",
                     range = c(0, 65535),
                     increment = 1,
                     value = port$config$trig.offset,
                     on.set = function(x) reconfig(trig.offset = x),
                     set.or.get = "trigger.offset.gui"
                     ),
               "---",
               "Save values of video gain, trigger offset, and range cell size to RTI database..." = save.db.params,
               "Reload values of video gain, trigger offset, and range cell size from RTI database" = reload.db.params
             ))
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], port$name)
  return(rv)
}

reconfig = function(p = port, ...) {
  ## configure the current source according to parameters supplied in (...)
  ## If necessary, restart the current source.
  ## Do nothing if the current source is not an xir3000.
  
  if (! inherits(p, MYCLASS))
    return (NULL)

  opts <- rss.validate.parms(valid.names = names(allowed),
                             valid.vals = allowed,
                             .ERRLAB = "xir3000 reconfig: ",
                             ...)
  
  do.restart <- FALSE
  for (optn in names(opts)) {
    opt <- opts[[optn]]
    if (!identical(opt, p$config[[optn]])) {
      config(p, structure(list(opt), names=optn))
      if (requires.restart[[optn]])
        ## the option is relevant to the source, has a different value than the current setting, and requires a restart
        do.restart <- TRUE
    }
  }  
  if (p$is.open && do.restart) {
    shut.down(p)

    ## the following appears necessary for changing sampling rate
    rss.dyn.unload(client.lib)
    rss.dyn.load(client.lib)
    
    start.up(p, restart=TRUE)
  }
}

## is a USB video processor available?
have.USB = function() .Call("have_xir3000", PACKAGE=MYCLASS)

## all known antenna names
get.all.antennas = function() .Call("get_all_antennas", PACKAGE=MYCLASS)

## send a direct USB command:

usb.cmd = function(code, val) {
  if (code %in% names(USB_CMD))
    .Call("usb_send_command", c(RTI_USB_VEND_CODE, USB_CMD[[code]], val))
  else
    warning("xir3000: Attempt to send invalid USB command ('", code, "' to XIR3000")
}

load.config.from.db = function(is.restart=FALSE) {
  ## get antenna configuration from the CANStar.ant database, transmit it to the
  ## XIR3000, update R level copies, and update the "Controls" GUI entries.
  ## If is.restart is FALSE, we load all parameters from the database.
  ## Otherwise, we load parameters from the database but override their values
  ## with those in the current port configuration.
  
  .Call("load_db_params")
  if (is.restart) {
    ## write current port configuration to device, overriding database values
    .Call("set_curr_params", port$config[RTI_ANT_PARAM_NAMES])
  } else {
    ## set port configuration from values loaded from database
    port$config[RTI_ANT_PARAM_NAMES] <- .Call("get_curr_params")
  }
  .Call("transmit_curr_params")
  vid.gain.gui(port$config$vid.gain)
  trigger.offset.gui(port$config$trigger.offset)
}


## get/set all antenna parms

ant = function(...) {
  np <- c(list(...), recursive=TRUE)
  p <- structure(.Call("get_all_antenna_parms"), names = RTI_ALL_ANT_PARAM_NAMES)
  if (! length(np))
    return(p)
  for (i in seq(along=np))
    if (names(np)[i] %in% names(p))
      p[names(np)[i]] <- as.integer(np[i])
    else
      stop("xir3000: unknown antenna configuration paramter " %:% names(np)[i])
  .Call("set_all_antenna_parms", p)
  .Call("transmit_curr_params")
}

save.config.to.db = function() {
  ## store antenna configuration to the CANStar.ant database
  .Call("save_db_params")
}

## radial size of range cell, in metres
range.cell.size = function(sample.rate = port$config$sample.rate, vid.div=port$config$vid.div) {
  rss.speed.of.light / (sample.rate * 1.0e6 / (1 + vid.div)) / 2
}

## total range of scan, in kilometres
total.range = function(sample.rate = port$config$sample.rate, vid.div=port$config$vid.div) {
  samples.per.pulse * range.cell.size(sample.rate, vid.div) / 1000
}

## save current parameters to RTI database 
save.db.params = function () {
  if (identical(2, rss.gui("POPUP_DIALOG", "Save current parameters?", sprintf("This means from now on, radR and RTI software will use the current values for:\n - video gain\n - trigger offset\n - range cell size (video division)\nwhen you select the antenna %s.\n\nAre you sure you want to do this?", port$config$antenna), buttons=c("   No   ", "   Yes   ")))) {
    .Call("save_db_params")
    rss.gui("POPUP_MESSAGEBOX", "Parameters saved", sprintf("I have saved the current values of:\n - video gain\n - trigger offset\n - range cell size (video division)\n\n to the RTI database entry for antenna %s", port$config$antenna), time.to.live = 30)
  }
}

## reload parameters from RTI database
reload.db.params = function() {
  load.config.from.db()
  rss.gui("POPUP_MESSAGEBOX", "Parameters reloaded from DB", "I have reloaded the values of:\n - video gain\n - trigger offset\n - range cell size (video division)\n\n from the RTI database entry for the antenna " %:% port$config$antenna, time.to.live = 30)
}

## methods for the xir3000 port:

globals = list (

  as.character.xir3000 = function(x, ...) {
    sprintf("radR interface port: %s: %s",
            MYCLASS,
            x$name
            )
  },

  print.xir3000 = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.xir3000 = function(port, ...) {
    ## ... can be a set of tagged parameters, or a single list
    ## e.g. config.xir3000(port, vid.xgain = 10, trig.offset=400) or config.xir3000(port, list(vid.xgain=10, trig.offset=400))
    ## This changes the current values of those parameters specified, and retransmits them
    ## to the XIR3000, but does not save the new values in the RTI database.
    
    ## check for parameter validity
    opts <- rss.validate.parms(valid.names = names(allowed),
                             valid.vals = allowed,
                             .ERRLAB = "config.xir3000: ",
                             ...)

    ## set param copies at R level
    port$config[names(opts)] <- opts

    ## set param copies at C level
    .Call("set_curr_params", port$config[RTI_ANT_PARAM_NAMES])

    ## further per-parameter treatment

    for (optn in names(opts)) {
        opt <- opts[[optn]]
        switch(optn,
               ## The following parameters must be retransmitted to the XIR3000
               ## We could use the CANRib library to do this, but following RTI's example
               ## RadarSample code, we do it by direct USB calls.
               vid.gain = {
                 usb.cmd("vid.xgain", opt %/% 16)
                 usb.cmd("vid.gain", opt %% 16)
               },
               trig.offset = {
                 usb.cmd("trig.offset.hi", opt %/% 256) ## allows for greater values than CSAPI_SetCableLength()
                 usb.cmd("trig.offset.lo", opt %% 256)
               },
               vid.div = {
                 usb.cmd("vid.div", opt)
               },

               ## The following parameters are taken from the plugin
               ## configuration file at startup, and need to be set in the
               ## plugin environment so that their current values can be resaved
               ## in that file if/when the user chooses to do so.  They are sent
               ## to C level upon device restart.
               ## FIXME: set_pulses should not require a device restart, but the
               ## way the getter thread is coded requires it.
               
               antenna = {
                 antenna <<- opt
               },
               sample.rate = {
                 sample.rate <<- opt
               },
               pulses = {
                 pulses <<- opt
                 .Call("set_pulses", opt)  ## done here instead of in start.up in anticipation of FIXME above

                 ## set empirically determined batch parameters, based on pulses per sweep
                 if (pulses >= 512)
                   .Call("set_batch_params", c(64L, 30L))
                 else
                   .Call("set_batch_params", c(32L, 60L))
               }
               )
      }
    return(port$config)
  },

  end.of.data.xir3000 = function(port, ...) {
    ## return TRUE if there is no data left to be read from the device
    ## (e.g. if digitizing has stopped)
    .Call("end_of_data", PACKAGE=MYCLASS)
  },

  get.scan.info.xir3000 = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next scan
    ## (and actually gets the next scan)
    .Call("get_scan_info",
          scan.info.names,
          scan.mat,
          trv,
          trv.index,
          PACKAGE=MYCLASS)
  },

  get.scan.data.xir3000 = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does all the work, so we just pass the extmat in
    ## case video negation needs to be performed.  FIXME: is this necessary
    ## now that we're using GetScanLineExt (which cooks data)?

    if (is.null(.Call("get_scan_data", extmat, PACKAGE=MYCLASS)))
      return (NULL)
    
    dim <- dim(extmat)
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim
    return (extmat)
  },

  start.up.xir3000 = function(port, restart=FALSE,...) {
    ## connect to the XIR3000

    repeat {
      if (port$is.open)
        break
      id <- rss.gui(POPUP_MESSAGEBOX, if (restart) "Restarting device" else "Opening device", sprintf("Please wait while I open the %s.\nThis will take up to 15 seconds...", port$name), time.to.live = 15)

      .Call("start_up", port$config$antenna, timeout * 1000, port$config$sample.rate)

      ## load configuration items from the RTI antenna database
      ## values from the database will be overridden by current values
      ## if this is a restart.
      load.config.from.db(restart)

      rss.gui("DELETE_MESSAGEBOX", id)

      if (!restart) {
        ## get pulses per sweep from library
        pulses.per.sweep <<- .Call("get_pulses_per_sweep", PACKAGE=MYCLASS)

        ## set allowable pulses per sweep
        allowed$pulses <<- round(pulses.per.sweep * (16:1) / 16)
      }

     id <- rss.gui("POPUP_MESSAGEBOX", "Checking for trigger", "Please wait while I verify that the XIR3000 is receiving radar trigger pulses")
     okay <- .Call("have_trigger", PACKAGE=MYCLASS)
     rss.gui("DELETE_MESSAGEBOX", id)
      if (okay) {
        port$is.open <- TRUE
        break
      }
      if (identical(1, rss.gui(POPUP_DIALOG, "XIR3000 Problem", "The XIR3000 is not detecting radar trigger pulses.\nPossible causes include:\n (1) the radar is not transmitting\n (2) a bad connection between radar and the XIR3000\n (3) incorrect trigger digitizing parameters for the antenna " %:% port$config$antenna %:% "\n\nYou can correct (3) using the RTI RadarSample.exe program.", buttons=c("   Okay   ", "  Retry  "))))
        break
    } 
    return(if (port$is.open) TRUE else NULL)
  },

  shut.down.xir3000 = function(port, ...) {
    ## shut down this port's connection to a "Virtual Radar Device",
    ## if it has one

    if (port$is.open) {
      .Call("shut_down")
      port$is.open <- FALSE
    }
    return(TRUE)
  },

  new.play.state.xir3000 = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }

  )  ## end of globals

hooks = list(
  ) ## end of hooks

## additional plugin variables

## the port for the (one and only) XIR3000 device, set at load time

port = NULL

## the maximum available number of pulses per sweep (in extended mode),
## set at device start.up

pulses.per.sweep = NULL

## the vendor code used by RTI for direct USB calls

RTI_USB_VEND_CODE = 0xD2L

## codes for direct USB commands to the XIR3000.  We use these to update
## parameter values without using the CANRib library.

USB_CMD = list (
  vid.div        = 0x55L,
  vid.xgain      = 0x84L,
  vid.gain       = 0x85L,
  trig.offset.hi = 0x50L,
  trig.offset.lo = 0x51L
  )

## names of antenna database parameters we allow user to modify
RTI_ANT_PARAM_NAMES = c("trig.offset", "vid.div", "vid.gain")

## names of all antenna database parameters
RTI_ALL_ANT_PARAM_NAMES = c(
"trgh",
"trgl",
"trgg",
"trge",
"bph",
"bpl",
"bpg",
"bpe",
"shmh",
"shml",
"shmg",
"shme",
"trgoffh",
"trgoffl",
"viddiv",
"vidg",
"vidxg",
"vidthresh",
"vidref",
"tunerange",
"negvideo",
"bppersweep",
"birdaltitude"
)

## ordered list of scan info items returned by this plugin
scan.info.names = c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp", 
  "duration", "sample.dist", "first.sample.dist", "bearing", "orientation", 
  "pulse.length", "PRF") 

## which config items require a restart of the device?

requires.restart = list (
  antenna      = TRUE,
  pulses       = FALSE,
  sample.rate  = TRUE,
  trig.offset  = FALSE,
  vid.div      = FALSE,
  vid.gain     = FALSE
  )

## Allowed values for those config items with restrictions.
## Those items which are a vector give all allowed items.
## Those items which are an expression return TRUE or FALSE,
## according to whether the value of "x" is allowed.

allowed = list ( 
  ## antenna names (filled in at load time)
  antenna = NULL,

  ## pulses per scan: (reset at startup time)
  pulses = round(8192 / (16 : 1)),
  
  ## sample rate values (in MHz):
  sample.rate = c(54, 100),

  ## trig.offset values:
  trig.offset = expression( x >= 0 && x <= 65535),
  
  ## vid.div values:
  vid.div = 0:31,

  ## gain values:
  vid.gain = expression( x >= 0 && x <= 255)
  )

## the true number of samples per pulse returned by the library

samples.per.pulse = 506
