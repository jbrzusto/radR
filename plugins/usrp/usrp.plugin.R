##  svn $Id$
##
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

##  Read data from Ettus research's USRP-1 using modified gnuradio code and
##  a custom FPGA build.

MYCLASS = "usrp"

about = function() {
  return(paste(plugin.label,
               "\nThis plugin reads data from the open source Ettus Research USRP-1 board.\n",
               "The board uses a custom build of the FPGA, supplied with radR.\n",
               "\nCurrent configuration:\n",
                     sprintf("\
  Antenna:           %s\n\
  Pulses per scan:   %d\n\
  Samples per pulse: %d\n\
  Range cell size:   %.2f m\n\
  Total range:       %.2f km\n",
                             port$config$antenna, port$config$n_pulses, port$config$n_samples, range.cell.size(), total.range()),
               "\n\nFull configuration:\n",
               paste(names(RSS$source$config),lapply(RSS$source$config,paste, sep=" "), collapse="\n"),
                 sep="\n")
         )
  
}

get.ports = function() {
  list(structure(strictenv(
                           name                   = "USRP-1",
                           is.source              = TRUE,
                           is.sink                = FALSE,
                           is.live                = TRUE,
                           is.file                = FALSE,
                           is.seekable            = FALSE,
                           is.open                = FALSE,
                           can.specify.start.time = FALSE,
                           config                 = get.current.params(),
                           has.toc                = FALSE,
                           si                     = NULL,
                           cur.run                = 0,
                           cur.scan               = 0,
                           prev.scan              = -1,
                           urp                    = .Call("get_ports", PACKAGE=MYCLASS)
                           ),
                 class = c(MYCLASS, "strictenv")))
}

load = function() {
  ## set the location of the path to the hardware files
  Sys.setenv(USRP_PATH = path.to.usrp.hw.files)

  ## FIXME: under windows, manually load required .dlls
  if (.Platform$OS.type != "unix") {
    for (f in c("libgcc_s_dw2-1.dll",
                "libstdc++-6.dll",
                "libboost_thread-mgw45-mt-1_46_1.dll",
                "libusrp-3-3-0-0.dll",
                "usrp_radR_plugin.dll"))
      rss.dyn.load(f, in.dir.of=plugin.file)
  }
  
  rss.dyn.load(MYCLASS, in.dir.of=plugin.file)
  
  ## get the list of known antennas
  allowed$antenna <<- get.all.antennas()

  ## create the usrp object
  .Call("create_usrp", scan.info.names)
  
  ## get the one and only port
  port <<- get.ports()[[1]]

  ## set non-database parameters from configuration file defaults
  config(port, antenna=antenna, n_bufs=n_bufs, n_samples=n_samples, n_pulses=n_pulses, decim=decim, vid_gain=vid_gain,
         hw.filenames = hw.filenames)
}

unload = function(save.config) {
  ## cleanly close the connection to the USRP
#  try ({
    shut.down(port)
#  }, silent=TRUE)
  rss.dyn.unload(MYCLASS)
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "USRP",
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
                   list(ant.menu="Choose an antenna",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 config(port, antenna = allowed$antenna[n])
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
                                 if (!okay.to.config())
                                   return()
                                 config(port, n_pulses =  n_pulses <<- allowed$n_pulses[n])
                               }
                               ),
                          structure(port$config$n_pulses == allowed$n_pulses,
                                    names = allowed$n_pulses)
                          )
                        )
                 }
                 ),
               "Samples per pulse..." = list(
                 "dyn.menu",
                 function() {
                   list(samples.menu="Choose samples per pulse",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 if (!okay.to.config())
                                   return()
                                 config(port, n_samples = n_samples <<- allowed$n_samples[n])
                               }
                               ),
                          structure(port$config$n_samples == allowed$n_samples,
                                    names = allowed$n_samples)
                          )
                        )
                 }
                 ),
               "Range cell size..." = list(
                 "dyn.menu",
                 function() {
                   ## we know the clock rate and give a choice of ranges based on decim
                   list(range.menu=sprintf("Range cell size and total range for %d samples per pulse", port$config$n_samples),
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 ## set the default decim rate
                                 config(port, decim = decim <<- allowed$decim[n])
                               }
                               ),
                          structure(port$config$decim == allowed$decim,
                                    names = sprintf("%5.1f m    (%5.1f km)",
                                      range.cell.size(decim=allowed$decim),
                                      total.range(decim=allowed$decim)))
                          )
                        )
                 }
                 ),
               "Number of sweep buffers..." = list(
                 "dyn.menu",
                 function() {
                   list(samples.menu="Choose number of sweep buffers to maintain",
                        c(list(option="choose.one",
                               on.set = function(n) {
                                 if (!okay.to.config())
                                   return()
                                 config(port, n_bufs = n_bufs <<- allowed$n_bufs[n])
                               }
                               ),
                          structure(port$config$n_bufs == allowed$n_bufs,
                                    names = allowed$n_bufs)
                          )
                        )
                 }
                 ),
               list (gain = "gauge",
                     label = "video gain", 
                     range = c(0, 20),
                     increment = 1,
                     value = port$config$vid_gain,
                     on.set = function(x) config(port, vid_gain = x),
                     set.or.get = "vid_gain.gui"
                     ),
               list (trig_delay = "gauge",
                     label = "trigger delay",
                     range = c(0, 65535),
                     increment = 1,
                     value = port$config$trig_delay,
                     on.set = function(x) config(port, trig_delay = x),
                     set.or.get = "trig_delay.gui"
                     ),
               "---",
               "Save signal parameters to antenna database..." = save.ant.params,
               "Reload signal parameters from antenna database" = reload.ant.params
             ))
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], port$name)
  return(rv)
}

## is a USRP available?
## have.usrp = function() .Call("have_usrp", PACKAGE=MYCLASS)

## all known antenna names
get.all.antennas = function(ext=antenna.file.extension.regex) {
  all.antennas <<- gsub(ext, "", dir(path=antenna.db.path, pattern=ext), perl=TRUE)
  return(all.antennas)
}

## get filename for antenna
get.antenna.filename = function(ant=antenna, ext=antenna.file.extension) {
  return (file.path(antenna.db.path, paste(ant, ext, sep="")))
}

## get the config for a particular antenna, as a list
get.antenna.params = function(ant=antenna, ext=antenna.file.extension) {
  e <- new.env()
  sys.source(get.antenna.filename(ant, ext), e)
  return (as.list(e))
}

load.config.from.db = function() {
  ## get antenna configuration from file for the current antenna, transmit it to the
  ## USRP, and update the "Controls" GUI entries.
  
  p <- get.antenna.params()
  config(port, p)
  vid_gain.gui(port$config$vid_gain)
  trig_delay.gui(port$config$trig_delay)
}

get.current.params = function() {
  structure(.Call("get_params", port$urp, PACKAGE=MYCLASS), names = usrp.param.names)
}

save.ant.params = function() {
  ## store antenna configuration to the antenna file
  ## first, load full set of parameters, then 
  f <- get.antenna.filename()
  p <- get.antenna.params()
  p[usrp.param.names] <- port$config[usrp.param.names]
  p <- as.strictenv(p)
  rss.rewrite.config(f, p, backup=paste("_old_", f, sep=""), outfile=f, allow.new=FALSE)
  rss.gui("POPUP_DIALOG", "Parameters saved to DB", "I have saved current signal parameters to the file " %:% get.antenna.filename())
}

## reload parameters from antenna database
reload.ant.params = function() {
  load.config.from.db()
  rss.gui("POPUP_DIALOG", "Parameters reloaded from DB", "I have reloaded signal parameters from the file " %:% get.antenna.filename())
}

## radial size of range cell, in metres
range.cell.size = function(decim=port$config$decim) {
  rss.speed.of.light / (sample.rate / (decim + 1)) / 2
}

## total range of scan, in kilometres
total.range = function(decim=port$config$decim, n_samples=port$config$n_samples) {
  n_samples * range.cell.size(decim) / 1000
}

## for Windows, calling end_getter from a Tcl callback causes
## a cascade of errors, so we don't allow configuration changes
## that would cause this.  This function checks whether that's the case.

okay.to.config = function() {
  if (.Platform$OS.type == "unix" ||
          ! inherits(RSS$source, "usrp") ||
          RSS$play.state < RSS$PS$PLAYING)
    return (TRUE)
  rss.gui("POPUP_MESSAGEBOX", "Can't reconfigure", "Please pause or stop play before changing this USRP digitization parameter", time.to.live=15)
  return (FALSE)
}

## methods for the usrp port:

globals = list (

  as.character.usrp = function(x, ...) {
    sprintf("radR interface port: %s: %s",
            MYCLASS,
            x$name
            )
  },

  print.usrp = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.usrp = function(port, ...) {
    ## ... can be a set of tagged parameters, or a single list
    ## e.g. config.usrp(port, vid_gain = 10, trig_delay=400) or config.usrp(port, list(vid_gain=10, trig_delay=400))
    ## This changes the current values of those parameters specified, and retransmits them
    ## to the USRP, but does not save the new values in the database.
    
    ## check for parameter validity
    opts <- rss.validate.parms(valid.names = all.param.names,
                             valid.vals = allowed,
                             .ERRLAB = "config.usrp: ",
                             ...)

    ## set param copies at R level
    port$config[names(opts)] <- opts

    ## send usrp-related parameters to usrp layer
    usrp.nos <- match(names(opts), usrp.param.names)
    usrp.opts <- opts[!is.na(usrp.nos)]
    usrp.nos <- usrp.nos[!is.na(usrp.nos)]

    .Call("set_params", port$urp, as.integer(usrp.nos), as.double(usrp.opts))

    ## further per-parameter treatment - NONE SO FAR

    ##     for (optn in names(opts)) {
    ##         opt <- opts[[optn]]
    ##         switch(optn,
    ##                NULL
    ##                )
    ##       }
    
    return(port$config)
  },

  end.of.data.usrp = function(port, ...) {
    ## return TRUE if there is no data left to be read from the device
    ## (e.g. if digitizing has stopped)
    .Call("end_of_data", port$urp, PACKAGE=MYCLASS)
  },

  get.scan.info.usrp = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next scan
    ## (and actually gets the next scan)
    dims <- c(port$config$n_samples, port$config$n_pulses)
    dim(scan.mat) <- dims
    dim(RSS$class.mat) <- dims
    dim(RSS$score.mat) <- dims
    .Call("get_scan_info",
          port$urp,
          scan.mat,
          trv,
          trv.index,
          PACKAGE=MYCLASS)
  },

  get.scan.data.usrp = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does all the work, so we just pass the extmat in
    ## case video negation needs to be performed.  FIXME: is this necessary
    ## now that we're using GetScanLineExt (which cooks data)?

    if (is.null(.Call("get_scan_data", port$urp, extmat, PACKAGE=MYCLASS)))
      return (NULL)
    
    return (extmat)
  },

  start.up.usrp = function(port, ...) {
    ## connect to the USRP

    ## set some sensible values for digitizing parameters

    config(port,
           counting   = 0, ## not interleaving debug counter
           all_pulses = 0, ## getting gated data, rather than all pulses,
           own_thread = 1, ## yes, getter runs in background
           bbprx_mode = 0  ## normal mode: a chunk of video data is digitized for each trigger pulse
           )

    repeat {
      if (port$is.open)
        break
      id <- rss.gui(POPUP_MESSAGEBOX, "Opening USRP device", sprintf("Please wait while I open the %s.\nThis will take up to 5 seconds...", port$name), time.to.live = 5)

      ## load configuration items from the antenna database

      load.config.from.db()

      ## start the usrp digitizing
      
      rv <- .Call("start_up", port$urp, port$config$hw.filenames, PACKAGE=MYCLASS)

      rss.gui("DELETE_MESSAGEBOX", id)

      if (rv != 0) {
        if (identical(1, rss.gui(POPUP_DIALOG, "No USRP", "I can't find (or start) the USRP.\nPossible causes include:\n (1) the USRP is not connected to the computer or not powered up\n (2) a bad connection between radar and the USRP\n(3) insufficient user priviliges for accessing a USB device\n", buttons=c("   Okay   ", "  Retry  "))))
          break
        else {
          shut.down(port, force=TRUE) ## make sure we try redetect the device
          next
        }
      }
        
     id <- rss.gui("POPUP_MESSAGEBOX", "Checking for trigger", "Please wait while I verify that the USRP is receiving radar trigger pulses")
     okay <- .Call("have_trigger", port$urp, PACKAGE=MYCLASS)
     rss.gui("DELETE_MESSAGEBOX", id)
      if (okay) {
        port$is.open <- TRUE
        break
      }
      if (identical(1, rss.gui(POPUP_DIALOG, "USRP Problem", "The USRP is not detecting radar trigger pulses.\nPossible causes include:\n (1) the radar is not transmitting\n (2) incorrect trigger digitizing parameters for the antenna " %:% port$config$antenna, buttons=c("   Okay   ", "  Retry  "))))
        break
    } 
    return(if (port$is.open) TRUE else NULL)
  },

  shut.down.usrp = function(port, force=FALSE, ...) {
    ## shut down the usrp
    ## if it has one

    if (port$is.open || force) {
      .Call("shut_down", port$urp, PACKAGE=MYCLASS)
      port$is.open <- FALSE
    }
    return(TRUE)
  },

  new.play.state.usrp = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }

  )  ## end of globals

hooks = list(
  ) ## end of hooks

## additional plugin variables

## the port for the (one and only) USRP device, set at load time

port = NULL

## the names of digitizing parameters known to the usrp layer
## each antenna entry in the antenna database has values for each of these
## parameters, as well as for other ones such as "desc"
## The order of this list must match that of the PARAM_XXX constants in class usrp_radR_plugin
  
usrp.param.names = c (
  "decim",
  "vid_gain",
  "vid_negate",
  "trig_gain",
  "trig_thresh_excite",
  "trig_thresh_relax",
  "trig_latency",
  "trig_delay",
  "arp_gain",
  "arp_thresh_excite",
  "arp_thresh_relax",
  "arp_latency",
  "acp_gain",
  "acp_thresh_excite",
  "acp_thresh_relax",
  "acp_latency",
  "n_samples",
  "n_pulses",
  "counting",
  "all_pulses",
  "own_thread",
  "bbprx_mode",
  "n_bufs",
  "acps_per_sweep",
  "use_acp_for_sweeps"
  )

## additional configuration parameters that are not seen by the usrp
plugin.param.names = c(
  "antenna",
  "desc",
  "rpm",
  "modes",
  "prf",
  "plen",
  "hw.filenames"
  )

## all in one convenient spot
all.param.names = c(usrp.param.names, plugin.param.names)

## ordered list of scan info items returned by this plugin
scan.info.names = c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp", 
  "duration", "sample.dist", "first.sample.dist", "bearing", "orientation", 
  "pulse.length", "PRF") 

## Allowed values for those config items with restrictions.
## Those items which are a vector give all allowed items.
## Those items which are an expression return TRUE or FALSE,
## according to whether the value of "x" is allowed.

allowed = list ( 
  ## antenna names (filled in at load time)
  antenna = NULL,

  ## pulses per scan
  n_pulses = 256 * c(1:16),

  ## decimation rate FIXME: need to find a more natural way to allow user to select an arbitrary decimation rate / cell size
  decim = 0:32,
  
  ## samples per pulse
  n_samples = 256 * c(1:16),
  
  ## gain values:
  vid_gain = expression (x >= 0 && x <= 20),

  ## number of buffers
  n_bufs = 2:16,

  ## hardware filenames (fpga, firmware)
  hw.filenames = NULL
  
  )

## the undecimated sample rate, in MHz

sample.rate = 64e6

## the standard extension for an antenna file  
antenna.file.extension = ".antenna.R"

## the regular expression matching an antenna file
antenna.file.extension.regex = "\\.antenna\\.[rR]$"
