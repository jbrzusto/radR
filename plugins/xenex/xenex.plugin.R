##  svn $Id: xenex.plugin.R 624 2010-07-14 02:39:24Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2008 John Brzustowski
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


##               XENEX    PLUGIN
##                                                         
##  Read data from Russell Technologies' IntegRadar server 
##  or XIR3000 USB radar digitizing hardware.              



MYCLASS = "xenex"

about = function() {
  return(plugin.label)
}

get.ports = function() {

  make.port <- function(name, type, devno) {
    structure(strictenv(
                        name                   = name,
                        type                   = type,                       # what kind of port "USB", "RIB", or "TCPIP"
                        devno                  = devno,                     # internal device number: 0 = USB, 1 = RIB, 1+n = nth host
                        is.source              = TRUE,
                        is.sink                = FALSE,
                        is.live                = TRUE,
                        is.file                = FALSE,
                        is.seekable            = FALSE,
                        is.open                = FALSE,
                        can.specify.start.time = FALSE,
                        config                 = defaults[[type]],
                        has.toc                = FALSE,
                        si                     = NULL,
                        cur.run                = 0,
                        cur.scan               = 0,
                        prev.scan              = -1
                        ),
              class = c(MYCLASS, "strictenv"))
  }


  ## create port objects

  rv <- list()

  if (have.port.USB <<- have.USB())
    rv <- c(rv, list(make.port("XIR3000 USB Reader",  "USB", 0)))

  if (have.port.RIB <<- have.RIB())
    rv <- c(rv, list(make.port("Radar Interface Board",  "RIB", 1)))

  for (i in seq(along=known.hosts))
    rv <- c(rv, list(make.port("IntegRadar Server on " %:% known.hosts[i],  "TCPIP", i + 1)))

  return(all.ports<<-rv)
}

load = function() {
  ## load the client library, which our library depends on
  new.path <- rss.get.foreign.path(client.lib, "Where is the Xenex Client Library?", keep.basename=TRUE)

  if (length(new.path) == 0)
    return(NA)
  
  client.lib <<- new.path
  rss.dyn.load(client.lib)
  rss.dyn.load("libs/pthreadGC2.dll")

  rss.dyn.load(MYCLASS, in.dir.of=plugin.file)
  ## discover any IntegRadar hosts
  if (have.winsock())
    known.hosts <<- get.hosts()
  ## get the list of known antennas
  allowed$antenna <<- get.all.antennas()
  
  ## get the list of ports
  all.ports <<- get.ports()
}

unload = function(save.config) {
  ## cleanly close connections to "Virtual Radar" devices
  try ({
    for (p in all.ports)
      shut.down(p)
  }, silent=TRUE)
  rss.dyn.unload(MYCLASS)
  rss.dyn.unload(client.lib)
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "Xenex (Russel)",
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
             plugin = c(list(),
               if (have.port.RIB) {
                 list(
                      "DSP version to use for RIB:",
                      c(list(option="choose.one",
                             on.set = function(n) {
                               defaults$RIB$dsp <<- n
                               reconfig(dsp = n)
                             }
                             ),
                        structure(defaults$RIB$dsp == seq(along=allowed$dsp), names=allowed$dsp)
                        )
                      )
               },
               list(
                    c(list(option="choose.any",
                           on.set = function(n, v) {
                             defaults$USB$neg.video <<- v
                             reconfig(neg.video = v)
                           }
                           ),
                      structure(defaults$USB$neg.video, names = "Negate Video")
                      )
                    ),
               "---",
               if (have.port.RIB || have.port.USB) {
                 list(
                      list(option="choose.any",
                           on.set = function(n, v) {
                             ## we cheat and use the same button for master mode in both cases
                             defaults$USB$master <<- v
                             defaults$RIB$master <<- v
                             reconfig(master = v)
                           },
                           "Master mode" = defaults$RIB$master
                           )
                      )
               },
               "---",
               list(
                    "Choose resolution & range..." = list(
                      "dyn.menu",
                      function() {
                        if (!inherits(RSS$source, MYCLASS))
                          list(range.menu="Source is not a " %:% MYCLASS %:% " port.")
                        else if (RSS$source$devno > 0) {
                          list(range.menu="Range cell size and maximum range",
                               c(list(option="choose.one",
                                      on.set = function(n) {
                                        ## we cheat and use the same button for range in all cases
                                        for (type in names(defaults))
                                          if (!is.null(defaults[[type]]$range.index))
                                            defaults[[type]]$range.index <<- n - 1
                                        reconfig(range.index = n - 1)
                                      }
                                      ),
                                 structure(RSS$source$config$range.index == allowed$range.index,
                                           names = sprintf("%5.1f m    %5.3f km",
                                             range.values[[get.prg.type(RSS$source)]] / 496,
                                             range.values[[get.prg.type(RSS$source)]] / 1000))
                                 )
                               )
                        } else {
                          ## for the USB board, we know the clock rate and give a choice of ranges based on usb.vid.divs
                          list(range.menu="Range cell size and maximum range",
                               c(list(option="choose.one",
                                      on.set = function(n) {
                                        ## set the default USB vid.div and that of the source
                                        config(RSS$source, vid.div = defaults$USB$vid.div <<- usb.vid.divs[n])
                                      }
                                      ),
                                 with(RSS$source$config,
                                      structure(vid.div == usb.vid.divs,
                                                names = sprintf("%5.1f m    %5.3f km",
                                                  rss.speed.of.light / (sample.rate * 1.0e6 / (1 + usb.vid.divs)) / 2,
                                                  RSS$scan.info$samples.per.pulse * rss.speed.of.light /
                                                  (sample.rate * 1.0e6 / (1 + usb.vid.divs)) / 2000)))
                                 )
                               )

                        }
                      }
                      )
                    )
               )
             )
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], sapply(all.ports, function(x) x$name))
  return(rv)
}

reconfig <- function(...) {
  ## Set the default parameter values for xenex ports as specified by "...", which
  ## is a list of NAME=VALUE pairs, as for config.xenex()
  ## The defaults are set for all ports for which the parameters are valid.
  ## Also, if the current source is a xenex port, then it is reconfigured with
  ## any of the specified parameters which are appropriate to its type, and if necessary,
  ## restarted.

  opts <- rss.validate.parms(valid.names = names(allowed),
                             valid.vals = allowed,
                             .ERRLAB = "xenex reconfig: ",
                             ...)


  ## set the defaults, where appropriate
  for (optn in names(opts)) {
    opt <- opts[[optn]]
    for (type in names(defaults))
      if (optn %in% names(defaults[[type]]))
        defaults[[type]][optn] <- list(opt)
  }

  p <- RSS$source
  if (inherits(p, "xenex")) {

    ## if the current source is a xenex port, reconfigure and possibly restart
    do.restart <- FALSE

    for (optn in names(opts)) {
      opt <- opts[[optn]]
      if (optn %in% names(defaults[[p$type]])) {
        if (!identical(p, config(p)[[optn]])) {
          config(p, structure(list(opt), names=optn))
          if (requires.restart[[optn]])
            ## the option is relevant to the source, has a different value than the current setting, and requires a restart
            do.restart <- TRUE
        }
      }
    }
    if (do.restart) {
      shut.down(p)
      start.up(p)
    }
  }
}

## return the list of names with hosts having IntegRadar servers
get.hosts <- function() {
  if (probe.net.hosts)
    rss.gui(POPUP_MESSAGEBOX, "Please wait...", "Probing the network for IntegRadar servers. This may take a few seconds...", time.to.live = get.hosts.timeout)
  .Call("get_hosts", static.hosts, probe.net.hosts, get.hosts.timeout * 1000, TRUE, PACKAGE=MYCLASS)
}

## return a string vector of the form c(USERNAME, PASSWORD) for logging into
## a server.  Return NULL if the user cancels.

get.host.login <- function(host) {
  info <- defaults$TCPIP$login.info[[host]]
  if (is.null(info))
    info <- as.character(c(NA, NA))

  ## prompt for missing parts
  if (is.na(info[1]))
    if (is.na(info[1] <- rss.gui(POPUP_DIALOG, "Login", "Username for IntegRadar server on " %:% host, entry=TRUE)[[2]]))
      return(NULL)

  if (is.na(info[2]))
    if (is.na(info[2] <- rss.gui(POPUP_DIALOG, "Password", "Password for " %:% info[1] %:% " on " %:% host, entry=TRUE)[[2]]))
      return(NULL)

  return (info)
}

## is a (PCI?) radar interface board available?
have.RIB <- function() .Call("have_radar_board", PACKAGE=MYCLASS)

## is a USB radar board available?
have.USB <- function() .Call("have_xir3000", PACKAGE=MYCLASS)

## can we look for IntegRadar servers?
have.winsock <- function() .Call("have_winsock", PACKAGE=MYCLASS)

## current antenna name
antenna.name <- function(x) .Call("get_antenna", x$devno, PACKAGE=MYCLASS)

## all known antenna names
get.all.antennas <- function() .Call("get_all_antennas", PACKAGE=MYCLASS)

## DSP program type for a port:

get.prg.type <- function(p) {
  x <- .Call("get_prg_type", p$devno)
  if (x == 7)
    return ("DSP7")
  if (x == 9)
    return ("DSP9")
  return (NULL)
}

## get the range cell size:

get.sample.dist <- function(p, spp) {
  if (p$type == "USB")
    with (p$config, rss.speed.of.light / (sample.rate * 1e6 / (1 + vid.div)) / 2)
  else
    with (p$config, range.values[[get.prg.type(p)]][range.index] / spp)
}

## send a direct USB command:

usb.cmd <- function(port, code, val) {
  if (code %in% names(USB_CMD))
    .Call("usb_send_command", port$devno, c(RTI_USB_VEND_CODE, USB_CMD[[code]], val))
  else
    warning("xenex: Attempt to send invalid USB command ('", code, "' to XIR3000")
}

## get/set all antenna parms

ant <- function(port, ...) {
  np <- c(list(...), recursive=TRUE)
  p <- structure(.Call("get_antenna_parms", port$devno), names = RTI_ANT_PARM_NAMES)
  if (! length(np))
    return(p)
  for (i in seq(along=np))
    if (names(np)[i] %in% names(p))
      p[names(np)[i]] <- as.integer(np[i])
    else
      stop("xenex: unknown antenna configuration paramter " %:% names(np)[i])
  .Call("set_antenna_parms", port$devno, p)
}
  

globals = list (

  as.character.xenex = function(x, ...) {
    sprintf("radR interface port: %s: %s",
            MYCLASS,
            x$name
            )
  },

  print.xenex = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.xenex = function(port, ...) {
    ## ... can be a set of tagged parameters, or a single list
    ## e.g. config.xenex(port, vid.xgain = 10, trig.offset=400) or config.xenex(port, list(vid.xgain=10, trig.offset=400))

    ## check for parameter validity

    opts <- rss.validate.parms(valid.names = names(defaults[[port$type]]),
                             valid.vals = allowed,
                             .ERRLAB = "config.xenex: ",
                             ...)

    for (optn in names(opts)) {
        opt <- opts[[optn]]

        port$config[[optn]] <- opt

        ## perform the reconfiguration on the port's actual device
        switch(optn,
               ## set the range
               range.index = .Call("set_range", port$devno, opt),

               ## set the video xgain
               vid.xgain = usb.cmd(port, "vid.xgain", opt),

               ## set the trigger offset
               trig.offset = {
                 usb.cmd(port, "trig.offset.hi", opt / 256)
                 usb.cmd(port, "trig.offset.lo", opt %% 256)
               },

               ## set the cable length
               cable.length = .Call("set_cable_length", port$devno, opt),

               ## set the video divisor (if set to n, then each sample is integrated for (n+1) digitizer clock ticks)
               vid.div = usb.cmd(port, "vid.div", opt)
               )
      }
    return(port$config)
  },

  end.of.data.xenex = function(port, ...) {
    ## return TRUE if there is no data left to be read
    ## on this port (e.g. if the end of a tape run has been hit)
    .Call("end_of_data", port$devno, PACKAGE=MYCLASS)
  },

  get.scan.info.xenex = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next scan

    .Call("get_scan_info",
          port$devno,
          scan.info.names,
          scan.mat,
          use.raw.data & port$type != "TCPIP",
          trv,
          trv.index,
          PACKAGE=MYCLASS)
  },

  get.scan.data.xenex = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does all the work, so we just pass the device number

    if (is.null(.Call("get_scan_data", as.integer(port$devno),
                      extmat,
                      ## negate video? if so, pass the subtrahend, otherwise pass 0
                      if (port$config$neg.video) {
                        as.integer(2^port$config$bps - 1)
                      } else {
                        0L
                      },
                      PACKAGE=MYCLASS)))
      return (NULL)
    
    dim <- dim(extmat)
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim
    return (extmat)
  },

  start.up.xenex = function(port, ...) {
    ## connect to the "Virtual Radar Device" for this port
    ## If necessary, wait for the required warmup time.

    if (!port$is.open) {
      rss.gui(POPUP_MESSAGEBOX, "Opening device", "Please wait while I open the " %:% port$name %:% ".\nThis will take 10-15 seconds...", time.to.live = start.up.message.time)
      parms <- switch(port$type,
                      USB = port$config[c("antenna", "timeout", "sample.rate")],
                      RIB = port$config[c("antenna", "master", "timeout", "mode")],
                      TCPIP = c(list(get.host.login(known.hosts[port$devno - 1])), port$config$timeout)
                      )
      if (is.null(parms))
        stop("Port startup cancelled")

      .Call("start_up", port$devno, parms)
      port$is.open <- TRUE

      ## perform port configuration.  The parameters are already set
      ## in the R level port$config object, but have not been
      ## transmitted to the device.

      config(port, port$config)

      ## for the USB device, load antenna parameters from the database
      ## and send them to the device.  Why doesn't CSAPI_OpenXIR3000 do this??
      
      if (port$type == "USB") {
        .Call("load_antenna_parms", port$devno)
        ant(port, ant(port))
      }
    }
    return(TRUE)
  },

  shut.down.xenex = function(port, ...) {
    ## shut down this port's connection to a "Virtual Radar Device",
    ## if it has one

    if (port$is.open) {
      .Call("shut_down", port$devno)
      port$is.open <- FALSE
    }
    return(TRUE)
  },

  new.play.state.xenex = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  },

  ## utilities for debugging:

  xopen = function() .Call("my_open_xir3000", "Furuno FR1954C-BB", 1000L),
  xclose = function(h) .Call("my_close_device", h),
  xsb = function(h, v) if (missing(v)) .Call("my_get_standby_mode", h) else .Call("my_set_standby_mode", h, v),
  xmm = function(h, v) if (missing(v)) .Call("my_get_main_mode", h) else .Call("my_set_main_mode", h, v),
  xsd = function(h, v) if (missing(v)) .Call("my_get_sample_depth", h) else .Call("my_set_sample_depth", h, v),
  xms = function(h, v) if (missing(v)) .Call("my_get_master_mode", h) else .Call("my_set_master_mode", h, v),
  xpr = function(h) .Call("my_get_prg_type", h),
  xprf = function(h) .Call("my_get_PRF", h),
  xplen = function(h) .Call("my_get_PLEN", h),
  xcbl = function(h, v) if (missing(v)) .Call("my_get_cable_length", h) else .Call("my_set_cable_length", h, v),
  xvidn = function(h, v) if (missing(v)) .Call("my_get_video_neg", h) else .Call("my_set_video_neg", h, v),
  xctl = function(h, c, v) if (missing(v)) .Call("my_get_control_level", h, c) else .Call("my_set_control_level", h, c, v),
  xtrg = function(h) .Call("my_have_TRIG", h),
  xshm = function(h) .Call("my_have_SHM", h),
  xints = function(h) list(present=.Call("my_have_INTS", h), count=.Call("my_get_INTS", h)),
  xbp = function(h) list(present=.Call("my_have_BP", h), per.rotation=.Call("my_get_BPR", h)),
  xcls = function(h) .Call("my_clear_scan", h),
  xant = function(h) .Call("my_get_current_antenna", h),
  xrng = function(h, v) if (missing(v)) .Call("my_get_range", h) else .Call("my_set_range", h, v),
  xscn = function(h, v) .Call("my_get_scanline_raw", h, if (missing(v)) .Call("my_get_current_scan_number", h) else v)
  )  ## end of globals

hooks = list(
  ) ## end of hooks

## additional plugin variables

## any hosts discovered by get.hosts() are saved here at plugin load time
## so that the list of hosts is stable during a plugin session

known.hosts = NULL

## we cache the port list here, to simplify clean unloading
all.ports = NULL

## whether we have particular ports in all.ports
have.port.USB = FALSE
have.port.RIB = FALSE

## The ranges associated with the "range index".  This depends on which
## DSP version the board is using, as returned by CSAPI_GetPrgType.
## The values are from the documentation of CSAPI_GetRange, and are converted
## from nautical miles to metres by the factor of 1852.
## Because R arrays are indexed from 1, we must add 1 to the CSAPI range index
## to get the index into the appropriate vector here:
## e.g. with DSP9 and a CSAPI range index of 2, the true range is range.values$DSP9[3]
##
## FIXME: are these the ranges for a full 512 sample scanline, or for the 496 actual samples??
## These don't actually make sense, given the advertised sample rates of 100 and 54 Mhz.

range.values = list (
  DSP7 = 0.125 * 2^(0:8) * 1852,
  DSP9 = 0.375 * 2^(0:8) * 1852
  )

## the vendor code used by RTI for direct USB calls

RTI_USB_VEND_CODE = 0xD2L

## codes for direct USB commands to the XIR3000C

USB_CMD = list (
  vid.div        = 0x55L,
  vid.xgain      = 0x84L,
  vid.gain       = 0x85L,
  trig.offset.hi = 0x50L,
  trig.offset.lo = 0x51L
  )

## names of parameter obtained from CSAPI_INT_VP_ConfigFileRead
RTI_ANT_PARM_NAMES = c("TrgH", "TrgL", "TrgG", "TrgE", "BPH", "BPL", "BPG", "BPE", "SHMH", "SHML", "SHMG", "SHME", "TrgOffH", "TrgOffL", "VidDiv", "VidG", "VidXG", "VidThresh", "VidRef", "TuneRange", "NegVideo", "BPPPerSweep", "BirdAltitude")

scan.info.names = c("pulses", "samples.per.pulse", "bits.per.sample", "timestamp",
        "duration", "sample.dist", "first.sample.dist",
        "bearing", "orientation", "pulse.length", "PRF")
