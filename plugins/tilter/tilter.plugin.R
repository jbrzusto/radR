##  svn: $Id: tilter.plugin.R 786 2011-05-05 17:27:52Z john $
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
##

##     The TILTER radR plugin

## This plugin spawns a "tilter minder server" process to manage the
## actual tilter device and retain a history of its recent states.
## The plugin allows the user to set up, start, and stop a tilter
## pattern.  The plugin queries the server at the end of each scan to
## get estimates of the tilter's actual angle at a set of evenly
## spaced times during the scan.  This information is used in
## different ways:
##
## - if recording a raw archive, the time->angle map is stored in the
## metadata for each scan.  In most scans, the time->angle map will be
## constant and so stored as a single number.
##
## - if blips are being extracted, the time->angle map is used to
## calculate the angle of each pulse in the scan, before blip
## parameters such as position are estimated.


MYCLASS="tilter"

about = function() {
  ## a function returning a description and the current state of the plugin.
  rv <- "Control Dave Wilson's remote dish tilter, allowing volume scans etc.\n\n"

  if (!have.calibrated)
    rv <- paste(rv, "The antenna has NOT been calibrated to 0 degrees yet.", sep="\n")

  rv <- paste(rv, "The tilting pattern is:\n\n   angle range: ", paste(angle.range, collapse=" ... "),
              "\n   angle step: ", angle.step, "\n   time spent at each step: ", time.per.step, " sec\n\n",
              "The pattern mode is", volume.modes[volume.mode])
  
  return(rv)
}

enable = function(enab) {
  ## the START_SOURCE hook is used to quietly disable the plugin if 
  ## the current source is not a radar; so that hook is always left enabled
  for (h in names(hooks))
    rss.enable.hook(h, name, enab || h == "START_SOURCE")
  if (! enab)
    pattern.enable(FALSE)  ## stop any current pattern
}

load = function() {
  ## redundant, but mark that we've not calibrated the antenna to zero degrees
  have.calibrated <<- FALSE

  ## get the list of possible serial ports
  
  all.ttys <<- get.ttys()

  ## if the configured port is among those available, use it; otherwise, choose
  ## the first
  if (! (tty %in% all.ttys) && length(all.ttys) > 0)
    tty <<- all.ttys[1]

  if (init.port())
    return (TRUE)
  
  return (FALSE)
}

unload = function(save.config) {
  if (! is.null(sock)) {
    send.command("q") ## stop the server
    close(sock)
  }
}


get.menus = function() {
  list(
       plugin = list (
         "Pattern Status " %:% (if (start.stop.pattern.with.play) " - pattern starts/stops automatically with play" else " - pattern must be started/stopped manually"),
         list(option="choose.any",
              on.set = function(n, v) {
                if (v) start.pattern() else stop.pattern()
              },
              set.or.get = "pattern.enable",
              "running when checked; click here to start/stop" = FALSE
              ),
         "---",
         "Serial port for tilter controller:",
         c(list("choose.one",
                on.set = function(v) {
                  tty <<- all.ttys[v]
                  init.port()                
                }),
           structure(tty == all.ttys, names=all.ttys)
           ),
         "---",
         "Tilting pattern mode:",
         c(list("choose.one",
                on.set = function(v) {
                  set.volume.mode(v)
                }),
           structure(volume.mode == names(volume.modes), names = volume.modes)
           ),
         "---",
         list (low.angle = "gauge",
               label = "low angle (degrees)", 
               range = c(5, 85),
               increment = 1,
               value = angle.range[1],
               on.set = function(x) { angle.range[1]<<- x; update.pattern() }
               ),
         list (high.angle = "gauge",
               label = "high angle (degrees)", 
               range = c(5, 85),
               increment = 1,
               value = angle.range[2],
               on.set = function(x) { angle.range[2]<<- x; update.pattern() }
               ),
         list (angle.step = "gauge",
               label = "angle step (degrees)",
               range = c(1, 85),
               increment = 1,
               value = angle.step,
               on.set = function(x) { angle.step <<- x; update.pattern() }
               ),
         list (res.time = "gauge",
               label = "time to spend at each angle (seconds)",
               range = c(0.1, 10000),
               increment = 2.4,
               value = time.per.step,
               on.set = function(x) { time.per.step <<- x; update.pattern() }
               ),
         "---",
         "Immediate Commands:",
         "Calibrate antenna to 0 degrees..." = function() maybe.calibrate.to.zero("You have chosen to move the antenna to zero degrees."),
         "Tilt antenna to a specific angle..." = maybe.goto.angle,
##         "Start tilting pattern now" = start.pattern,
##         "Stop tilting pattern" = stop.pattern,
         "Next angle" = next.angle,
         "Restart tilting pattern" = restart.pattern,
         "Restart tilt server" = init.port
         )
       )
}


hooks = list(               
  START_SOURCE = list( enabled = TRUE, read.only = TRUE,
    f = function(...) {
      ## a new source has been started; this plugin's other hooks gets enabled
      ## only if the plugin is currently "enabled" and the source is live.
      enable (RSS$source$is.live & enabled)
    }
    ),
    
  GET_SCAN_INFO = list( enabled = FALSE, read.only = FALSE, 
    f = function(si) {
      if (! have.calibrated || !isTRUE(RSS$source$is.live))
        ## NB: need to check whether source is live because GET_SCAN_INFO
        ## might be called before START_SOURCE (via start.up() method)
        return(si) 
      
      angles <- as.numeric(send.command("L", unclass(si$timestamp), si$duration / 1000.0, 0.1))  ## get linear approximation to angle during the scan with maximum error of 0.1 degrees
      if (length(angles) == 0 || ! is.finite(angles[1])) {
        ## if there was an error retrieving angles, then mark the antenna angle as 0
        si$antenna.angle <- scantime.angle <<- 0
      } else {
        si$antenna.angle <- angles
        if (length(angles) == 1) {
          scantime.angle <<- angles
        } else {
          ## FIXME?: for learning purposes, we use the approximate time-averaged angle for this scan
          tdiff <- diff(angles[c(FALSE, TRUE)])  ## the even-numbered entries are timestamps
          weights <- (c(0, tdiff) + c(tdiff, 0)) / 2  ## count each angle for half the timespans on either side
          scantime.angle <<- sum(angles[c(TRUE, FALSE)] * weights) / sum(weights) ## the odd-numbered entries are angles
        } 
      }
      return(si)
    }),

  PRE_PROCESS = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (!RSS$previewing && volume.mode != "manual") {
        ## swap in the learning variables for the current angle
        ac <- flatten.angle(scantime.angle)
        ensure.learning.vars(ac)
        RSS$mean.mat <- mean.mats[[ac]]            
        RSS$dev.mat <- dev.mats[[ac]]

        ## Even if learning is finished at the previous angle, it
        ## might not be finished at the current angle; zero the class
        ## matrix to prevent the plot for the latter from being
        ## contaminated with class info from the former, and set num.blips
        ## to zero to prevent them from being written out again. (Before
        ## learning is complete, the class mat is not processed, and
        ## so is assumed to be entirely cold).

        if (RSS$scans.to.learn == 0 && scans.to.learn[[ac]] > 0) {
          zero(RSS$class.mat)
          zero(RSS$prev.class.mat)
          RSS$num.blips <- RSS$num.patches <- RSS$num.hot.samples <- 0
        }
        RSS$num.scans.learned <- num.scans.learned[[ac]]
        RSS$scans.to.learn <- scans.to.learn[[ac]]
        RSS$have.valid[have.valid.names] <- have.valid[[ac]]
      }
    }),

  POST_STATS = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (!RSS$previewing && volume.mode != "manual") {
        ## update local copies of learning variables for the current angle
        ## This does not need to be done for the mean and dev extmats,
        ## because they are manipulated as pointers.
        ac <- flatten.angle(scantime.angle)
        num.scans.learned[[ac]] <<- RSS$num.scans.learned
        scans.to.learn[[ac]] <<- RSS$scans.to.learn
        have.valid[[ac]] <<- RSS$have.valid[have.valid.names]
      }                       
    }),

  RESTART_LEARNING = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (volume.mode != "manual") {
        for (ac in names(scans.to.learn)) {
          scans.to.learn[[ac]] <<- RSS$default.scans.to.learn
          num.scans.learned[[ac]] <<- 0
          have.valid[[ac]] <<- RSS$have.valid[have.valid.names]
          have.valid[[ac]][c("stats", "classification", "scores")] <- c(FALSE, FALSE, FALSE)
          zero(mean.mats[[ac]])
          zero(dev.mats[[ac]])
        }
      }                       
    }),

  ONPLAY = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (start.stop.pattern.with.play && volume.mode != "manual")
        start.pattern()
    }),

  ONSTOP = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (start.stop.pattern.with.play)
        stop.pattern()
    }),
  
  ONPAUSE = list( enabled = FALSE, read.only = TRUE, 
    f = function(...) {
      if (start.stop.pattern.with.play)
        stop.pattern()
    })
  )

## (re) initialize the controller port

init.port = function() {
  if (! is.null(sock)) {
    ## shut down the existing server
    send.command("q")
    close(sock)
    Sys.sleep(1)
  }
  sock <<- NULL
  have.calibrated <<- FALSE
  
  ## start a server
  if (0 == system(paste(start.server.cmd, tty, server.port, log.filename, if (server.debug) "-d"))) {
    Sys.sleep(0.5)  ## wait for server to be ready
    timeout <- options("timeout")
    options(timeout = 2)
    try(sock <<- socketConnection("localhost", server.port, blocking=TRUE), silent=TRUE)
    options(timeout = timeout)
    if (!is.null(sock)) {
      set.volume.mode(volume.mode)
      update.pattern()
      return(TRUE)
    }
  }
  rss.gui(POPUP_MESSAGEBOX,
          "Can't start server",
          "The tiltminder server cannot be started.\nVerify that:\n - tiltserv1.1s.exe is in the plugins/tilter folder\n - the B&B Ulinx TTL5USB9M is connected to the serial port\n - the log file is valid"
          )
  return(FALSE)
}

## send a command to the server, returning the reply as
## a character vector with each slot representing one line of the reply
## up to but not including the "Ok" line.  The arguments are
## pasted together with spaces before sending the command.

send.command <- function(...) {
  if (is.null(sock))
    return (NULL)
  cat(paste(..., collapse=" "), '\n', sep="", file=sock)
  flush(sock)
  r <- character(0)
  repeat {
    line <- readLines(sock, 1)
    if (length(line) == 0 || line[1] == "Ok")
      break
    r <- c(r, line)
  }
  return (r)
}

## set the tilter angle, in degrees

set.tilter.angle = function(angle) {
  send.command("a", angle)
}

## set the volume mode
set.volume.mode = function (mode) {
  if (is.character(mode))
    mode <- which(mode == names(volume.modes))
  if(length(mode) == 0)
    stop("tilter: unknown volume mode")
  volume.mode <<- names(volume.modes)[mode]
  if (mode == 1)
    send.command("s") ## stop any running pattern - it's manual mode
  else
    send.command("e", mode - 1) ## set at_end_do mode
}

## calibrate antenna to zero degrees

calibrate.to.zero = function() {
  return (as.numeric(send.command("c")))
}

## confirm from user whether to calibrate to zero degrees; wait for a
## given delay (in seconds) after calibrating

maybe.calibrate.to.zero = function(msg = "The tilter has" %:% (if (have.calibrated) " already" else " not") %:% " been calibrated to 0 degrees.\n") {
  resp <- rss.gui(POPUP_DIALOG,
                  title = "WARNING: keep body away from antenna!",
                  msg = paste(msg, "The tilter motor is powerful, and serious injury could result if anyone\n is too close to the tilter!\nIf the antenna is already at 0 degrees AND the tilter has not been turned off\nsince the previous calibration, you can also choose to skip calibration.\n\nCalibrate the antenna angle now?", sep="\n"),
                  default = 1,
                  drop.down = TRUE,
                  buttons = c("No - do not calibrate the antenna now, but ask again next time",
                    "Yes - it is safe to tilt the antenna to 0 degrees. DANGER: antenna might begin tilting!",
                    "Skip calibration (only do this if you're sure!).   DANGER: antenna might begin tilting!")
                  )
  if (is.na(resp))
    return(FALSE)

  if (resp == 2) {
    wait <- calibrate.to.zero()
    if (isTRUE(wait >= 0)) {
      rss.gui(POPUP_MESSAGEBOX,
              title="Waiting for tilter calibration",
              msg = "To ensure accurate angles, I'm waiting " %:% round(wait) %:% " seconds for the tilter\nto finish calibrating",
              time.to.live = wait)
      rss.gui(UPDATE_GUI)
      Sys.sleep(wait) ## sleep while waiting for calibration to happen
    } else {
      rss.gui(POPUP_MESSAGEBOX,
              title="Error during calibration",
              msg = "An unknown error occurred while calibrating the tilter.\nCheck that the serial port is correct and connected.")
    }      
  } else if (resp == 3) {
    send.command("C", 0)
  }

  if (resp != 1)
    ## indicate we've calibrated (or have skipped it)
    have.calibrated <<- TRUE
  return(TRUE)
}

angle.is.safe = function(ang) {
  (ang >= safe.operating.range[1]) &&
  (ang <= safe.operating.range[2])
}

goto.angle = function(ang, force=FALSE) {
  if (!force && !angle.is.safe(ang)) {
    rss.gui(POPUP_MESSAGEBOX,
            "Angle out of range",
            sprintf("You have asked to set the tilter to %d degrees,\
which is not in the safe range from %d to %d degrees", ang, safe.operating.range[1], safe.operating.range[2])
            )
    return()
  }
  if (!have.calibrated)
    if (!maybe.calibrate.to.zero())
      return()
  set.tilter.angle(ang)
}

maybe.goto.angle = function() {
  realtime.angle <- as.numeric(send.command("lr", 0, 1, 0))
  resp <- rss.gui(POPUP_DIALOG,
                title = "WARNING: keep body away from antenna!",
                msg = "When you hit OK, the tilter will immediately begin raising or lowering the antenna.\nMAKE SURE NO ONE IS STANDING TOO CLOSE!\n\nEnter the target angle between 5 and 85 degrees:",
                  default = 1,
                  entry = TRUE,
                  default.entry = as.character(realtime.angle),
                  buttons = c("Cancel - do not tilt the antenna", "Ok - it is safe to tilt the antenna now. DANGER!")
                )
  if (is.na(resp[[1]]) || resp[[1]] == 1)
    return()

  goto.angle(as.integer(resp[[2]]))
}


## start the current pattern
start.pattern = function() {
  if (!have.calibrated)
    if (!maybe.calibrate.to.zero())
      return()
  send.command("r", -1, -1) ## start (or resume) imediately and run forever
  ## set GUI pattern running indicator
  pattern.enable(1, TRUE)
}

## stop the current pattern
stop.pattern = function() {
  send.command("s")
  ## clear GUI pattern running indicator
  pattern.enable(1, FALSE)
}

## restart the volume scan
restart.pattern = function(volindex = NULL) {
  if (!have.calibrated)
    if (!maybe.calibrate.to.zero())
      return()
  send.command("R")
  ## set GUI pattern running indicator
  pattern.enable(1, TRUE)
}

## Ensure we have a copy of learning variables for a given angle class.  This is
## used when generating a new pattern, and whenever we're about to copy these
## variables to the global RSS list, in case the user has forced the antenna to
## a new out-of-pattern angle.

ensure.learning.vars = function(ac) {
  ## ac is a character representation of an angle class
  if (is.null(mean.mats[[ac]])) {
    mean.mats[[ac]] <<- extmat(sprintf("means of radar data cells; angle class=%s", ac)     , type=RSS$types$frame_cell)
    dev.mats[[ac]]  <<- extmat(sprintf("deviations of radar data cells; angle class=%s", ac) , type=RSS$types$frame_cell)
    num.scans.learned[[ac]] <<- 0
    scans.to.learn[[ac]] <<- RSS$default.scans.to.learn
    have.valid[[ac]] <<- RSS$have.valid[have.valid.names]
    have.valid[[ac]][c("classification", "scores", "stats")] <<- FALSE
  } else {
    ## copies of the learning variables for this angle already exist, so just 
    ## use them. They can be reset by forcing a learning restart.
  }
}

## Create a new pattern.  Also, delete any learning vars corresponding
## to no-longer-used angles.  Other learning vars are preserved,
## but will be reset upon a full restart of learning.

update.pattern = function() {
  
  angle.tags <- unique(flatten.angle(seq(from = angle.range[1], to = angle.range[2], by = angle.step * sign(diff(angle.range)))))

  ## delete learning vars for no-longer-used angles
  ## Note that the extmats will be destroyed by their finalizer code
  ## when they're no longer referred to.

  drop.angles <- names(mean.mats)[! (names(mean.mats) %in% angle.tags)]
  for (ac in drop.angles) {
    mean.mats[[ac]] <<- NULL
    dev.mats[[ac]] <<- NULL
    num.scans.learned[[ac]] <<- NULL
    scans.to.learn[[ac]] <<- NULL
    have.valid[[ac]] <<- NULL
  }
  
  ## ensure we have learning vars for all angles in the new pattern,
  ## re-using extmats where possible.  We don't worry about the dimensions
  ## of these extmats since radR_update_stats ensures they're OK.
  for (ac in angle.tags)
    ensure.learning.vars(ac)

  send.command("p", angle.range[1], angle.range[2], angle.step, time.per.step)

  ## setting a new pattern reverts to the default volume mode (on the server
  ## side), but it's more intuitive to preserve the existing mode.
  
  set.volume.mode(volume.mode)
}


## Issue the command to tilt the radar to the next angle in the volume
## pattern.

next.angle = function() {
  if (!have.calibrated)
    if (!maybe.calibrate.to.zero())
      return()
  send.command("n")
}

## plugin variables

## the (mean) angle for the scan being processed
scantime.angle = 0

## have we calibrated the antenna angle since loading the plugin?

have.calibrated = FALSE

## the socket connection through which we communicate with the tiltminder server

sock = NULL

## the list of all potential controller serial ports, found at load time

all.ttys = NULL

## arrays of copies of learning variables; each is a list whose names
## are character strings, each representing an angle in the current
## volume pattern.  We index by strings so that we'll be able to use
## fractional angles in the future, and so that we don't have to
## allocate space for skipped or out-of-range angles.

mean.mats = list()
dev.mats = list()
num.scans.learned = list()
scans.to.learn = list()
have.valid = list()

## names of elements of RSS$have.valid which are swapped out for learning at
## different angles
have.valid.names = c("classification", "patches", "scores", "stats")
