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

##  Read data from Russell Technologies Inc's SIMRAD USB video processor.

MYCLASS = "simrad"

about = function() {
  return(paste(plugin.label,
               "\nThis plugin acquires data from a Simrad radar.  The radar multicasts pulse data",
               "\nover ethernet, so the radar, a Simrad display/control unit, and this PC need to be",
               "\nconnected together on a local network."
         ))
}

get.ports = function() {
  list(structure(strictenv(
                           name                   = "Simrad radar on ethernet",
                           is.source              = TRUE,
                           is.sink                = FALSE,
                           is.live                = TRUE,
                           is.file                = FALSE,
                           is.seekable            = FALSE,
                           is.open                = FALSE,
                           can.specify.start.time = FALSE,
                           config                 = list(),
                           has.toc                = FALSE,
                           si                     = NULL,
                           cur.run                = 0,
                           cur.scan               = 0,
                           prev.scan              = -1
                           ),
                 class = c(MYCLASS, "strictenv")))
}

load = function() {
  ## get the one and only port
  port <<- get.ports()[[1]]
}

unload = function(save.config) {
  ## cleanly close the connection to the Simrad
  try ({
    shut.down(port)
  }, silent=TRUE)
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "Simrad radar",
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
               "Choose a raw file to read instead of ethernet multicast..." =
               list ("file",
                     type = "open.one",
                     title = "Choose a raw file of multicast data",
                     file.types = list(".dat" = "raw multicast data"),
                     on.set = function(f) {
                       raw.filename <<- f
                       if(port$is.open) {
                         shut.down(port)
                         start.up(port)
                       }},
                     init.file = function() raw.filename
                     ),
               "Read from ethernet multicast" = function() {
                 raw.filename <<- NULL
                 if (port$is.open) {
                         shut.down(port)
                         start.up(port)
                       }}
##               "Restart multicast listener" = restart.multicast
             ))
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], port$name)
  return(rv)
}

get.bytes <- function(n) {
  if (buf$pos + n - 1 <= length(buf$dat)) {
    buf$pos <<- buf$pos + n
    return (buf$dat[(buf$pos-n):(buf$pos-1)])
  } else {
    if (buf$pos <= length(buf$dat))
      buf$dat <<- c(buf$dat[buf$pos:length(buf$dat)], readBin(mcast.pipe, integer(), size=1, n=buf$inc, signed=FALSE))
    else
      buf$dat <<- readBin(mcast.pipe, integer(), size=1, n=buf$inc, signed=FALSE)
   
    buf$pos <<- 1
    if (length(buf$dat) == 0)
      ## pipe broken or end of file
      have.more.data <<- FALSE
    return(get.bytes(n))
  }
}

pushback.bytes <- function(bytes) {
  if (buf$pos < length(buf$dat))
    buf$dat <<- c(bytes, buf$dat[buf$pos:length(buf$dat)])
  else
    buf$dat <<- bytes
  buf$pos <<- 1
}

get.sweep <- function() {
  ## get as much data as possible until the azimuth angle crosses 0
  ## return the number of pulses read
  ## data are stored in plugin variable dat
  ## azimuth angles (0..4095 of compass rotation) are returned in plugin variable azis
  
  azis <<- integer(max.pulses)
  np <<- 0
  dat <<- matrix(integer(), samples.per.pulse, max.pulses)
  last.azi <- -1L

  while(TRUE) {
    block[] <<- get.bytes(536)
    ## get a likely header + data block
    if (block[1] == 1) {
      ## skip 8 bytes of unknown info
      block[] <<- c(block[-(1:8)], get.bytes(8))
    }
    if (block[1] != 0x18) {
      stop("unknown block type", block[1])
    }
    ## get azimuth for this pulse (azimuth is in range [0..4096])
    azi <- block[9] + 256L * block[10]
    if (azi < last.azi) {
      pushback.bytes(block)
      break
    }
    np <<- 1 + np
    azis[np] <<- last.azi <- azi
    dat[, np] <<- block[dat.ind]
  }
  return(np)
}

## methods for the simrad port:

globals = list (

  as.character.simrad = function(x, ...) {
    sprintf("radR interface port: %s: %s",
            MYCLASS,
            x$name
            )
  },

  print.simrad = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.simrad = function(port, ...) {
    ## nothing to do yet
    return(port$config)
  },

  end.of.data.simrad = function(port, ...) {
    ## return TRUE if there is no data left to be read from the device
    ## (e.g. if digitizing has stopped)
    return(! have.more.data)
  },

  get.scan.info.simrad = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next scan
    ## (and actually gets the next scan)
    np <- get.sweep()
    
    return (list(pulses = desired.pulses,
                 samples.per.pulse = samples.per.pulse,
                 bits.per.sample = bits.per.sample,
                 timestamp = Sys.time() - 2.5, ## roughly!
		 duration = 2500,
                 sample.dist = 5,
                 first.sample.dist = 0,
		 orientation = 1,
                 PRF = PRF,
                 PLEN = PLEN,
                 true.np = np)
            )
  },

  get.scan.data.simrad = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does most of the work.

    if (np == 0)
      return (NULL)
    
    dim <- c(samples.per.pulse, desired.pulses)
    dim(extmat) <- dim
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim

    ## do gating;
    ## we have np pulses in dat (512 samples each), corresponding
    ## to np azimuths (compass-sweep fraction from 0..4095) in azis

    ## use linear approximation for gating
    
    extmat[] <- dat[, approx(azis[1:np], 1:np, ((1:desired.pulses)-1) / desired.pulses * 4095, method="constant", rule=2, f=0.5)$y]
    
    return (extmat)
  },

  start.up.simrad = function(port, restart=FALSE,...) {
    ## connect to the SIMRAD
    if (is.null(raw.filename))
      mcast.pipe <<- pipe(make.multicast.cmd(multicast.addr, multicast.port), "rb")
    else
      mcast.pipe <<- file(raw.filename, "rb")
    port$is.open <- TRUE
    have.more.data <<- TRUE
  },

  shut.down.simrad = function(port, ...) {
    ## shut down this port's multicast listener
    ## if it has one

    if (port$is.open) {
      if (!is.null(mcast.pipe))
        close(mcast.pipe)
      
      mcast.pipe <<- NULL
      have.more.data <<- FALSE
      port$is.open <- FALSE
    }
    return(TRUE)
  },

  new.play.state.simrad = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }

  )  ## end of globals

hooks = list(
  ) ## end of hooks

## additional plugin variables

## the port for the (one and only) SIMRAD device, set at load time

port = NULL

## the pipe connecting to the multicast reader
mcast.pipe = NULL
  
## standard parameter values for the Simrad

max.pulses = 2048
samples.per.pulse = 512
bits.per.sample = 8
PRF = 200
PLEN = 0.001 

have.more.data = FALSE

## a buffer for holding data from the Simrad  
buf = list(pos=1, dat=integer(), inc = 53600)

dat.ind = 25:536
coords = 1:512

## sweep buffer
dat = NULL

## sweep azimuths
azis = NULL

block = integer(536)

np = 0

## if set, the following is used as a source, rather than reading
## from ethernet multicast

raw.filename = NULL
