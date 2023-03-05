##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2023 John Brzustowski
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

##  Read data from a pipe fed by radarcam.

MYCLASS = "radarcam"

about = function() {
  return(paste(plugin.label,
               "\nThis plugin acquires data from radarcam, a golang radar sweep broker.",
               "\nRadarcam receives data from a Furuno NavNet radar"
         ))
}

get.ports = function() {
  list(structure(strictenv(
                           name                   = "NavNet radar via radarcam",
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
  ## cleanly close the connection to radarcam
  try ({
    shut.down(port)
  }, silent=TRUE)
}

get.menus = function() {
  rv <- list(
             sources = list (
               titles = "Radarcam",
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
             ))
  rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], port$name)
  return(rv)
}

get.sweep <- function() {
    ## get a sweep from the named pipe where radarcam writes it
    save.error = options()$error
    options(error=NULL)
    buf = raw()
    while (length(buf) < 128) {
        try({
            rawbuf <- readBin(rcpipe, raw(), n=128 - length(buf))
            buf <- c(buf, rawbuf)
        }, silent=TRUE)
        Sys.sleep(0.1)
    }
    pos = 1
    GI = function() {
        val = readBin(buf[pos+0:3], integer(), size=4, n=1)
        pos <<- 4 + pos
        return (val)
    }

    GS = function(n) {
        val = readBin(buf[pos+(0:(n-1))], character())
        pos <<- 4 + n;
        return (val)
    }

    ## field definitions in the bscan structure are as provided
    ## in the header file with the inrad radR plugin, and translated
    ## from inradClient.js

    hdr              = list(
        header_size          = GI(),
        rev_number           = GI(),
        samples_per_line     = GI(),
        num_output_lines     = GI(),
        data_size            = GI(), ## bytes
        compression_state    = GI(),
        compressed_size      = GI(), ## bytes
        acp_count_range      = GI(),
        num_source_lines     = GI(),
        time_stamp_seconds   = GI(), ## seconds since unix epoch, 00:00 UTC 1/1/1970
        time_stamp_useconds  = GI(), ## microseconds since unix epoch, 00:00 UTC 1/1/1970
        scan_duration        = GI(), ## microseconds
        rotation_period      = GI(), ## milliseconds
        prf                  = GI(), ## milliHz
        sample_rate          = GI(), ## samps per second * 10
        range_per_sample     = GI(), ## mm
        start_range          = GI(), ## mm
        end_range            = GI(), ## mm
        adc_gain             = GI(), ## dB * 100
        adc_offset           = GI(), ## code, 0-1024
        heading              = GI(), ## degrees * 1e6
        time_zone            = GI(),
        antenna_elevation    = GI(), ## degrees * 1e6
        latitude             = GI(), ## +/- degrees.decimal * 1e7
        longitude            = GI(), ## +/- degrees.decimal * 1e7
        altitude             = GI(), ## mm from datum
        pl_mode              = GI(), ## pulse length table entry record number
        pl_name              = GS(16)
    )
    samples.per.pulse <<- hdr$samples_per_line
    num.pulses <<- hdr$num_output_lines
    print(hdr)
    ## accumulate the full data matrix, possibly reading multiple times from the fifo
    i = 0
    n = samples.per.pulse * num.pulses
    intbuf = integer()
    while (n > 0) {
        try({
            part = readBin(rcpipe, integer(), n, signed=FALSE, size=1)
            intbuf = c(intbuf, part)
            n = n - length(part)
            Sys.sleep(0.00001)
        }, silent=TRUE)
    }
    dat <<- matrix(intbuf, samples.per.pulse, num.pulses)
    options(error=save.error)
    return(hdr)
}

## methods for the radarcam port:

globals = list (

  as.character.radarcam = function(x, ...) {
    sprintf("radR interface port: %s: %s",
            MYCLASS,
            x$name
            )
  },

  print.radarcam = function(x, ...) {
    ## print a description of this port
    cat (as.character(x) %:% "\n")
  },

  config.radarcam = function(port, ...) {
    ## nothing to do yet
    return(port$config)
  },

  end.of.data.radarcam = function(port, ...) {
    ## return TRUE if there is no data left to be read from the device
    ## (e.g. if digitizing has stopped)
    return(! file.exists(pipename))
  },

  get.scan.info.radarcam = function(port, scan.mat, trv, trv.index, ...) {
    ## gets the header information for the next scan
    ## (and actually gets the next scan)
        port$cur.scan = port$cur.scan + 1

        SH = get.sweep()

        port$si <- list(pulses = SH$num_output_lines,

                        samples.per.pulse = SH$samples_per_line,

                        bits.per.sample = 8 * (SH$data_size / (SH$samples_per_line * SH$num_output_lines)),

                        timestamp = structure(SH$time_stamp_seconds + SH$time_stamp_useconds / 1e6, class=class(Sys.time())),

                        duration = SH$scan_duration / 1e6,

                        sample.dist = SH$range_per_sample / 1e3,

                        first.sample.dist = SH$start_range / 1e3,

                        bearing = SH$heading,

                        orientation = +1,

                        is.rectangular = FALSE,

                        adc.gain = SH$adc_gain,

                        adc.offset = SH$adc_offset

                        )

        print(port$si)
        return(port$si)
  },

  get.scan.data.radarcam = function(port, extmat, ...) {
    ## gets the data for the scan whose info was most recently retrieved
    ## get.scan.info does most of the work.

    if (np == 0)
      return (NULL)

    dim <- c(samples.per.pulse, num.pulses)
    dim(extmat) <- dim
    dim(RSS$class.mat) <- dim
    dim(RSS$score.mat) <- dim

    extmat[] <- dat[]

    return (extmat)
  },

  start.up.radarcam = function(port, restart=FALSE,...) {
    ## connect to radarcam named pipe
      if (is.null(rcpipe)) {
          rcpipe <<- socketConnection("localhost", port=8123, server=TRUE, blocking=FALSE, timeout=-1, open="rb")
      }
    port$is.open <- TRUE
    have.more.data <<- TRUE
  },

  shut.down.radarcam = function(port, ...) {
    ## shut down this port's multicast listener
    ## if it has one

    if (port$is.open) {
      if (!is.null(rcpipe))
        close(rcpipe)

      rcpipe <<- NULL
      have.more.data <<- FALSE
      port$is.open <- FALSE
    }
    return(TRUE)
  },

  new.play.state.radarcam = function(port, new.state, old.state, ...) {
    ## indicate to this port that radR is
    ## changing play state.

  }

  )  ## end of globals

hooks = list(
  ) ## end of hooks

## additional plugin variables

## the port for the (one and only) radarcam device, set at load time

port = NULL

## the pipe connecting to radarcam
rcpipe = NULL

## standard parameter values for radarcam

samples.per.pulse = 884
num.pulses = 0
bits.per.sample = 8

have.more.data = FALSE

## sweep buffer
dat = NULL

np = 0
