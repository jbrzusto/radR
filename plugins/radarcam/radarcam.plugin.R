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
        config                 = list(
            sweepURL = default.sweepURL,
            timeout = default.timeout),
        has.toc                = FALSE,
        si                     = NULL,
        cur.run                = 0,
        cur.scan               = 0,
        prev.scan              = -1,
        urlcon                 = NULL,
        urlhost                = NULL,
        urlpath                = NULL
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
            list ("string",
                  label = "url from which to read sweeps",
                  width = 40,
                  height = 2,
                  value = default.sweepURL,
                  val.check = function(x)TRUE,
                  on.set = function(x) {
                      default.sweepURL <<- x
                      config(port, sweepURL=x)
                  }
                  ),
            list ("gauge",
                  label = "timeout (seconds); if no sweep read within timeout, source is deemed finished",
                  range = c(5, 300),
                  increment = 5,
                  value = default.timeout,
                  on.set = function(x) {
                      default.timeout <<- x
                      config(port, default.timeout=x)
                  }
                  )
            ))
    rv$sources$menu[[2]] <- c(rv$sources$menu[[2]], port$name)
    return(rv)
}

parseURL <- function(url) {
    re = '^(?:(?P<scheme>https?|ftp):/)?/?(?:(?P<username>.*?)(?::(?P<password>.*?)|)@)?(?P<hostname>[^:/\\s]+)(?::(?P<port>[^/]*))?(?P<path>(?:/\\w+)*/)(?P<filename>[-\\w.]+[^#?\\s]*)?(?P<query>\\?(?:[^#]*))?(?:(?:#(?P<fragment>.*)))?$'
    cap = regexpr(re, url, perl=TRUE)
    start = attr(cap, "capture.start")
    len = attr(cap, "capture.length")
    end = start + len - 1
    parts = list()
    for (x in seq(along=attr(cap, "capture.names"))) {
        parts[[attr(cap, "capture.names")[x]]] = substring(url, start[x], end[x])
    }
    return(parts)
}

get.sweep <- function(port) {
    ## get a sweep from the port
    ## Do manual http fetching because I can't figure out how else to get
    ## R to persist an http connection, which is needed to play nice with socat
    ## and also works much more efficiently even with an ssh-tunnelled port.
    ##
    ## As of 2023-04-20, the HTTP response from the radarcam sweep server looks like this:
    ##    HTTP/1.1 200 OK
    ##    Content-Length: XXXXXX
    ##    Content-Type: application/radarcam
    ##    Date: Thu, 20 Apr 2023 12:35:28 GMT
    ##    [blank line]
    ##    [XXXXXX bytes of compressed binary radar sweep data]

    if (!tryCatch({
        start = Sys.time()
        while(as.numeric(Sys.time() - start) < port$config$timeout) {
            req = sprintf("GET %s HTTP/1.1\r\nHost: %s\r\n\r\n", port$urlpath, port$urlhost)
            cat(req, file=port$urlcon)
            rep = readLines(port$urlcon, n=5)
            n = as.integer(substring(rep[2], 17))
            buf <<- readBin(port$urlcon, raw(), n=n)
# DEBUG:            cat(sprintf("Read sweep: %d bytes\n", length(buf)))
            if (length(buf) > 0) {
                break
            }
        }
        if (length(buf) == 0) {
            stop("timeout trying to read sweep from radarcam")
        }
        TRUE
    }, error = function(e) {
        print(e)
        end.of.data <<- TRUE
        FALSE
    })) {
        return(NULL)
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
    n = samples.per.pulse * num.pulses
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
        opts <- list(...)
        if (length(opts) != 0) {
            for (opt in names(opts)) {
                switch(opt,
                       sweepURL = {
                           port$config$sweepURL <- opts[[opt]]
                       },
                       timeout = {
                           port$config$timeout <- opts[[opt]]
                       },
                       {
                           rss.plugin.error("radarcam: unknown configuration option for port: " %:% opt)
                           return(NULL)
                       }
                       )
            }
        }
        return(port$config)
    },

    end.of.data.radarcam = function(port, ...) {
        ## return TRUE if the the last sweep was received more than timeout seconds ago
        ## (e.g. if capture by radarcam has stopped)
        return(end.of.data || (last.sweep.ts != 0 && as.numeric(Sys.time() - last.sweep.ts) >= port$config$timeout))
    },

    get.scan.info.radarcam = function(port, scan.mat, trv, trv.index, ...) {
        ## gets the header information for the next scan
        ## (and actually gets the next scan)
        port$cur.scan = port$cur.scan + 1

        SH = get.sweep(port)
        if (is.null(SH)) {
            return(NULL)
        }

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

        return(port$si)
    },

    get.scan.data.radarcam = function(port, extmat, ...) {
        ## gets the data for the scan whose info was most recently retrieved
        ## get.scan.info does most of the work.

        dim <- c(samples.per.pulse, num.pulses)
        dim(extmat) <- dim
        dim(RSS$class.mat) <- dim
        dim(RSS$score.mat) <- dim

        ## copies the data for the current scan into the extmat
        ## the data have already been read by get.scan.info

        if (is.null(buf))
            return (NULL)

        if (!isTRUE(.Call("decompress_sweep", buf, pointer(extmat))))
            stop("problem decompressing sweep")

        return(extmat)
    },

    start.up.radarcam = function(port, restart=FALSE,...) {
        port$is.open <- TRUE
        if (!is.null(port$urlcon)) {
            close(port$urlcon)
            port$urlcon = NULL
        }
        parts = parseURL(port$config$sweepURL)
        if (parts$scheme != "http") {
            stop("can't use an https source for radarcam sweeps")
        }
        port$urlcon <- socketConnection(parts$hostname, parts$port, open="a+b", blocking=TRUE)
        port$urlhost <- parts$hostname
        if (parts$path == "/") {
            parts$path = ""  ## otherwise, we get two "//"  at the start of a simple path like "/sweep"
        }
        port$urlpath <- file.path(parts$path, parts$filename)
        end.of.data <<- FALSE
    },

    shut.down.radarcam = function(port, ...) {
        ## shut down this port's multicast listener
        ## if it has one

        if (port$is.open) {
            port$is.open <- FALSE
            close(port$urlcon)
            port$urlcon <- NULL
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

## timestamp for the last received sweep (if non-zero)
last.sweep.ts = 0

## standard parameter values for radarcam

samples.per.pulse = 884
num.pulses = 0
bits.per.sample = 8

## sweep buffer
buf = NULL

## flag to mark timeout or error fetching data
end.of.data = FALSE
