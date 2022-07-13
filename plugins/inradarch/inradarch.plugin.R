##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2015 John Brzustowski
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


##           INRADARCH   PLUGIN
##
##  Read scans from a folder of .dat files written by the SG inrad client

##
## Format of files:

MYCLASS = "inradarch"

about = function() {
    return(plugin.label)
}

getSweep = function(f, port) {
    ##!@param f filename
    ##!@param port; port whose scan.data variable will store compressed data
    ##
    ##!@ returns sweepHeader list

    buf = port$scan.data = readBin(f, raw(), n=file.info(f)$size)
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

    sweepHeader              = list(
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

    return(sweepHeader)
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
            config = list(folder = folder),
            has.toc = TRUE,
            cur.run = 0,
            cur.scan = 0,
            prev.scan = -1,
            scan.data = NULL,                 ## the data, pre-read by get.scan.info
            contents = empty.TOC,
            start.time = NULL,                ## the time of the first sweep
            end.time = NULL,                ## the time of the first sweep
            duration = 0L,                    ## how long the folder lasts, in seconds
            si = NULL,                        ## scan info
            files = NULL,                     ## full paths to all files in archive
            num.scans = 0L,
            ts = NULL  ## timestamps of files, parsed from names
            ## (N.B. these are receipt times, always just
            ## after the end of the sweep true sweep
            ## timestamps are in file headers
            ),
                  class = c(MYCLASS, "strictenv"))
    }

    rv <- list()

    ##                    name    id  source  sink
    rv[[1]] <- my.port <<- make.port("Reader", 1,  TRUE, FALSE)
    rv
}

load = function() {
    rss.dyn.load("inradarch", in.dir.of=plugin.file, local=TRUE)
}

unload = function(save.config) {
    rss.dyn.unload("inradarch")
}

get.menus = function() {
    list(
        sources = list (
            titles = "Inrad Archive Reader",
            menu = list (
                options = "no-tearoff",
                "Choose a folder..." = gui.create.port.folder.selector(get.ports()[[1]])
                )
            ),
        plugin = list (
            )
        )
}

globals = list (

    as.character.inradarch = function(x, ...) {
        sprintf("radR interface port: %s: %s: %s",
                MYCLASS,
                x$name,
                if (is.null(x$config$folder)) "(no folder)" else x$config$folder
                )
    },

    print.inradarch = function(x, ...) {
        ## print a description of this port
        cat (as.character(x) %:% "\n")
    },

    config.inradarch = function(port, ...) {

        opts <- list(...)
        if (length(opts) != 0) {
            for (opt in names(opts)) {
                switch(opt,
                       folder = {
                           port$config$folder <- opts[[opt]]
                           port$contents <- empty.TOC  ## mark the table of contents as needing regeneration
                       },
                       {
                           rss.plugin.error("INRADARCH: unknown configuration option for port: " %:% opt)
                           return(NULL)
                       }
                       )
            }
        }
        return(port$config)
    },

    get.contents.inradarch = function(port, ...) {
        return(port$contents)
    },

    end.of.data.inradarch = function(port, ...) {
        ## return TRUE if there is no data left to be read
        ## on this port (e.g. if the end of a tape run has been hit)
        port$cur.scan >= port$contents$num.scans[port$cur.run]
    },

    get.scan.info.inradarch = function(port, ...) {
        ## gets the header information for the next scan

        port$cur.scan = port$cur.scan + 1

        SH = getSweep(port$files[port$cur.scan], port)

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

    get.scan.data.inradarch = function(port, extmat, ...) {
        ## copies the data for the current scan into the extmat
        ## the data have already been read by get.scan.info

        if (is.null(port$scan.data))
            return (NULL)

        dim <- c(port$si$samples.per.pulse, port$si$pulses)

        if (is.null(dim))
            stop("calling get.scan.data when RSS$scan.info has NULL dimension info")

        dim(extmat) <- dim
        dim(RSS$class.mat) <- dim
        dim(RSS$score.mat) <- dim

        if (!isTRUE(.Call("decompress_sweep", port$scan.data, pointer(extmat))))
            stop("problem decompressing sweep")

        return(extmat)
    },

    seek.scan.inradarch = function(port, run, scan, ...) {
        ## seek to a particular run and scan on the current source
        ## scan = integer NAN requests a seek past the last scan in
        ##        the current run (which must be the last run
        ##        for most modules)
        ## run = 0 represents the current run; but we only allow one run per inrad archive
        if (scan == port$cur.scan + 1)
            return()
        port$cur.scan <- scan - 1 ## adjust for immediate adding of +1 by get.scan.info
    },

    seek.time.inradarch = function(port, time, ...) {
        ## seek to the first scan at or after time in the current source
        ## scan = +Inf requests a seek past the last scan in
        ##        the current run (which must be the last run
        ##        for most modules)

        ## FIXME: do this

        return (NULL)
    },


    start.up.inradarch = function(port, ...) {
        ## determine the archive contents
        ## frame interval

        ## The contents will be treated as a single run.
        ##
        ## Returns a one-row dataframe with three elements:
        ## num.scans:   number of scans (frames) in the run
        ## start.time:  timestamp first scan
        ## end.time:    timestamp of last scan

        id <- rss.gui("POPUP_MESSAGEBOX", "Scanning folder for radar sweeps", "Please wait while I scan the specified folder for radar data.  This will take a while on large drives.")
        f = port$config$
        fs = port$config$firstScan

        port$files = dir(port$config$folder, recursive=TRUE, pattern="-sweep\\.dat$", full.names=TRUE)
	port$files = port$files[order(basename(port$files))]
        port$num.scans = length(port$files)
        port$ts = getFileTimestamp(basename(port$files))
        port$start.time <- port$ts[1]
        port$end.time <- tail(port$ts, 1)
        port$contents <- list (
            num.scans = port$num.scans,
            start.time = port$start.time,
            end.time = port$end.time
            )

        port$cur.run <- 1
        port$scan.data <- NULL
        port$config$filename <- port$files[1]
        rss.gui(DELETE_MESSAGEBOX, id)
        return (TRUE)
    },

    shut.down.inradarch = function(port, ...) {
        ## do whatever is required to minimize
        ## resource consumption by this port
        ## eg. stopping digitization and playback,
        ## closing files, etc.

        ## drop the current file data
        port$start.time <- NULL
        port$scan.data <- NULL
        return(TRUE)
    },

    new.play.state.inradarch = function(port, new.state, old.state, ...) {
        ## indicate to this port that radR is
        ## changing play state.
        return(NULL)
    }

    )  ## end of globals

hooks = list (
    )

getFileTimestamp = function(f) {
    ## return a vector of timestamps a vector of SensorGnome sweep filenames

    ## sample filename:
    ## Canso3-2313BBBK3402-0000021-2015-06-11T08-51-46.137Z-sweep.dat
    ## proj     ID           boot   ts

    parts = strsplit(f, "-", fixed=TRUE)
    return(ymd_hms(lapply(parts, function(x) paste(tail(x, 6)[1:5], collapse="-"))))
}

## what a table of contents looks like, initially
empty.TOC = list(
    start.time = structure(double(0), class = "POSIXct"),
    end.time = structure(double(0), class = "POSIXct"),
    num.scans = integer(0)
    )

## the one and only port
my.port = NULL
