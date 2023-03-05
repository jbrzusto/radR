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
##
################################################################################
##
## navnet2blips.R:  read from a live NavNet radar and create a blips.csv and/or a blipmovie and/or tracks files
##
## This is run from the main radR directory like so:
##
##   cd radR
##   rbatch [--no-progress] [--parm PARMFILE] --script scripts/navnet2blips.R [--no-blips] [--no-blipmovie] [--no-tracks] SECS SITE FOLDER
##
## where SECS indicates how many seconds of data to capture.  SECS=0 means continue indefinitely.
## SITE is a prefix for output filenames
## FOLDER is where output files will be written
##
## Blips output filenames will be SITE_YYYY-MM-DDTHH-MM-SS_to_YYYY-MM-DDTHH-MM_SS_blips.csv
## Blipmovie filenames will be SITE_YYYY-MM-DDTHH-MM-SS_to_YYYY-MM-DDTHH-MM_SS.bm[.i]
## Track output filenames will be SITE_YYYY-MM-DDTHH-MM-SS_to_YYYY-MM-DDTHH-MM_SS_tracks.csv

## options(error=recover, warn=2) ## for debugging

bail = function(...) {
  print("ARGV is")
  print(ARGV)
  stop(..., call.=FALSE)
}

do.overrides = function(what, where, over, then.do=function(...){}, valid=names(where)) {
  ## override parameters in strictenv/environment "where" (whose
  ## human-readable name is "what") with those in the list "over"
  ## report an error if any unknown parameters are provided.
  ## "then.do": a function called with the name of each valid parameter
  ## overridden

  for (n in names(over)) {
    if (!(n %in% valid))
      bail("The name '", n, "' is not a valid ", what, " parameter.  Valid names are:\n", paste(valid, collapse=", "))
    where[[n]] = over[[n]]
    then.do(n)
  }
}

## does the user want a progress indicator?

show.progress = !is.na(match("--show-progress", commandArgs()))
do.csv = is.na(match("--no-blips", commandArgs()))
do.bm  = is.na(match("--no-blipmovie", commandArgs()))
do.tracks  = is.na(match("--no-tracks", commandArgs()))

if (! do.csv && ! do.bm && ! do.tracks)
    stop("Error: you specified --no-blips AND --no-blipmovie AND --no-tracks, which means I have nothing to do!")

## extract the dirname from the command line and verify
## it's a directory

ARGV = commandArgs(trailingOnly = TRUE)
n = length(ARGV)
if (n < 3)
    stop("Error: you need to specify duration, sitename and directory")

folder = ARGV[n]

if (! file.exists(folder))
    stop("Error: non-existent directory: ", folder)

if (! file.info(folder)$isdir)
    stop("Error: file specified instead of directoy: ", folder)

site = ARGV[n - 1]
duration = ARGV[n-2]

if(show.progress)
  cat(sprintf("\nProcessing live NavNet radar\n"))

## open the inradarch

p = RADARCAM$get.ports()[[1]]

start.up(p)

## set up scan counters
rbatch.i = 0
rbatch.n = tc$num.scans[1]

## set up a progress reporting string
rbatch.prog = "Scan: %-21s  Elapsed: %-11s  Left: %-11s\r"
rbatch.summary = "Did from %-21s to %-21s in %-20s\n"

## set the port as the source for further processing
rss.set.port(p)

for (plug in c(if (do.csv) "saveblips", if (do.bm) "blipmovie", if (do.tracks) "tracker")) {
    if (!exists(toupper(plug))) {
        rss.load.plugin(plug)
        warning("The ", plug, " plugin was not loaded by default, so I loaded it.")
    }
}

## set up output filenames

TS = function(x) structure(x, class=c("POSIXt", "POSIXct"))

fob = sprintf("%s_%s_to_%s_blips.csv",
    site,
    format(TS(tc$start.time[1]), "%Y-%m-%dT%H-%M-%S"),
    format(TS(tc$end.time[1]), "%Y-%m-%dT%H-%M-%S")
    )

fobm = sub("_blips.csv$", ".bm", fob)
ftrk = sub("_blips", "_tracks", fob)

if (do.bm) {
    po = BLIPMOVIE$get.ports()[[2]]
    ## set the port for output
    rss.set.port(po, filename=fobm)

    RSS$recording = TRUE
    cat("Will create blipmovie: ", fobm, "\n")
}

if (do.csv) {
    rss.enable.plugin("saveblips")
    SAVEBLIPS$blip.filename = fob
    cat("Will create blips file: ", fob, "\n")
}

if (do.tracks) {
    rss.enable.plugin("tracker")
    TRACKER$track.filename <- sub(".csv", "", ftrk)
    TRACKER$csv.filename <- ftrk
    cat("Will create tracks file: ", ftrk, "\n")
}


## Read parameters values from the file specified by
##
##   --parm PARMFILE
##
## on the command line.  Any parameter values found there
## override those in the usual *.conf.R files.
##
## For an example of this file's syntax, see main/batchparm.R

read.parms = FALSE

i = match("--parm", ARGV)
if (!is.na(i)) {
    f = ARGV[i+1]
    if (!file.exists(f))
      bail("Cannot read parameter file ",  f)
    read.parms = TRUE
}

if (read.parms) {
  cat(sprintf("Reading parameter overrides from %s", f))
  flush(stdout())
  x = source(f)$value
  cat(" - ok.\n")

  if (length(x$find) > 0)
    do.overrides("blip finding",
                 RSS,
                 x$find,
                 valid=c("noise.cutoff", "blip.finding", "default.scans.to.learn", "stats.k",
                   "update.stats.while.blipping", "cell.dims", "blip.score.threshold",
                   "blip.exclude.blips.from.stats.update"
                   )
                 )

  if (length(x$blip) > 0)
    do.overrides("blip filtering",
                 RSS,
                 x$blip,
                 valid=c(ls(RSS, pattern="^blip\\."), "use.blip.filter.expr")
                 )

  if (length(x$antenna) > 0)
      ANTENNA$load.antenna.config(x$antenna)

  if (length(x$declutter) > 0) {
    if (! exists("DECLUTTER")) {
      rss.load.plugin("declutter")
    }
    if (! RSS$blip.filtering) {
      RSS$blip.filtering = TRUE
    }
    do.overrides("declutter",
                 DECLUTTER,
                 x$declutter,
                 function(n) {
                   ## if user specifies a non-NULL clutter file, load it
                   if (n == "clutter.filename") {
                     if (!is.null(DECLUTTER$clutter.filename)) {
                       DECLUTTER$load.clutter.file()
                       rss.enable.plugin("declutter")
                     } else {
                       ## if user specifies NULL clutter filename, disable the plugin
                       rss.disable.plugin("declutter")
                     }
                   }
                 })
  }

  ## do any tracker overrides

    if (length(x$tracker) > 0)
    do.overrides("tracker plugin",
                 TRACKER,
                 x$tracker,
                 function(n) {
                   if (is.function(TRACKER[[n]]))
                     ## set the environment for any function parameters to the tracker plugin environment
                     environment(TRACKER[[n]]) <- TRACKER
                 })

  if (length(x$mfc) > 0)
    do.overrides("MFC tracker model",
                 TRACKER$models$multiframecorr,
                 x$mfc,
                 function(n) {
                   if (is.function(TRACKER$models$multiframecorr[[n]]))
                     ## set the environment for any function parameters to the model's environment
                     environment(TRACKER$models$multiframecorr[[n]]) <- TRACKER$models$multiframecorr
                 })

  ## read in any zone file specifed
    if (! exists("ZONE")) {
      rss.load.plugin("zone")
   }
  if (is.character(x$zonefile)) {
    f = x$zonefile[1]
    if (nchar(f) > 0) {
      if (!file.exists(f))
        bail("The zone file ", f, " does not exist.")
      ZONE$load.zones(f)
    }
    ZONE$enable(TRUE)
  }
}

## give radR something to do when it finishes processing, namely to quit

rss.add.hook("ONPAUSE", function(){

## if the user requested a progress indicator, provide a hook for that

  rss.add.hook("DONE_SCAN", "rbatch", function(...) {
    rbatch.i <<- 1 + rbatch.i
    s = Sys.time()
    elap = difftime(s, rbatch.stime)
    if (show.progress) {
        cat(sprintf(rbatch.prog, format(round(RSS$scan.info$timestamp)), format(round(elap)), ifelse(duration > 0, duration - elap, Inf))
    }
    if (duration > 0) {
        if (elap >= duration) {
            rss.do.stop()
            if (do.bm) {
                RSS$recording = FALSE
                shut.down(po)
            }
            q()
        }
    }
    })
}

## mark the start time
rbatch.stime = Sys.time()

## pretend we hit play
RSS$new.play.state = RSS$PS$PLAYING

RSS$event.loop.sleeptime = 0

## start the event loop
go()

## we never get here, since the ONPAUSE handler quits
