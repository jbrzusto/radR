##  svn $Id: process_one.R 682 2010-11-22 16:31:45Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006, 2007, 2008 John Brzustowski        
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
## process_one.R:  process a single blipmovie file to extract tracks
##
## This is run from the main radR directory like so:
##
##   cd radR
##   R --no-save --args --no-gui --source main/process_one.R [--show-progress] [--parms PARMOVERRIDEFILE ] FILENAME
##
## where FILENAME is the blipmovie or raw archive filename, and can have any of the
## file extensions associated with the blipmovie (.bma, .blp, .smp,
## .scr, .bm, .bm.i) or raw archive (.raw.biglist, .raw.biglist.i), or no extension.
##
## If FILENAME has an extension, that is used to determine whether to process it
## as an old (blipmoviearch plugin) or new (blipmovie plugin) blipmovie or raw archive.
##
## If FILENAME has no extension, but there exist files with extensions only
## for the old (.bma, .blp, .smp, .scr) or only for the new (.bm, .bm.i)
## or for the raw archive formats, then the script will choose the appropriate plugin.
##
## If FILENAME has no extension and there are files with extensions for more than
## one of the archive formats, an error is reported.

bail <- function(...) {
  print("ARGV is")
  print(ARGV)
  stop(..., call.=FALSE)
}

do.overrides <- function(what, where, over, then.do=function(...){}, valid=names(where)) {
  ## override parameters in strictenv/environment "where" (whose
  ## human-readable name is "what") with those in the list "over"
  ## report an error if any unknown parameters are provided.
  ## "then.do": a function called with the name of each valid parameter
  ## overridden

  for (n in names(over)) {
    if (!(n %in% valid))
      bail("The name '", n, "' is not a valid ", what, " parameter.  Valid names are:\n", paste(valid, collapse=", "))
    where[[n]] <- over[[n]]
    then.do(n)
  }
}
  
## does the user want a progress indicator?

show.progress <- !is.na(match("--show-progress", commandArgs()))

## extract the filename from the command line and try
## to determine whether the blipmovie is in old or new format

ARGV <- commandArgs(trailingOnly = FALSE)
filename <- tail(ARGV, 1)

if (length(grep("\\.(blp|bma|smp|scr)$", filename, perl=TRUE)) > 0) {
  which.format <- 1
} else if (length(grep("\\.(bm|bm\\.i)$", filename, perl=TRUE)) > 0) {
  which.format <- 2
} else if (length(grep("\\.raw\\.(biglist|biglist\\.i)$", filename, perl=TRUE)) > 0) {
  which.format <- 3
} else {
  have.format <- file.exists(paste(filename, c(".bma", ".bm", ".raw.biglist"), sep=""))
  if (sum(have.format) > 1)
    bail("process_one.R:  ambiguous file specification.\nThere are files named",
         filename,
         "\nwith extensions for two or more of old (bma, blp, smp, scr) and new (bm, bm.i)",
         "\nblipmovie archives and/or raw (.raw.biglist) archives.  Please",
         "\nspecify the filename with an extension to select one of the archives.")
  which.format <- which(have.format)
}
filename <- gsub("\\.(blp|bma|smp|scr|bm|bm\\.i|raw\\.biglist|raw\\.biglist\\.i)$", "", filename, perl=TRUE)
if (! any(file.exists(paste(filename, c(".bm", ".bma", ".raw.biglist"), sep="")))) {
  bail("File does not exist: ", filename)
}

if(show.progress)
  cat(sprintf("\nProcessing file %-63s\n", filename))

## kludge: RAWARCH needs .raw.biglist extension on filename
if (which.format == 3) {
  real.filename <- paste(filename, ".raw.biglist", sep="")
} else {
  real.filename <- filename
}

## open the blipmovie as a radR port of the correct type

p <- switch(which.format, BLIPMOVIEARCH, BLIPMOVIE, RAWARCH)$get.ports()[[1]]

config(p, filename=real.filename)
start.up(p)

## get the table of contents

tc <- get.contents(p)

## fail if more than one run (lazy programmer)

if (length(tc$num.scans) > 1)
  bail("The file '", filename, "'\nhas more than one run, and rbatch can't handle this yet.  Tell jbrzusto@ncf.ca")

## set up scan counters
rbatch.i <- 0
rbatch.n <- tc$num.scans[1]

## set up a progress reporting string
rbatch.prog <- "Scan: %-21s  Elapsed: %-11s  Left: %-11s\r"
rbatch.summary <- "Did from %-21s to %-21s in %-20s\n"
                            
## set the port as the source for further processing
rss.set.port(p)

## seek to the start of the first scan in the first run
seek.scan(p, 1, 1)

if (!exists("TRACKER")) {
  rss.load.plugin("tracker")
  warning("The tracker plugin was not loaded by default, so I loaded it.")
}

## set up tracker output filenames
TRACKER$track.filename <- paste(filename, "_tracks", sep="")
TRACKER$csv.filename <- paste(TRACKER$track.filename, ".csv", sep="")

## Read parameters values from the file specified by
##
##   --parm PARMFILE
##
## on the command line.  Any parameter values found there
## override those in the usual *.conf.R files.
##
## For an example of this file's syntax, see main/batchparm.R

read.parms <- FALSE

i <-match("--parm", ARGV)
if (!is.na(i)) {
    f <- ARGV[i+1]
    if (!file.exists(f))
      bail("Cannot read parameter file ",  f)
    read.parms <- TRUE  
}

if (read.parms) {
  cat(sprintf("Reading parameter overrides from %s", f))
  flush(stdout())
  x <- source(f)$value
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
           
  if (length(x$antenna) > 0) {
    e <- as.strictenv(ANTENNA$parms)
    do.overrides("antenna",
                 e,
                 x$antenna,
                 function(n) {
                   ## mark that we're overriding source antenna info for this parameter
                   ANTENNA$override[match(n, names(ANTENNA$parms))] <- TRUE                 
                 })
    for (p in names(e)) 
      ANTENNA$parms[[p]] <- e[[p]]                 
  }

  if (length(x$declutter) > 0) {
    if (! exists("DECLUTTER")) {
      warning("Since you specified parameters for declutter, I'm loading that plugin.\n(your radR installation does not load it by default)\n")
      rss.load.plugin("declutter")
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

  ## read in any zone file specifed
  
  if (is.character(x$zonefile)) {
    f <- x$zonefile[1]
    if (nchar(f) > 0) {
      if (!file.exists(f))
        bail("The zone file ", f, " does not exist.")
      ZONE$load.zones(f)
    }
  }
}

## give radR something to do when it finishes processing, namely to quit

rss.add.hook("ONPAUSE", function(){
  if (show.progress) {
    s <- Sys.time()
    elap <- difftime(s, rbatch.stime)
    cat(sprintf(rbatch.summary, format(round(tc$start.time[1], 1)), format(structure(round(RSS$scan.info$timestamp, 1), class="POSIXct")), format(round(elap, 1))))
  }   
  q()
})

## if the user requested a progress indicator, provide a hook for that

if (show.progress) {
  rss.add.hook("DONE_SCAN", "rbatch", function(...) {
    rbatch.i <<- 1 + rbatch.i
    s <- Sys.time()
    elap <- difftime(s, rbatch.stime)
    eta <- difftime(rbatch.stime + as.numeric(difftime(s, rbatch.stime, units="secs")) * rbatch.n / rbatch.i, s)
    cat(sprintf(rbatch.prog, format(structure(round(RSS$scan.info$timestamp,1), class="POSIXct")), format(round(elap)), format(round(eta))))
  })
}
                                
## enable the tracker plugin
rss.enable.plugin("tracker")

## mark the start time
rbatch.stime <- Sys.time()

## pretend we hit play
RSS$new.play.state <- RSS$PS$PLAYING

## start the event loop 
go()

## we never get here, since the ONPAUSE handler quits
