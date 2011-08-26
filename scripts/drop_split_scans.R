## svn: $Id: drop_split_scans.R 595 2010-05-21 20:10:29Z john $

## skip processing of scans which are only partial updates from the
## preceding scan.  Used for processing blipmovies and raw archives
## where a bug in the seascan plugin(?)  in May, 2010, caused some
## extra scans with duplicated timestamps and only partially updated
## data.

## We just skip any scans whose timestamps are the same as the next scan
## in the archive.  The split scan problem appears to have generated a spurious
## extra scan with data from two real scans, and whose timestamp is that of
## the second real scan.

## Also, correct durations of zero (which may be due to the split scan problem)
## to 2400 milliseconds, corresponding to 25 RPM.

check.dup <- function(...) {
  ## assume that if two or more consecutive scans have the same
  ## timestamp, the last of them has the blip data from a fully
  ## updated scan, so discard all but the first

  if (! inherits(RSS$source, c("blipmovie", "rawarch")))
    return()

  ## Sets the global flag RSS$skip$all.processing if the current
  ## scan is to be discarded; assume not.
  
  RSS$skip$all.processing <- FALSE

  scan <- RSS$source$cur.scan
  if (scan < RSS$source$contents$num.scans[RSS$source$cur.run]) {
    ## not at last scan - check if next scan has same timestamp
    seek.scan(RSS$source, 0, scan + 1)
    if (get.scan.info(RSS$source)$timestamp == RSS$scan.info$timestamp) {
      ## discard this scan, wait for fully updated data in next one
      RSS$skip$all.processing <- TRUE
      
      ## for debugging
      ## gui.print.cons(sprintf("Skipping scan at %s", format(structure(RSS$scan.info$timestamp, class="POSIXct"))))
    } else {
      if (RSS$scan.info$duration == 0)
        RSS$scan.info$duration <- 2400
    }
    ## restore the source pointer position and metadata to current scan
    seek.scan(RSS$source, 0, scan)
    get.scan.info(RSS$source)
  }
  return(NULL)
}

## install the function on the FULL_SCAN hook (which is called after
## data for a scan has been obtained, but before any processing takes place)

rss.add.hook("FULL_SCAN", check.dup)
