##  svn $Id: fixraw.R 51 2008-11-07 12:56:35Z john $
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
#####################################################################################
## fixraw.R - rebuild the table of contents and/or index for a raw archive         ##
##                                                                                 ##
## Due to the rawarch bug fixed in radR27, some raw archives have                  ##
## their index files wiped out (by filling both columns with NA),                  ##
## except possibly for the entry at index [[1]], which holds the table             ##
## of contents that is written at the end of the biglist data file.                ##
## This will have happened if more than one raw archive was recorded               ##
## in the same radR session.                                                       ##
##                                                                                 ##
## This function will attempt to rebuild the index file and TOC as follows:        ##
##                                                                                 ##
## - load the existing bigframe index (BFI)                                        ##
##                                                                                 ##
##   - save a copy of the existing index file under "f.i.saved", unless the latter ##
##   already exists                                                                ##
##                                                                                 ##
## - if the BFI has any NA rows, or if its size does not match                     ##
##   that expected given the biglist data file (BDF) size, it needs to be rebuilt: ##
##                                                                                 ##
##   - make a pass through the biglist data file, unserializing                    ##
##   one element at a time from the the file, and noting its file location         ##
##   size, and R class. (do this without opening as a biglist, but just with       ##
##   R code)                                                                       ##
##                                                                                 ##
##   - rebuild a table of contents                                                 ##
##                                                                                 ##
##   - rewrite the bigframe                                                        ##
##                                                                                 ##
##   - write the rebuilt table of contents to the biglist file at slot [[1]]       ##
##                                                                                 ##
#####################################################################################

##  This function needs access to the radR "bigframe" and "biglist" packages,
##  whose path (absolute or relative to getwd()) is this:

PATH.TO.RADR.PACKAGES <- "../../packages"


library(bigframe, lib.loc=PATH.TO.RADR.PACKAGES)
library(biglist, lib.loc=PATH.TO.RADR.PACKAGES)

## The size of a serialized scan info entry, in bytes
SCAN.INFO.ENTRY.SIZE <- 1356

## helper shortcut

p <- function(...) cat(paste(..., "\n", sep="", collapse=", "))

fixraw <- function(f, prompt=TRUE, force=FALSE, samples.per.pulse=1024, pulses=4096, bps=12) {
  ## fix a biglist raw archive
  ##
  ## f:      filename of the raw archive, in the form "XXX.raw.biglist"
  ## prompt: show the user the proposed TOC before writing it, allowing cancellation
  ## force:  rebuild the index and TOC even if the former appears superficially correct
  ##
  ## Remaining parameters are used to estimate how many scans are contained in the biglist
  ## data file.  Their product (divided by 8) is all that matters:
  ##
  ##   samples.per.pulse: number of samples per pulse
  ##   pulses: number of pulses per scan
  ##   bps: bits per sample

  bfn <- paste(f, ".i", sep="")

  ## make a backup copy of the index file
  
  file.copy(bfn, bfns <- paste(bfn, ".saved", sep=""), overwrite = FALSE)

  ## read entire index file

  bf <- bigframe(bfn)
  ndx <- bf[]

  ## number of rows in the index
  n <- dim(ndx)[1]

  ## apparent number of scans in the biglist, based on data parameters:
  nscan <- floor(file.info(f)$size / (bps / 8 * samples.per.pulse * pulses + SCAN.INFO.ENTRY.SIZE))
    
  if (!any(is.na(ndx[,1]))
      && n == 1 + 2 * nscan
      && !force)
    {
      p("Index appears to be okay.  To force a rebuild of index and TOC, specify force=TRUE.")
      return()
    }
  
  ## the new index data frame
  ind <- data.frame(offs=rep(as.double(NA), n+1), size=rep(as.double(NA), n+1), class=rep("", n+1), len=rep(0, n+1), stringsAsFactors=FALSE)
  
  blf <- file(f, "rb")
  ## skip the biglist header string
  i <- 32

  ## read until there are no more objects in the file.
  ## Don't assume the bigframe told the truth about this.

  p("Scanning the biglist data file - this will take some time.")
  
  ## Leave the first slot in ind empty, for a future TOC
  j <- 2

  ## Catch possible errors in R_unserialize resulting from the last
  ## biglist object not having been completely written to disk.  The
  ## (partial) object will be discarded.
  
  try ({
    repeat {
      ## unserialize one object at a time
      seek(blf, i)
      x <- .Call("R_unserialize", blf, NULL)
      new.i <- seek(blf, NA)
      ind[j,] <- list(i, new.i - i, class(x), length(x))
      i <- new.i
      j <- j + 1
    }
  }, silent=TRUE)

  j <- j - 1
  ind <- ind[1:j, ]
  
  close(blf)

  p("Original bigframe rows:            ", n)
  p("Filesize-based biglist item count: ", 2 * nscan + 1)
  p("Actual # biglist items:            ", j - 1)

  if (j == 1) {
    p("File is empty so there's nothing I can do - sorry!\n")
    close(bf)
    return()
  }
  
  ## A rawarch biglist data file consists of a sequence of serialized
  ## R objects.  This sequence is not generally in the same order as
  ## the objects appear when accessed by indexing the biglist object;
  ## e.g. the TOC is at biglist index [[1]], but is the last actual
  ## object in the biglist data file.
  
  ## Because the rawarch plugin writes a TOC entry after each run, and
  ## because the biglist plugin currently does not recycle space
  ## within files, the sequence of objects can be represented by this
  ## regular expression, even if the writing was terminated
  ## ungracefully:

  ## RAWARCH :=    ((SCAN_INFO SCAN_DATA)+ TOC)+ TOC*                                 (a)
  ##            || ((SCAN_INFO SCAN_DATA)+ TOC)* (SCAN_INFO SCAN_DATA)+               (b)
  ##            || ((SCAN_INFO SCAN_DATA)+ TOC)* (SCAN_INFO SCAN_DATA)* SCAN_INFO     (c)

  ## The three clauses correspond to
  ## a) complete and correct rawarch file, but allows for multiple TOCs at end of file
  ## b) table of contents was not rewritten after end of final run (any preceding TOC are present),
  ##    but last scan record has both scan info and scan data
  ## c) toc not rewritten after end of final run, and last scan record is missing its data

  ## SCAN_INFO: an R list object with length >10 with entries like samples.per.pulse, timestamp, etc.
  ## SCAN_DATA: an R "character" object with length==1 and nchar==bits.per.sample / 8 * pulses * samples.per.pulse
  ## TOC: an R list object with length 3 and entries "num.scans", "start.time" and "end.time"


  ## Which if any records look like a table of contents?
  
  itoc <- which(ind$len == 3 & ind$class == "list")

  ## remove any toc records from the index list; those small parts of the
  ## biglist data file will effectively be unused

  if (length(itoc) > 0)
    ind <- ind[-itoc,]

  ## if the last record is scan_info, we assume it corresponds to a record whose
  ## data are lost, so delete it.

  if (tail(ind$class, 1) == "list")
    ind <- head(ind, -1)
  
  ## Recreate the biglist index file; this will simply be the offsets and
  ## sizes of the objects found in the biglist data file, except for those
  ## objects which appear to be TOC records, and with an empty record
  ## for slot 1, to be filled in later.

  j <- dim(ind)[1]

  p("Reconstructed biglist index will have ", j, " slots.")
  if (prompt) {
    resp <- readline("Enter 'y' to write this biglist index, anything else to cancel: ")
    if (resp != "y") {
      close(bf)
      p("fixraw cancelled. No files were modified.")
      return()
    }
  }
    
  dim(bf) <- c(j, 2)
  bf[1:j,] <- ind[1:j, 1:2]
  close(bf)

  ## Get the lists of slots corresponding to first and last
  ## scan.info records for each run. First, remove trailing
  ## TOC entries, so that we're left with only interior TOC entries.

  browser() 
  while (length(itoc) > 0 && tail(itoc, 1) == j + length(itoc))
    itoc <- head(itoc, -1)

  ## Now get the first and last scan.info slot numbers, remembering
  ## to adjust the toc locations to take into account their removal.
  
  first <- c(2, itoc - cumsum(seq(along=itoc) - 1))
  last <- c(itoc - cumsum(seq(along=itoc) - 1) - 2, j - 1)
  
  ## reopen the biglist and construct the TOC
  
  bl <- biglist(f)

  ## grab the scaninfo records for the first and last scan in each run
  
  sif <- bl[first]
  sil <- bl[last]

  toc <- list(num.scans  = floor((last - first) / 2) + 1,
              start.time = sapply(sif, function(x) x$timestamp),
              end.time   = sapply(sil, function(x) x$timestamp))

  toc.fmt <- toc
  class(toc.fmt$start.time) <- class(toc.fmt$end.time) <- "POSIXct"

  p("The new TOC will be: ")
  print(toc.fmt)

  if (prompt) {
    resp <- readline("Enter 'y' to write this TOC, anything else to cancel: ")
    if (resp != "y") {
      close(bl)
      p("fixraw cancelled.  biglist now has a correct index file, but there is no rawarch TOC")
      return()
    }
  }
  bl[[1]] <- toc
  close(bl)
  p("rawarch fixed.")
}


