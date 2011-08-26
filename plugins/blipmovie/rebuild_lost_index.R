## svn $Id: rebuild_lost_index.R 574 2010-05-11 02:07:15Z john $
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

## rebuild the missing ".bm.i" index file for a blipmovie.
##
## Note: so far, this only works with blipmovies that have a single run.
## Eventually, this should be made to work with all blipmovies, and built
## into the plugin.
##
## To use this script, save in the radR/plugins/blipmovie folder and do this
## in the console:
##
##   source("plugins/blipmovie/rebuild_lost_index.R")
##   rebuild.index("/path/to/my/blipmovie.bm")
##
## A progress message will appear in the text console ("Rterm" window in Windows,
## shell window in linux), unless you call it like this:
##
##   rebuild.index("/path/to/my/blipmovie.bm", quiet=TRUE)
##
## By default, an existing index will not be overwritten.  You can either delete
## the index (.bm.i) file yourself, or call the function like this:
##
##   rebuild.index("/path/to/my/blipmovie.bm", force=TRUE)
##
##
##
## Algorithm:
## - create a new ".bm.i" bigframe
## - open the ".bm" file, f,  in binary read mode ("rb")
## - seek past the header to the start of data (position 32)
## - deserialize first object, which should be a list with these items:
##    - data.items: a list with a named item for every plugin that created data;
##        so far, only the blipmovie plugin does this, and its item
##        is this character vector: "blip.runbuf"  "blip.samples" "blip.scores"
##
##    - n.slot: the total number of biglist slots per scan, including the slot
##      for scan.info; so far, this is always 4
##      
## while not at end of file
##   - deserialize x from f
##   - if class(x) == "list", then this is a scan.info record
##   - if class(x) != "list", then this is data for the scan, and there are two more
##     consecutive slots in the list which should be deserialized.
## end

## Notes:
## Each time we deserialize an item, record the starting offset and size in the bigframe.
## Note that the last item should be the table of contents, which is really the object
## for slot 1 in the biglist.  So other items start at slot 2.
##
## For scans with no blips, no data slots in the biglist are actually written out, so
## the corresponding bigframe entries must be left at c(NA, NA).  The easiest way to
## do this is to keep track of whether the previously deserialized object was a list.
## If so, then if the current object is a list, bump up the slot by 4, otherwise, 
## bump it up by 1.
##
## Each time we read a scan.info list, we need to determine the timestamp by examining
## the list items to see which metadata are present, and pulling out the appropriate one.
## Pull this code from the blipmovie plugin.

## TODO: change the biglist format so that it records the list index just before serializing
## the object to the list.  That way, we'll be able to rebuild the index for an arbitrary
## biglist.

## Code to create bigframe:
## ndx <- bigframe(paste(filename, biglist.indexfile.suffix, sep=""), data.frame(offs=double(0), size=double(0)), header=header)

require(biglist)

rebuild.index <- function(f, quiet=FALSE, force=FALSE) {
  ndxf <- paste(f, biglist.indexfile.suffix, sep="")
  if (file.exists(ndxf) && !force)
    stop("The file '", ndxf, "' already exists.")
  ndx <- bigframe(ndxf, data.frame(offs=double(0), size=double(0)), header=list())
  fsize <- file.info(f)$size
  bl <- file(f, open="rb")
  get1 <- function() {
    ## get one item from the biglist
    tryCatch(unserialize(bl), error=function(e) done <<- TRUE)
  }
  recpos <- function() {
    ## record position and size of most recently read biglist item
    ppos <<- pos
    pos <<- seek(bl)
    ndx[i,] <<- data.frame(offs=ppos, size=pos - ppos)
    i <<- 1 + i
    if(!quiet)
      cat(sprintf("%8.0f    %5.1f\r", (i-2)/4, pos / fsize * 100))
  }                           
  seek(bl, pos <- 1 + nchar(biglist.header.string))
  ## starting index
  i <- 2
  done <- FALSE
  last.was.list <- FALSE
  o <- get1()
  if (done) {
    warning("The file '", f, "' is either empty or not a valid blipmovie .bm file")
  } else {
    if (!identical(o, list(data.items=list(blipmovie=list(names=c("blip.runbuf", "blip.samples", "blip.scores"), offset=1)), n.slot=4))) {
      close(ndx)
      close(bl)
      file.remove(ndxf)
      stop("The file '", f, "' is not a valid blipmovie .bm file")
    }
    if(!quiet)
      cat(sprintf("Rebuilding index for blipmovie '%s'\nProgress:\n  scan    %% of file\n", f))
    recpos()
    repeat {
      obj <- tryCatch(unserialize(bl), error=function(e) done <<- TRUE)
      if (done) {
        warning("Blipmovie '", f, "' appears to be missing its table of contents'")
        break
      }
      if (class(obj) == "data.frame") {
        ## this is the contents item
        ## which should be written to the first slot
        i <- 1
        recpos()
        break
      }
      if (class(obj) == "list") {
        if (last.was.list)
          i <- i+3  ## the preceding scan data items were empty, so skip 3 slots
        last.was.list <- TRUE
      } else {
        last.was.list <- FALSE
      }
      recpos()
    }
  }
  if (seek(bl) < fsize)
    warning("The blipmovie '", f, "' appears to have more than one run of data,\nbut the reconstructed index contains only the first run.\nPlease contact the author of this script to get support for multi-run blipmovies added!")
  close(ndx)
  close(bl)
  if(!quiet)
    cat("\n")
 }
