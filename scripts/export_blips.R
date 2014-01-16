## svn: $Id: export_blips.R 422 2010-01-19 18:35:41Z john $

## install hooks to allow export of blip data
##
## Summary: when the user hovers over a blip and hits a key,
## the summary data for that blip (x, y, z, timestamp, etc.)
## along with some or all of its raw data is appended to a
## data.frame.  The column corresponding to raw data and coordinates
## are lists.
## 
## When the desired blips have been accumulated, the user can hit
## another key to export the data.frame as a file.

## GUI CONFIGURATION

## keystroke that adds blip under pointer to current list of retained blips

add.blip.key    <- "a"

## keystroke that saves current list of retained blips to a file, then resets
## user is prompted for a file

save.blips.key  <- "s"

## default filename for blip export

default.blips.filename <- "./blips.rds"

## Convert raw seascan sample value to received power at input
## waveguide, in dbm.  Assumes seascan gain was 255.  Default
## seascan offset is 61.

sample.to.dbm <- function(sample, offset=61) {
  -4.834e+01 + 1.738e-02 * sample - 8.020e-01 * offset
}


reset.retained.blips <- function() {
  n.blips <<- 0
  blips <<- data.frame(x = numeric(0),
                       y = numeric(0),
                       z = numeric(0),
                       t = numeric(0),
                       ns = integer(0),
                       area = numeric(0),
                       int = numeric(0),
                       max = numeric(0),
                       aspan = integer(0),
                       rspan = integer(0),
                       perim = numeric(0),
                       range = numeric(0)
                       )
  blips$samp.r <<- list()
  blips$samp.theta <<- list()
  blips$samp.phi <<- list()
  blips$samp.dbm <<- list()
  blips$samp.iangle <<- list()
  blips$samp.irange <<- list()
}
  
c("x", "y", "z", "t", "ns", "area", "int", "max", "aspan", "rspan",  "perim", "range")
## retain data for the blip at c(sample, pulse)

retain.blip.at.sample.pulse <- function(sp) {
  patch <- rss.patch.at.sample.pulse(sp)
  if (is.null(patch)) {
    rss.gui("SET_POINTER_INFO", "(not in patch)")
    return()
  }

  ## get patch summary stats
  row <- as.list(RSS$patches[attr(patch, "index")[1],])

  ## get patch sample locations in spherical coordinates
  sph <- rss.sp.to.sph(patch)

  ## get sample data in dbm (using the appropriate seascan "offset" parameter)
  ## You can remove the 2nd parameter in this call to use the default seascan offset of 61.
  
   dbm <- sample.to.dbm(RSS$scan.mat[patch], RSS$scan.info$antenna.plugins$seascan$offset)

  n.blips <<- n.blips + 1
  blips[n.blips,] <<- c(row, list(samp.r = list(sph[,"r"]), samp.theta = list(sph[,"theta"]), samp.phi = list(sph[,"phi"]), dbm = list(dbm), iangle=list(patch[,2]), irange=list(patch[,1])))

  ## tell user point was saved
  rss.gui("SET_POINTER_INFO", sprintf("\nRetained blip # %d\n", n.blips))
  
}

retain.blip.at.pointer <- function() {
  retain.blip.at.sample.pulse(GUI$tx.plot.to.matrix(GUI$last.pointer.coords))
}

save.retained.blips <- function() {
  n <- n.blips
  file <- rss.gui("FILE_DIALOG",
                  mode = "save",
                  title = sprintf("Choose file to hold %d blips", n),
                  types = list(".rds" = "R Data Set", ".*" = "All files"),
                  init.file = default.blips.filename)
  if (length(file)) {
    saveRDS(blips, file)
    reset.retained.blips()
    rss.gui("SET_POINTER_INFO", sprintf("\nSaved %d blips to file %s\n", n, file))
    default.blips.filename <<- file
  }
}

########################################################################
#
# processing
#
######################################################################

reset.retained.blips()
tcl("bind", ".plot", add.blip.key, retain.blip.at.pointer)
tcl("bind", ".plot", save.blips.key, save.retained.blips)

