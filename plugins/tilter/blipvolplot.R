##svn $Id: blipvolplot.R 477 2010-03-11 17:31:54Z john $
##
## Quick and dirty volume plot of blip data.  To run, first view at
## least one scan of data in radR (to establish plot dimensions), then
## do this from the console:

##   source("plugins/tilter/blipvolplot.R")

library(rgl)

## some points to set the bounding box big enough to enclose all data

mx<-RSS$scan.info$samples.per.pulse * rss.rps()
points3d(c(0, mx, -mx), c(0, mx, mx), c(0, mx, -mx), color="black", alpha=0)
rgl.bbox()
rgl.bg(color="black")

rss.add.hook("DONE_SCAN", 
             function(...) {
               if (RSS$play.state >= RSS$PS$PLAYING) {
                 b <- RSS$blips
                 if (RSS$have.valid$patches && length(b) > 0) {
                   ## dither the angle phi to avoid artefacts stemming from assuming
                   ## all targets are on the beam axis
                   
                   r <- RSS$patches$range[b]
                   x <- RSS$patches$x[b]
                   y <- RSS$patches$y[b]
                   theta <- atan2(y, x)
                   phi <- (RSS$scan.info$antenna.angle[1] + rnorm(length(b), 0, 1.5)) * (pi / 180)
                   rad <- sqrt(RSS$patches$area[b])
                   col <- rss.rgbint.to.tclcolour(RSS$palette.mat[1 + (RSS$patches$max[b]/4096 * 256),3])
                   ## swap y and z for a more natural plot orientation
                   
                   id <- rgl.spheres(r * cos(phi) * cos(theta), r * sin(phi), r * cos(phi) * sin(theta), rad, col)
                 } else {
                   ## create a bogus invisible sphere to keep the rgl stack synchronized with the
                   ## scan count (i.e. there's something added every scan)
                   
                   id <- rgl.spheres(0, 0, 0, 0, alpha=0)
                 }
                 ## remove old blips from 3d plot; roughly those generated at the same point in
                 ## the previous volume scan
                 ## protect against errors from trying to pop non-existent elements
                 
                 try(rgl.pop(id=id - 2 * (sum(TILTER$pattern$scans) - 2)), silent=TRUE)
               }
             }
             )
