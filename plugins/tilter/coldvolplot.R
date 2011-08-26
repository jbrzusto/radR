##svn $Id: coldvolplot.R 477 2010-03-11 17:31:54Z john $
##
## Quick and dirty volume plot of cold sample data.  We make no attempt to
## correct for spatially varying sampling probability. To run, first view at
## least one scan of data in radR (to establish plot dimensions), then
## do this from the console:

##   source("plugins/tilter/coldvolplot.R")

library(rgl)
volplot.gamma <- 3  ## adjust this to change how "solid" the plot appears
volplot.n <- 1000 ## adjust this to change how many raw samples are taken per scan

## some points to set the bounding box big enough to enclose all data

mx<-RSS$scan.info$samples.per.pulse * rss.rps()
points3d(c(0, mx, -mx), c(0, mx, mx), c(0, mx, -mx), color="black", alpha=0)
rgl.bbox()

rss.add.hook("DONE_SCAN", 
             function(...) {
               if (RSS$play.state >= RSS$PS$PLAYING) {
                 ## pick points at random from the current scan
                 
                 p <- floor(runif(volplot.n, 1, dim(RSS$scan.mat)[2]-1))
                 s <- floor(runif(volplot.n, 1, dim(RSS$scan.mat)[1]-1))
                 r <- s * rss.rps()
                 theta <- (2*pi/dim(RSS$scan.mat)[2])*p

                 ## dither the elevation angle around the axis, using a normal distribution
                 ## with mean = 1/2 beamwidth
                 phi <- (RSS$scan.info$antenna.angle[1] + rnorm(length(p),0,1.5))* (pi/180)
                 x <- r * cos(phi) * cos(theta)
                 y <- r * cos(phi) * sin(theta)
                 z <- r * sin(phi)
                 id <- rgl.points(x, z, y, col="blue", alpha=(RSS$scan.mat[s,p]/4095)^(1/volplot.gamma))

                 ## delete old points, ignoring errors about non-existent ones
                 try(rgl.pop(id=id-34),silent=TRUE)
               }
             }
             )
