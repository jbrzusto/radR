## svn $Id: blip3d.R 213 2009-02-12 16:15:11Z john $
##
## A script to display the blip you point at in 3d.
## Requires: the rgl R package.
##
## Load this into radR by doing:
##
##   > source("gui/blip3d.R")
##
## from the console window.
##
## Functions accessible from the plot window
##
## Control-A: append a new 3d plot of the blip under the cursor.  Existing
##            3d blip plots are preserved.
##
## Control-B: make a 3d plot of the blip under the cursor. Replaces any existing
##            3d blip plot.
##
## To delete the 3d blip plot(s), just close the RGL window.

library("rgl")

gui.current.patch <- function() {
  xy <- rss.patch.at.sample.pulse(GUI$tx.plot.to.matrix(GUI$last.pointer.coords))
  cbind(xy, RSS$scan.mat[xy])
} 
                                                                  
blip.3d <- function(coords=NULL, sp=NULL, append=FALSE) {
  ## generate a 3d plot of the blip at the specified coordinates
  ## in the current scan.
  ## coords is a vector with two elements: c(sample#, pulse#)

  if (is.null(sp))
    sp<-rss.patch.at.sample.pulse(coords)
  if (is.null(sp))
    ## silently ignore attempt to plot non-existent blip
    return(NULL)
  ## generate xy grid points for this blip
  x<-1:(max(sp[,1])-min(sp[,1]) + 1)
  y<-1:(max(sp[,2])-min(sp[,2]) + 1)
  ## create the grid of heights and get them from the raw data matrix
  z<-matrix(nrow=length(x),ncol=length(y))
  z[1+cbind(sp[,1]-min(sp[,1]),sp[,2]-min(sp[,2]))]<-10*log(RSS$scan.mat[sp])
  ## set missing values to the minimum for this blip
  z[is.na(z)]<-min(z,na.rm=TRUE)
  
  if (!append) {
    ## clear the 3d plot, so only this blip will appear
    rgl.clear()
    GUI %$!% blip3d.dx <- 0
  } else {
    ## alternatively, shift this blip over by GUI$blip3d.dx
    if (! ("blip3d.dx" %in% GUI))
      ## create GUI$blip3d.dx since it doesn't exist
      GUI %$!% blip3d.dx <- 0
    ## increase blip3d.dx for the next blip; 2 is the spacing between blips
    GUI$blip3d.dx <- GUI$blip3d.dx + length(x) + 2
    x <- x + GUI$blip3d.dx
  }
  ## plot the blip
  rgl.surface(x,y,z)
}

this.blip.3d <- function(append=FALSE) {
  ## plot the blip at the most recent pointer coordinates
  blip.3d(GUI$tx.plot.to.matrix(GUI$last.pointer.coords), append=append)
}

## Control-b in the plot window will make a 3d plot of the blip
## under the cursor.

tcl("bind", ".plot", "<Control-b>", function() this.blip.3d(FALSE))

## Control-a in the plot window will add a 3d plot of the blip
## under the cursor to the current 3d plot.

tcl("bind", ".plot", "<Control-a>", function() this.blip.3d(TRUE))
