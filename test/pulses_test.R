## svn: $Id$
##
## show the rotation of the radar in 3d, using output from pulse_test.c
##

library(rgl)
library(tcltk)

## 3d plotting functions that swap y and z coords because I always forget
## to do this when writing the code

lines3D <- function(x, y, z, ...) lines3d(x, z, -y, ...)
texts3D <- function(x, y, z, ...) texts3d(x, z, -y, ...)
spheres3D <- function(x, y, z, ...) spheres3d(x, z, -y, ...)
segments3D <- function(x, y, z, ...) segments3d(x, z, -y, ...)

dat<-read.table(pipe("./pulses_test"), header=T)

## convert to cartesian coordinates (azimuths are degrees clockwise from N)

x   <- cos(rad(dat$elev))*cos(rad(90 - dat$azi))
y   <- cos(rad(dat$elev))*sin(rad(90 - dat$azi))
z   <- sin(rad(dat$elev))
wgx <- cos(rad(dat$wg_elev))*cos(rad(90 - dat$wg_azi))
wgy <- cos(rad(dat$wg_elev))*sin(rad(90 - dat$wg_azi))
wgz <- sin(rad(dat$wg_elev))

## plot the beam axis graph

rgl.open()
lines3D(x, y, z, col="blue")
lines3D(rbind(x, 0), rbind(y, 0), rbind(z, 0), col="red")
lines3D(c(-1, 1), c(0, 0), c(0, 0), col="green", alpha=0.5)
lines3D(c(0, 0), c(-1, 1), c(0, 0), col="green", alpha=0.5)
lines3D(c(0, 0), c(0, 0), c(-1, 1), col="green", alpha=0.5)
texts3D(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1), c("0", "East", "North", "Up"), col="green")
spheres3D(x[1:4], y[1:4], z[1:4], c(0.03, 0.02, 0.01, 0.005), "green", alpha=0.5)

## animate rotation of the radar; blue is beam axis, red is waveguide long axis

doit <- function() {
  rgl.open()
  id <- NULL
  first <- TRUE
  lines3D(c(-1, 1), c(0, 0), c(0, 0), col="green", alpha=0.5)
  lines3D(c(0, 0), c(-1, 1), c(0, 0), col="green", alpha=0.5)
  lines3D(c(0, 0), c(0, 0), c(-1, 1), col="green", alpha=0.5)
  repeat {
    rgl.pop(id=id)
    id <- NULL
    texts3D(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1), c("0", "East", "North", "Up"), col="green")
    for (i in seq(along=x)) {
      new.id <- segments3D(c(0, x[i], wgx[i] * c(-0.2, .2)), c(0, y[i], wgy[i] * c(-0.2, .2)), c(0, z[i], wgz[i] * c(-0.2, .2)), col=c("blue", "blue", "red", "red"), alpha=1.0)
      rgl.pop(id=id)
      id <- new.id
      if (first)
        rgl.points(x[i], y[i], z[i], 0.1, col="yellow")
      tcl("update")  ## allows user to manipulate the rgl window
    }
    first <- FALSE
  }
}

cat("\nYou can restart the animated radar plot by typing 'doit()'\n")

doit()
