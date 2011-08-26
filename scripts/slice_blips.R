## svn: $Id: slice_blips.R 478 2010-03-11 17:34:08Z john $

## load a data.frame of saved blips, and extract the brightest angular
## cross-section; i.e. the slice at constant range in which the brightest
## sample occurs.
##

## name of a file created by the export_blips script

blip.filename <- "blips.RData"

###       UTILITY FUNCTIONS

## convert between linear and decibel units
db   <- function(x) 10*log10(x)
undb <- function(x) 10^(x/10)

## convert between radians and degrees

rad <- function(x) x * (pi/180)
deg <- function(x) x * (180/pi)

## standard error of mean, from samples
se <- function(x) sd(x) / sqrt(length(x))

## great-circle angle between two unit vectors in spherical coordinates
## input and output angles are in degrees
## (via the dot product)

sph.angle <- function(phi1, th1, phi2, th2) {   
  phi1 <- rad(phi1)
  th1 <- rad(th1)
  phi2 <- rad(phi2)
  th2 <- rad(th2)
  return (deg(acos(cos(phi1) * cos(phi2) * cos(th1-th2) + sin(phi1) * sin(phi2)))) 
}

###       PROCESSING

## load the blips

load(blip.filename)

## the blips are automatically loaded into the variable "blips", which is the
## name of the data.frame that was saved to the file

print(dim(blips))

## find the index of the brightest sample in each blip

i.max <- sapply(blips$samp.dbm, which.max)

## find those samples in each blip which are at the same range as the max
## sample (i.e. the angular cross section through the max sample of each blip)

xc <- mapply(function(x, y) which(x == x[y]), blips$samp.r, i.max)

## use only the sample dbm, r, theta, phi from the cross section for each blip

blips$samp.dbm   <- mapply("[", blips$samp.dbm,   xc)
blips$samp.r     <- mapply("[", blips$samp.r,     xc)
blips$samp.theta <- mapply("[", blips$samp.theta, xc)
blips$samp.phi   <- mapply("[", blips$samp.phi,   xc)

## samples per blip in cross-section

blips$cns <- sapply(blips$samp.r, length)

## re-find the index of the brightest sample, this time relative only to
## those samples retained as the cross-section

i.max <- sapply(blips$samp.dbm, which.max)

## find the true angular separation between the target and beam angle, for each sample
## We use sign() to make it go from negative to positive as the beam sweeps past the target.
blips$sep.ang <- mapply(function(phi, theta, imax) sph.angle(phi, theta, phi[imax], theta[imax]) * sign(theta-theta[imax]),
                        blips$samp.phi,
                        blips$samp.theta,
                        i.max)

## convenience function: append all coordinates together with an NA separating each
## original set; handy for plotting

clump <- function(x) unlist(lapply(x, function(y) c(y, NA)))

## plot dbm vs. angular separation for all blips
## use the clump function to insert NA's which will separate curves for each blip
## and replicate colours an appropriate number of times

plot(clump(blips$sep.ang),
     clump(blips$samp.dbm),
     col=rep(1:dim(blips)[1], times=blips$cns+1),
     type="b",
     xlab="true angle from blip centre (degrees)",
     ylab="sample brightness (dbm)",
     main="Blip angular cross sections through sample with max brightness"
     )
