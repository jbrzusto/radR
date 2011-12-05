##
## iris_radR.R:  read data from NexRad ("iris") files in formats used by Env. Canada
##               weather radar.
##
## The main functions of interest:
##
##  read.iris(name) - reads the file "name" (compressed or not) and pulls out sweeps
##
## This returns a list of sweeps present in the file.  For each sweep, you get a list
## of matrices (one per product), and the following attributes:
##		  "az_lo"	: angles of ray start azimuth (angles in radians: 0... 2 * pi)
##		  "az_hi"	: angles of ray end azimuth
##		  "el_lo"	: angles of ray start elevation
##		  "el_hi"	: angles of ray end elevation
##		  "ts"		: timestamps of rays
##		  "bin_range"	: range covered by a bin, in metres

## Environment Canada Weather Radar Files:
## =======================================

## There are three types of raw product files produced:

## CONVOL:  reflectivity volume scan
##          PRF: 1200  Plen: 2us
##          One product: 
##          1: DB_DBT: Total power: dbZ = (x-64)/2, 0 means NA; 255 = not scanned
##      Bin range: 999.6093 497.7778   Number of bins: 48...256

## DOPVOL1_A/B/C (alternating PRFs between 890 and 1100 Hz; Plen 800ns; A,B,C are different elevation angles)
##      Bin range: 497.7778   Number of bins: 225

## DOPVOL2 (constant PRF of 1200 Hz; Plen ?)
##      Bin range: 997.7876   Number of bins: 226

## All DOPVOL files have four interleaved products each:

##      1: DB_DBT: Total power;  dbZ = (x-64)/2; 0 = no data; 255 = not scanned
##      2: DB_DBZ: Clutter-corrected Reflectivity; dbZ = (x-64)/2; 0 = no data; 255 = not scanned
##      3: DB_VEL: Velocity; vel (m/s) = (x-128) * Nyquist / 127; 0 = unavailable; Nyquist = wavelength * PRF / 4 * K,
##                where K is 3 for the 4:3 double PRF mode, and 1 for the constant PRF mode.
##      4: DB_WIDTH: Width; spectrum width  = x/256 * unambiguous velocity; 0=no data; 255=not scanned

## The wavelength for these radars is 5.32cm

## Further assumptions:

## - each product in a sweep has the same number of rays, and they are stored interleaved
## - data for each bin in a ray is 8 bits wide
## - all bins in all rays for all product in a sweep have the same bin_range (it
##   can vary from sweep to sweep)
## - within a product and within a sweep, the number of bins in a ray must be constant.
##   However, different products within a sweep may have different numbers of bins,
##   and the same product may have a different number of bins in different sweeps.

##   FIXME: in fact, isn't bin_range constant within an entire raw product file,
##   since we're calculating it from a subfield of the product header??

dyn.load("iris_radR")

## the following library is optional, but the .3d functions depend on it.

library(rgl)

deg <- function(x) x * (180/pi)
rad <- function(x) x * (pi/180)

gz.read <- function(fname) {
  ## read a .gz compressed file into a raw vector
  ## only works for gz files less than 4gb in size!
  ## (since the size determination is limited by the file format)
  con <- file(fname, "rb")
  seek(con, -4, "end")
  size <- readBin(con, "int", n=1, size=4, endian="little")
  close(con)
  con <- gzfile(fname, "rb")
  rv <- readBin(con, "raw", n=size)
  close(con)
  return(rv)
}

read.file.as.raw <- function(name) {
  ## read a file as raw bytes, using decompression if its name ends in ".gz"
  ## if passed a raw vector, just return it
  if (is.raw(name))
    return(name)
  if (length(grep("\\.gz$", name)))
    return(gz.read(name))
  else
    return (readBin(name, "raw", n=file.info(name)$size))
}

read.iris <- function(name) {
  ## read the data in NexRad iris file (Env. Canada formats only)
  ## name can be a filename or a raw vector 
  if (is.raw(name) || is.character(name))
    return(.Call("get_raw_sweeps", read.file.as.raw(name)))
  else
    stop("read.iris needs either a filename or a raw vector")
}

plot.sweep <- function(dat, i, i0=1, di=1, lim = c(-230,230), sym=".") {
  ## i is a two element index: the first chooses the sweep, the second chooses the product
  pal <- rainbow(256)
  d <- dat[[i[1]]][[i[2]]]
  which <- seq(from=i0, by=di, to=dim(d)[2])
  sf <- cos(attr(dat[[i[1]]], "el_lo")[which])
  th <- attr(dat[[i[1]]], "az_lo")[which]
  x <- (1:(dim(d)[1])) %o% (sin(th) * sf)
  y <- (1:(dim(d)[1])) %o% (cos(th) * sf)
  plot(x, y, col=pal[1+c(d[, which])], pch=sym, xlim=lim, ylim=lim)
}

plot.sweep.3d <- function(dat, i, i0=1, di=1, va=5, lim=c(-230,230)) {
  ## i is a two element index: the first chooses the sweep, the second chooses the product
  pal <- rainbow(256)
  d <- dat[[i[1]]][[i[2]]]
  which <- seq(from=i0, by=di, to=dim(d)[2])
  sf <- cos(attr(dat[[i[1]]], "el_lo")[which])
  th <- attr(dat[[i[1]]], "az_lo")[which]
  x <- (1:(dim(d)[1])) %o% (sin(th) * sf)
  y <- (1:(dim(d)[1])) %o% (cos(th) * sf)
  z <- (1:(dim(d)[1])) %o% sin(attr(dat[[i[[1]]]], "el_lo")[which])
  use <- x >= lim[1] & x <= lim[2] & y >= lim[1] & y <= lim[2]
  rgl.points(c(x)[use], c(z)[use], c(y)[use], col=pal[1+c(d[, which])][use])
  if (nrow(rgl.ids()) > 1)
    rgl.pop(id=rgl.ids()[1,1])
}

plotraw <- function(f, i0 = 1, di = 1) {
  ## plot the raw scan data in f, which is either a filename
  ## or a raw vector
  dat <- read.iris(f)
  for(i in seq(along = dat)) {
    plot.sweep(dat, c(i, 1), i0, di)
    par(new = TRUE)
  }
  par(new = FALSE)
}

plotraw.3d <- function(f, i0 = 1, di = 1) {
  ## plot the raw scan data in f, which is either a filename
  ## or a raw vector
  dat <- read.iris(f)
  for(i in seq(along = dat)) {
    plot.sweep.3d(dat, c(i, 1), i0, di)
  }
}


## Examples:

ex <- function(path.to.data="d:/home/data/radar/weather/WKR", type="dopvol1.a") {
  ## type can be dopvol1.a, dopvol1.b, dopvol1.c, dopvol2, or convol
  
  ## get the list of DOPVOL1.A files 
  ff <- dir(path.to.data, pattern=type, full.names=TRUE, ignore.case=TRUE)

  ## read the first data file
  dat <- read.iris(ff[1])

  ## summarize the structure
  str(dat)

  ## plot it
  plotraw(ff[1])

  ## animate a sequence of plots in the plot window
  ## open a new plot window
  windows()

  ## plot all files
  for (f in ff) plotraw(f)

  ## do a plot in 3d
  plotraw.3d(ff[1])

  ## animate a sequence of plots in a 3d window
  ## open a new plot window
  rgl.open()

  ## plot all files
  for (f in ff) plotraw.3d(f)
}
