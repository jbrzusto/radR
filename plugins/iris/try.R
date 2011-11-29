## ## load the C-level SWIG interface
dyn.load("iris")

## ## if necessary, source() the R-level SWIG interface

if (!file.exists("iris.Rdata") || diff(file.info(c("iris.R", "iris.Rdata"))$mtime) < 0) {
  source("iris.R")
  save.image(file="iris.Rdata")
} else {
  load("iris.Rdata")
}

dyn.load("iris_radR")
library(rgl)

source("swig.R")


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
}

plotraw <- function(z, i0 = 1, di = 1) {
  ## plot the raw scan data in z, which is either a filename
  ## or a raw vector
  if (is.character(z))
    z <- readBin(z, "raw", file.info(z)$size)
  dat<-.Call("get_raw_sweeps", z)
  if (dev.cur() != 1)
    dev.off()
  x11()
   for(i in seq(along = dat)) {
     plot.sweep(dat, c(i, 1), i0, di)
     par(new = TRUE)
  }
  par(new = FALSE)
}

plotraw.3d <- function(z, i0 = 1, di = 1) {
  ## plot the raw scan data in z, which is either a filename
  ## or a raw vector
  if (is.character(z))
    z <- readBin(z, "raw", file.info(z)$size)
  dat<-.Call("get_raw_sweeps", z)
  if (rgl.cur() != 0)
    rgl.close()
  rgl.open()
  for(i in seq(along = dat)) {
    plot.sweep.3d(dat, c(i, 1), i0, di)
  }
}



#plotraw("/usr/data/radar/weather/Iris/WVY/expanded/CONVOL:20030714124023")
#plotraw("/usr/data/radar/weather/Iris/WVY/expanded/DOPVOL1_A:20030714123135")
#plotraw("/usr/data/radar/weather/Iris/WVY/expanded/DOPVOL1_B:20030714123251")
#plotraw("/usr/data/radar/weather/Iris/WVY/expanded/DOPVOL1_C:20030714123400")
#plotraw("/usr/data/radar/weather/Iris/WVY/expanded/DOPVOL2:20030714123438")


RECSIZE <- 6144
RECNO <- 1
f<-"d:/home/toshi/data/weather/iris/XFT/200802050610..CONVOL.URP.XFT.RADAR.IRIS"
#f <- "/usr/data/radar/weather/Iris/WVY/expanded/DOPVOL1_A:20030714123135"
#f <- "/usr/data/radar/weather/Iris/WVY/expanded/CONVOL:20030714124023"
z <- readBin(f, "raw", file.info(f)$size)
NREC <- length(z) / RECSIZE

## single letter comands for navigation
makeActiveBinding("N", function() {x <<- x + c(0, RECSIZE); RECNO <<- RECNO + 1}, .GlobalEnv)

SKIP <- sizeof("raw_prod_bhdr")
HDRSKIP <- SKIP + sizeof("ingest_data_header")

x <- cast(z, "product_hdr")
PHD <- x
N
IHD <- cast(x, "ingest_header")
N
BHDR <- cast(x, "raw_prod_bhdr")
x <- x + c(0, sizeof(BHDR))
IDH <- cast(x, "ingest_data_header")

dat<-.Call("get_raw_sweeps", z)


tar.read.dir <- function(f, compressed=substr(f, nchar(f)-1, nchar(f)) == "gz", tz="") {
  ## read the directory of a tar file
  ##
  ## f: full filename; if it ends in "gz", compressed is set to TRUE
  ## compressed: if TRUE, the archive is compressed using the "z" option to tar
  ## tz: timezone of timestamps for this archive
  ## 
  ## returns a data frame with columns mode, owner (user/group), size (bytes), date (POSIXct), name (full path to file in archive)

  p <- pipe(sprintf("tar -t%svf%s", if (compressed) "z" else "", f))
  x <- read.table(p, as.is=TRUE, col.names=c("mode", "owner", "size", "date", "time", "name"))
  x$date <- as.POSIXct(paste(x$date, x$time), tz=tz)
  x$time <- NULL
  return(x)
}

tar <- function(f, compressed=substr(f, nchar(f)-1, nchar(f)) == "gz", tz="", binary=TRUE, get.dir=TRUE) {
  ## create a connection to the tar archive

  if (get.dir)
    dir <- tar.read.dir(f, compressed)
  else
    dir <- NULL

  props <- new.env(parent=emptyenv())
  props$binary = binary
  props$index = 0
  props$dir = dir
  structure (pipe(sprintf("tar -Ox%sf%s", if (compressed) "z" else "", f), open=sprintf("r%s", if (binary) "b" else "")),
             props = props,
             class=c("tar", "pipe", "connection"))
}

tar.next.fileinfo <- function(f) {
  props <- attr(f, "props")
  dir <- props$dir
  if (is.null(dir))
    stop('To use tar.next.filename, you must create the connection with "get.dir=TRUE", which is the default')

  if (props$index >= dim(dir)[1])
    return(NULL)
  return (dir[props$index + 1,])
}
  
tar.next.file <- function(f, binary) {
  ## read the next file in the tar archive in one gulp
  ##
  ## f: connection opened by the tar() function
  ## binary: should the file be treated as text or binary?
  ##
  ## Return value depends on binary (which defaults to the value used when opening the tar connection):
  ##
  ##   FALSE => returns the file's lines as a character vector, as if readLines(F) had been done on a file F
  ##
  ##   TRUE  => returns the file as a raw vector, as if readBin(F, "raw", n=file.info(F)$size) had been done on a file F
  ##

  props <- attr(f, "props")
  if (missing(binary))
    binary <- props$binary
  dir <- props$dir
  if (is.null(dir))
    stop('To use tar.next.file, you must create the connection with "get.dir=TRUE", which is the default')

  if (props$index >= dim(dir)[1])
    return(if (binary) raw(0) else character(0))
  i <- props$index <- 1 + props$index
  if (binary)
    return (readBin(f, "raw", n=dir$size[i]))
  else
    ## split at any of the three EOL conventions
    return (strsplit(rawToChar(readBin(f, "raw", n=dir$size[i])), "(\n|\r\n|\r)", perl=TRUE)[[1]])
}

tar.skip.file <- function(f, num=1) {
  ## skip over the next num files in the tar archive
  props <- attr(f, "props")
  dir <- props$dir
  if (is.null(dir))
    stop('To use tar.skip.file, you must create the connection with "get.dir=TRUE", which is the default')
  i <- props$index
  n <- dim(dir)[1]
  while (num > 0 && i < n) {
    i <- i + 1
    seek(f, dir$size[i], "current")
  }
  props$index <- i
}

  

#f <- "/usr/data/radar/weather/Iris/WVY/wvy_20031114123139_20031115123109.tar.gz"
#t <- tar(f)
#plotraw(tar.next.file(t))

# bad files:

#8   46  /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz CONVOL:20030721201042
#12  35  /usr/data/radar/weather/Iris/WVY/wvy_20030724123439_20030725123023.tar.gz CONVOL:20030724182054
#17  5   /usr/data/radar/weather/Iris/WVY/wvy_20030729123027_20030730122130.tar.gz CONVOL:20030729131028
#18  137 /usr/data/radar/weather/Iris/WVY/wvy_20031106123049_20031107122128.tar.gz CONVOL:20031107111058
#19  85  /usr/data/radar/weather/Iris/WVY/wvy_20031107123101_20031108122127.tar.gz CONVOL:20031108023133
#20  37  /usr/data/radar/weather/Iris/WVY/wvy_20031108123050_20031109122126.tar.gz CONVOL:20031108183056
#23  35  /usr/data/radar/weather/Iris/WVY/wvy_20031109165433_20031110122405.tar.gz CONVOL:20031109224050
#23  111 /usr/data/radar/weather/Iris/WVY/wvy_20031109165433_20031110122405.tar.gz CONVOL:20031110112119
#25  10  /usr/data/radar/weather/Iris/WVY/wvy_20031111123436_20031112122130.tar.gz CONVOL:20031111141057
#25  127 /usr/data/radar/weather/Iris/WVY/wvy_20031111123436_20031112122130.tar.gz CONVOL:20031112094052
#26  112 /usr/data/radar/weather/Iris/WVY/wvy_20031112123438_20031113122234.tar.gz CONVOL:20031113071200
#26  130 /usr/data/radar/weather/Iris/WVY/wvy_20031112123438_20031113122234.tar.gz CONVOL:20031113101217
#26  136 /usr/data/radar/weather/Iris/WVY/wvy_20031112123438_20031113122234.tar.gz CONVOL:20031113111216
#27  5   /usr/data/radar/weather/Iris/WVY/wvy_20031113123202_20031114122310.tar.gz CONVOL:20031113131152
#27  14  /usr/data/radar/weather/Iris/WVY/wvy_20031113123202_20031114122310.tar.gz CONVOL:20031113144140
#28  12  /usr/data/radar/weather/Iris/WVY/wvy_20031114123139_20031115123109.tar.gz CONVOL:20031114142133    
#8   191 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_A:20030721200150
#8   192 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_A:20030721201144
#27  156 /usr/data/radar/weather/Iris/WVY/wvy_20031113123202_20031114122310.tar.gz DOPVOL1_A:20031113142158
#8   337 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_B:20030721200302
#8   338 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_B:20030721201255
#9   127 /usr/data/radar/weather/Iris/WVY/wvy_20030722125100_20030722232034.tar.gz DOPVOL1_B:20030722132322
#8   480 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_C:20030721200414
#8   481 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL1_C:20030721201412
#20  461 /usr/data/radar/weather/Iris/WVY/wvy_20031108123050_20031109122126.tar.gz DOPVOL1_C:20031108172359
#23  452 /usr/data/radar/weather/Iris/WVY/wvy_20031109165433_20031110122405.tar.gz DOPVOL1_C:20031110092417
#8   621 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL2:20030721200439
#8   622 /usr/data/radar/weather/Iris/WVY/wvy_20030721123443_20030722124201.tar.gz DOPVOL2:20030721201440

# this is 28 out of 17672 files. = 0.158 %


find.buggy.files <- function() {
  
##   bad.list <- list(c(8, 46), c(8, 191), c(8, 192), c(8, 337), c(8, 338), c(8, 480), c(8, 481), c(8, 621), c(8, 622),
##                    c(9, 127), c(12, 35), c(17, 5), c(18,137), c(19, 85), c(20, 37), c(20,461), c(23, 35), c(23,111),
##                    c(23, 452), c(25, 10), c(25, 127), c(26, 112), c(26, 130), c(26, 136), c(27, 5), c(27, 14), c(27, 156),
##                    c(28, 12))

##   errs <- c("bps=28736", "bps=21248", "bps=25840", "bps=16", "bps=22732", "segfault", "bps=12424", "segfault", "bps=21236",
##             "segfault", "bps=24163", "bps=13110", "segfault", "segfault", "segfault", "segfault", "segfault",
##             "segfault", "segfault", "segfault", "segfault", "segfault", "segfault", "segfault", "segfault", "segfault",
##             "segfault")

  bad.list <- list(c(0, 0))
  last.bad <- tail(bad.list, 1)[[1]]
  tarfiles <- dir("/usr/data/radar/weather/Iris/WVY", full.names=TRUE, pattern=".*gz")

  for (j in seq(along=tarfiles)) {
    if (j < last.bad[1])
      next
    tf <- tarfiles[j]
    t <- tar(tf)
    i <- 1
    repeat {
      f <- tar.next.fileinfo(t)
      ## no fileinfo means end of archive
      if (is.null(f))
        break
      print(paste(j, i, tf, f$name))
      dat <- tar.next.file(t)

      ## files with mode "-rw-------" are truncated, apparently; we drop them, as we do with zero-length files
      
      if (f$mode == "-rw-------" || length(dat) == 0 || (j == last.bad[1] && i <= last.bad[2])) {
        ## do nothing
      } else {
        z <- .Call("get_raw_sweeps", dat)
      }
      i <- i + 1
    }
    j <- j + 1
  }

}

tarfiles <- dir("/usr/data/radar/weather/Iris/WVY", full.names=TRUE, pattern=".*gz")
files <- dir("/usr/data/radar/weather/Iris/XFT/", full.names=TRUE, pattern=".*gz")
t <- tarfiles[1]

## try <- c(8, 482)
## t <- tar(tarfiles[try[1]])
## for (i in 1:try[2])
##   f <- tar.next.file(t)
## dat<-.Call("get_raw_sweeps", f)
## plot.sweep.3d(dat, 1)

## find.buggy.files()

gz.read <- function(fname) {
  ## only works for gz files less than 4gb in size!
  ## (since the size determination is limited by the file format)
  con <- file(fname, "rb")
  seek(con, -4, "end")
  size <- readBin(con, "int", n=1, size=4, endian="little")
  close(con)
  con <- gzfile(fname, "r")
  rv <- readBin(con, "raw", n=size)
  close(con)
  return(rv)
}

read <- function(fname) {
  readBin(fname, "raw", n=file.info(fname)$size)
}

tar.read.dir <- function(fname) {
  f <- gzfile(fname, "rb")
  dir <- NULL
  pos <- 0
  repeat {
    z <- readBin(f, "raw", n=512)
    if (length(z) < 512)
      break
    h <- .Call("get_tar_header", z)
    if (!is.null(h)) {
      ## header record is valid
      if (h[[9]]) {
        ## header record is for a regular file
        dir <- rbind(dir, h)

        ## skip the file contents     
        pos <- pos + 512 * ceiling(h[[5]]/512)
      }
    }
    ## skip header
    pos <- pos + 512
    seek(f, pos)
  }
  return(dir)
}


  g <- function() {
    z<-readBin(f, "raw", n=512)
    print(.Call("get_tar_header", z))
  }

  h <- function(x) {
    pos <<- pos + x
    seek (f, pos)
  }
