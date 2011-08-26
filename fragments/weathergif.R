## weathergif.R - simple background-removal/compression scheme for Environment Canada GIFs
##
## (C) 2007 by John Brzustowski
## Licence: GPL
##
## Usage:
##
##   R --vanilla --slave --args path1 path2 ... < weathergif.R 
##
## path1, path2, ... are names of GIF files containing images from the EC weather radar, or names
##                   of directories which are searched for such files
##                   If a path argument begins with '@', it names a file containing a list of GIF filenames,
##                   one per line.
##
## For each file specified (e.g. X.gif), this creates a file called X.wrd (for Weather Radar Data)
## consisting of a compressed version of the non-background pixels in X.gif
##
## Requires:  - the tcltk package from R
##            - the extmat package from radR
##            - the radR.so and unixgui.so libraries from radR
##            - the shared library weathergif.so whose source is weathergif.c
##
## Assumed Environment Canada GIF format:
##
## - weather radar data are represented by coloured pixels in a gif file
## - radar data is found only in the 478 by 478 pixel square beginning 1 pixel down and right from
##   the top-left corner of the GIF
## - data pixels appear in the following non-background colours:
## - the entire GIF is opaque (so the resulting Tk photo image has 0xff in its alpha channel)
##
## You can get the raw image (without overlays) from:
##
## http://www.weatheroffice.gc.ca/data/radar/temp_image/XFT/XFT_PRECIP_RAIN_%Y_%m_%d_%H_%M.GIF
##
## where XFT is one of the weather radars, and the "%" macros are as for strftime.
##
## These images are:
##   Format: GIF (CompuServe graphics interchange format)
##   Geometry: 580x480
##   Class: PseudoClass
##   Type: Palette
##   Endianess: Undefined
##   Colorspace: RGB
##   Rendering-intent: Undefined
##   Resolution: 72x72
##   Units: Undefined
##   Filesize: 16kb
##   Interlace: None
##   Background Color: black
##   Border Color: #DFDFDF
##   Matte Color: grey74
##   Page geometry: 580x480+0+0
##   Dispose: Undefined
##   Compression: LZW
##   Orientation: Undefined
##   Tainted: False
##
## The colour binning is as follows (this is for rain; FIXME: get winter values)
##
## RRGGBB	  mm/hr	   dBZ
##                >=        >=
## 99ccff	  0.1	    7
## 0099ff	  1	    23
## 00ff66	  ?	    ?
## 00cc00	  4	    33
## 009900	  ?	    ?
## 006600	  12	    40
## ffff33	  ?	    ?
## ffcc00	  24	    45
## ff9900	  ?	    ?
## ff6600	  50	    50
## ff0000	  ?	    ?
## ff0299	  100	    55
## 9933cc	  ?	    ?
## 660099	  200	    60		    
##
## !!! WARNING: this code requires that there be at most 14 non-background pixel values.  If there are more,
## the compression scheme in weathergif.c will have to be changed!!!

compress <- function(filename, x, new.file.ext = ".wrd", check=TRUE) {
  if (!file.exists(filename)) {
    print(paste(WG, filename, "does not exist\n"))
    return()
  }
  
  ## create an external matrix for holding RGB data
  x <- extmat(type="int")
  
  ## read the gif file, creating a Tk photo image
  ok <- TRUE
  tryCatch ({
    img <- tcl("image", "create", "photo", file=filename)
  }, error = function(e) {
   print(paste(WG, filename, "is not readable by Tk's GIF handler\n"))
   ok<<-FALSE
  })
  if (!ok) {
    rm(x)
    return()
  }
  
  ## grab the tcl interpreter, which we need below
  ti <-.Call("get_tcl_interp", as.integer(tcl("winfo", "id", ".")))  

  ## force the extmat to point to the image data
  .Call("radR_attach_image_to_extmat", as.character(img), ti, x)

  ## tcl stores pixel data in 0x00BBGGRR order, so we swap to get 0x00RRGGBB
  .Call("radR_image_swap_RB", x) 

  ## extract the data pixel rectangle, zeroing the alpha channel (high order byte, which must be 0xff)
  ## Note that image columns are matrix rows.
  data <- x[data.cols, data.rows] + as.integer((2^32) - 255 * 2^24)

  ## find the number of the data bin for each pixel; NA means a background pixel
  bin.num <- match(data, rgblist)

  ## get the linearized coordinates of the non-background pixels
  coords <- which(!is.na(bin.num))

  ## get the bin numbers of the non-background pixels
  vals <- bin.num[!is.na(bin.num)]

  ## encode the data into a raw vector
  z <- .Call("encode", dim(data), vals, coords)

  if (check) {
    ## check that compression is reversible and correct
    zz <- .Call("decode", z)
    if (any (zz != 0 & zz != bin.num) || any(zz == 0 & !is.na(bin.num))) {
      print(paste(WG, "compression failed on image", filename, "\n"))
      rm(x)
      tcl("image", "delete", img)
      return()
    }
  }

  rm(x)
  tcl("image", "delete", img)
  tryCatch({
    new.filename <- sub(".gif", new.file.ext, filename, ignore.case=TRUE)
    new.file <- file(new.filename, "wb")
    writeBin(z, new.file)
    close(new.file)
  }, error = function(e) {
    print(paste(WG, "unable to write compressed data to file", new.filename, "\n"))
  })
  
}

## for acadia grid:
##    library(tcltk, lib.loc="/array/home/jbrzusto/my_R_libraries")

library(tcltk)
library(extmat, lib.loc=".")
dyn.load("radR.so")
dyn.load("unixgui.so")
dyn.load("weathergif.so")

WG <- "weathergif:"

data.rows <- 2:479  ## which GIF rows hold the data (columns numbered from 1)
data.cols <- 2:479  ## which GIF columns hold the data (columns numbered from 1)

## set up the map of RGB colour values to bin numbers
rgblist <- as.integer(c(0x99ccff, 0x0099ff, 0x00ff66, 0x00cc00, 0x009900, 0x006600, 0xffff33, 0xffcc00, 0xff9900, 0xff6600, 0xff0000, 0xff0299, 0x9933cc, 0x660099))

paths <- commandArgs()
paths <- paths[-(1:which(paths == "--args"))]

files <- c()

for (p in paths) {
  if (substring(p, 1, 1) == "@") {
    p <- substring(p, 2)
    if (!file.exists(p)) {
      warning(paste(WG, "listfile", p, "not found\n"))
    } else {
      listed.files <- readLines(p, warn=FALSE)
      files <- c(files, listed.files[file.exists(listed.files)])
    }
  } else {
    if (!file.exists(p)) {
      warning(paste(WG, "file/path", p, "not found\n"))
    } else {
      if (file.info(p)$isdir)
        files <- c(files, dir(p, pattern="gif$", full=TRUE, recursive=TRUE))
      else
        files <- c(files, p)
    }
  }
}

## filter out obvious problem files
files <- files[file.info(files)$size > 0]

for (f in files)
  compress(f)
