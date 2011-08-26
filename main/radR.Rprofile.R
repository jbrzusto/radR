## This file is used to start radR.  It will be copied to '.Rprofile' in the
## top-level radR run or install directory.

## Thus, to run radR, you can do:

##   cd radR
##   R

## or, to run without a GUI:

##   cd radR
##   R --args --no-gui

## Add / re-arrange packages to load before radR as required.
## Do not load GUI-specific packages (such as tcltk) here.

library(base)
library(datasets)
library(utils)
library(grDevices)
library(graphics)
library(stats)
library(methods)

source("main/radR.R")
