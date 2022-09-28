##  svn $Id: radR.R 334 2009-04-21 17:55:18Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006, 2007 John Brzustowski        
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##########################################################################
##
## radR.R - loader for the radR main program and required libraries
##          this file should be kept short and simple, as its purpose is
##          to raise startup speed by lowering code parsing time.
##

options(warn = 1) ## emit warnings as we go.

## Load pre-parsed R function definitions.  First check whether
## any of the source files has changed since the last save of pre-parsed
## code.  We use different files 

radR.source.files <- c("main/radRmain.R", "main/radRutil.R", "gui/gui.R", "gui/guiutil.R", "gui/guicanvas.R")
radR.binfile <- "radR.Rdata"
radR.package.dir <- "packages"

if (! file.exists(radR.binfile) ||
    any(file.info(radR.source.files)$mtime > file.info(radR.binfile)$mtime)) {
  for (f in radR.source.files)
    source(f)
  ## don't save values of these variables, or they will be clobbered
  ## upon loading radR.binfile next time, causing problems.
  rm(f, radR.source.files)
  save.image(radR.binfile)
} else {
  ## load a cached binary version of the source files.
  load(radR.binfile)
}

## Parse any command line options here.

## Did the user specify --no-gui?

use.gui <- is.na(match("--no-gui", commandArgs()))
event.loop <- is.na(match("--no-eventloop", commandArgs()))

## Quietly load the strictenv strict environment class.  Don't warn
## about the masking of "base::%in%", since our replacement dispatches
## to it.
library("strictenv", lib.loc = radR.package.dir, warn.conflicts=FALSE)
.S3method("[[<-", "strictenv", "[[<-.strictenv")
.S3method("[<-", "strictenv", "[<-.strictenv")
.S3method("[", "strictenv", "[.strictenv")
.S3method("[[", "strictenv", "[[.strictenv")

## load the extmat package.
library("extmat",    lib.loc = radR.package.dir, warn.conflicts=FALSE)

## load the bigframe package
library("bigframe",  lib.loc = radR.package.dir, warn.conflicts=FALSE)

## load the biglist package
library("biglist",   lib.loc = radR.package.dir, warn.conflicts=FALSE)

## read the tcltk required for the GUI, if needed
if (use.gui) {
  library("tcltk")
} else {
  rss.tcl.stubs()
}

library(lubridate)
## run the main radR loop, which also initializes the GUI, if any.

rss.init(use.gui, event.loop)
