## svn $Id: gifmovie.conf.R 285 2009-03-20 18:33:43Z john $
##                                                         
## unix-specific configuration for gifmovie plugin

## what programs are used to capture window contents into a file.
## By default, the programs distributed with radR are used.
## No file extension is required.  The programs and any options
## must be in the same order as frame.file.formats (see below).

capture.progs = c("/usr/local/bin/gifsicle", "/usr/local/bin/win2rgb")

## Command line for a viewer program for movies or frames
## Nice how it's the same program...

viewer = "gifview -a"

## Here's an alternative - use Firefox:

## viewer = "firefox"

## a function to view a movie given by name

view.movie = function() {
  system(paste(viewer, paste('"', movie.filename, '"', sep="")), wait=FALSE)
}
