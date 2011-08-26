## svn $Id: gifmovie.conf.R 285 2009-03-20 18:33:43Z john $
##                                                         
## unix-specific configuration for gifmovie plugin

## Command line for a viewer program for movies or frames
## Nice how it's the same program...

viewer = "gifview -a"

## Here's an alternative - use Firefox:

## viewer = "firefox"

## a function to view a movie given by name

view.movie = function() {
  system(paste(viewer, paste('"', movie.filename, '"', sep="")), wait=FALSE)
}
