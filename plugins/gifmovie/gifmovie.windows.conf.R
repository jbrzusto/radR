## svn $Id: gifmovie.conf.R 285 2009-03-20 18:33:43Z john $
##                                                         
## Windows-specific configuration for gifmovie plugin

## what programs are used to capture window contents into a file.
## By default, the programs distributed with radR are used.
## No file extension is required.  The programs and any options
## must be in the same order as frame.file.formats (see below).

capture.progs = c("plugins/gifmovie/gifsicle", "plugins/gifmovie/win2rgb")

## Command line for a viewer program for movies or frames
## This one is for Windows XP's 'picture and fax viewer'

viewer = "rundll32 c:/windows/system32/shimgvw.dll,ImageView_Fullscreen"

## Here's an alternative - use Firefox:

## viewer = "c:/Program Files/Mozilla Firefox/firefox.exe"

## If you figure out how to do it in Vista, let me know...

## a function to view a movie given by name

view.movie = function() {
  shell(gsub("/", "\\", paste(viewer, get.movie.filename()), fixed=TRUE), wait=FALSE)
}
