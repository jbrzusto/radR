##     VIDEO   PLUGIN   CONFIG
##                                                        
##  Read frames from video files.
##                                                        
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because     
##  it is overwritten with new values when you exit       
##  radR.                                                 


## is this plugin enabled?
enabled = TRUE

## this plugin is not always on
always.on = FALSE

## for guessing timestamps from the base filename, we use a strptime-compatible
## format string:
date.guess.format = "%Y-%m-%d %H-%M-%S"

## in order to recognize the presence of a date in the base filename,
## we use a regular expression compatible with date.guess.format
date.guess.regexp = "((19)|20)[0-9][0-9]-[0-9]?[0-9]-[0-9]?[0-9] [0-9]?[0-9]-[0-9]?[0-9]-[0-9]?[0-9]"

## regardless of the video's frame rate, at what rate do we grab frames for processing in radR?
## this is in frames per second

default.frame.rate = 10

## regardless of the video's actual resolution, we can specify the width
## and height in video pixels for the frames we analyze, and ffmpeg
## will take care of rescaling for us

default.width = 320
default.height = 200

## default offset of image centre from the plot window centre, in pixels
## a value of c(A, B) here has the effect of shifting display of the video
## right A pixels and up B pixels (video pixels, not plot window pixels)

default.origin = c(0, 0)

## default scale of the video; i.e. how many metres represented by a pixel width
## we assume square pixels

default.scale = 5

## default rotation of the coordinate system, in degrees clockwise.
## Normally, y increases bottom to top, and x increases left to right, but
## any rotation can be specified here.

default.rotation = 0

## list of video extensions (and descriptions) that we're interested
## in and able to read (via ffmpeg). The first one listed will be the
## default choice in "open file" dialgoues, and the rest will be in
## the drop-down menu in the order given here.  An all files ("*.*") choice
## is automatically added to the end of the list, and this will allow
## the user to open any file.

video.extensions = list(
  wmv = 'Windows Movie',
  asf = 'Flash',
  mpg = 'MPEG',
  avi = 'AVI video'
  )

## the plot window date format:
plot.title.date.format           = "%Y %b %d %H:%M:%OS3"
