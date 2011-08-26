## svn $Id: tilter.conf.R 771 2011-04-06 05:38:39Z john $
##
##    Configuration for the tilter plugin

## is the plugin enabled upon loading?  Commands are
## only issued to the tilter when the plugin is enabled.

enabled = TRUE

## volume scan angle range

angle.range = c(5, 85) 

## stepsize for angle range, in degrees

angle.step = 5 

## time to spend at each angle, in seconds; doesn't include transition time

time.per.step = 2.4

## modes for running a pattern

volume.modes = list (
  manual = "manual: no automatic tilting",
  stop = "once: run pattern a single time",
  wrap = "wrap: go back to starting angle at end of pattern and repeat",
  bounce = "bounce: go forward then backward through pattern, and repeat",
  slow.bounce = "bounce slowly: same as bounce, but double time at endpoints"
  )

## current volume mode (a name of an item in the volume.modes list)
volume.mode = "slow.bounce" 

## should volume scanning be restarted (rather than continued) after
## a stop?

reset.volume.after.stop = FALSE 

## what is the safe angle operating range
## We will never ask the tilter to move outside this range,
## except when calibrating to zero.

safe.operating.range = c(1, 89) 

## local port number on which tiltminder server should listen

server.port = 12345

## name of file to log tilter usage (for analyzing wear etc.)

log.filename = "plugins/tilter/tilter1.1.log.txt" 

## should tilter server traffic be output to the R(term) console window?

server.debug = TRUE

## How should the angle range be flattened into a smaller number of slots
## so that we don't need too many stats matrices?
##
## This is a function that takes a double vector (representing the possibly
## fractional mean tilt angle for the scan), and returns a character vector
## giving the name of the slot for the learning variable at each angle.

## This version just rounds to the nearest integer angle:

flatten.angle = function(a) as.character(round(a))

## This version divides angles by 3 to reduce the amount of memory taken
## by stats matrices when there is a large angular range:

## flatten.angle = function(a) as.character(round(a / 3))

## Should the tilter pattern start/stop automatically with play start/stop?

start.stop.pattern.with.play = TRUE
