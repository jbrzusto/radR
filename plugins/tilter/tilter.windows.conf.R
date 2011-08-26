## svn: $Id: tilter.windows.conf.R 767 2011-04-06 04:50:06Z john $
##                                                         
##    Windows-dependent configuration for the tilter plugin.

## function to get a character vector of names of available serial ports

get.ttys = function () 
{
 x <- shell("mode", intern = TRUE)
 sapply(strsplit(x[grep("COM[0-9]+:", x)], " "), function(x) tail(x, 1))
} 

## default serial port where TTL5USB9M adapter is found
## if this port is found not to be valid, then the first one
## found by get.ttys will be used instead.

tty = "COM3:"

## Important note on COM ports.  Even though, in theory, we should be able
## to use names like "\\.\COM12" for com ports above COM9, this doesn't
## appear to work.  If your TTL5USB9M adapter is showing up as a com port
## above COM9, you can force Windows to assign it a lower com port number
## by going into device manager:
##
##    Start | Control Panel | System | Hardware | Device Manager
##
##
## and then look for a "TTL port" under the entry "Ports (COM & LPT)"
## Double click on it, and Properties window select the pane "Port Settings",
## click "Advanced", and select a new COM Port Number from the drop-down menu at top.
## Ignore any warning Windows gives you about this.


## system command to start the tilt minder server

start.server.cmd = "plugins/tilter/tiltserv.bat"
  



