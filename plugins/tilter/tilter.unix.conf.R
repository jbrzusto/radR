## svn: $Id: tilter.unix.conf.R 477 2010-03-11 17:31:54Z john $
##
##    Unix-dependent configuration for the tilter plugin.

## function to get a character vector of names of available serial ports

get.ttys = function() {
  system("ls -1 /dev/ttyUSB*", intern=TRUE)
}


## default serial port where TTL5USB9M adapter is found
## if this port is found not to be valid, then the first one
## found by get.ttys will be used instead.

tty = "/dev/ttyUSB0"

## system command to start the tilt minder server

## Note: user must be in sudoers group if the tilter server is to be
## given realtime priority, which is recommended for timing accuracy.

start.server.cmd = "plugins/tilter/tiltserv.sh"
