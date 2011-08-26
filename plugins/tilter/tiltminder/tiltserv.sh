#!/bin/bash
#
# svn: $Id: tiltserv.sh 785 2011-05-05 17:26:03Z john $
#
# Configure the controlling serial port and
# start the tilter server with realtime priority.
# 
# parameters:
#
#  $1  tilter control device
#  $2  port on which server should listen 
#  $3  file for logging tilter activity
#  $4  -d means log server traffic to stdout
#  $5  -g means server listens on all interfaces for commands
#
# example:
#
# ./tiltserv.sh /dev/ttyUSB0 12345 tilterlog.txt
#

if [ "$2" == "" ]; then
    echo
    echo Usage: tiltserv.sh DEVICE PORT [LOGFILE [-d] [-g]]
    echo
    exit 1
fi

if ! stty -F "$1" 4800 -parenb cs8 -cstopb -crtscts; then exit 1; fi

./plugins/tilter/tiltserv1.1s "$1" "$2" "$3" "$4" "$5" &

PID="$!"

sleep 0.2

# test whether server is alive

if ! test -a /proc/$PID; then

    echo
    echo Error:  the server did not start or died unexpectedly.
    echo
    exit 2
fi

sudo renice -19 -p $PID > /dev/null
sudo ionice -c1 -p $PID > /dev/null

echo 
echo You can now control the tilter by doing
echo "'telnet localhost $2'"
echo
