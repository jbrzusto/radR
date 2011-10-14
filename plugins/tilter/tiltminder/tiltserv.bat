@rem svn $Id: tiltserv.bat 785 2011-05-05 17:26:03Z john $

@echo off

rem Configure the controlling serial port and
rem start the tilter server with realtime priority.
rem
rem parameters: 
rem            %1  the tilter serial port device; e.g. com9
rem            %2  the port # on which to listen
rem            %3  the name of the log file 
rem            %4  -d if specified, server traffic is logged to stdout
rem            %5  -g if specified, server listens for commands on all interfaces
rem
rem example:
rem            .\tiltserv com9 12345 log.txt

if /i "%2"=="" goto usage

mode %1 baud=4800 parity=n data=8 xon=off odsr=off octs=off dtr=off rts=off

rem Start the tilt server with a minimized window that can be
rem closed to kill the server.

start "radR tilter server" /min /realtime .\plugins\tilter\tiltserv1.1s.exe %1 %2 %3 %4 %5

rem -- OR --
rem 
rem Start the tilt server without a window.  The server can be killed
rem from Windows Task Manager.
rem
rem start /b /realtime .\plugins\tilter\tiltserv1.1s.exe %1 %2 %3 %4 %5

echo.
echo You can now control the tilter server by doing
echo 'telnet localhost %2'
echo.

exit /b 0

:usage

echo.
echo Usage: tiltserv.bat DEVICE PORT [LOGFILE [-d] [-g]]
echo   where DEVICE is the COM port to which the tilter is attached,
echo   PORT is the IP port to which the server should listen (e.g. 12345),
echo   -d means log traffic to the console, 
echo   and -g means listen for tilter commands on all interfaces, not just
echo   the local host.  DON'T USE -g WITHOUT A NETWORK FIREWALL!
echo.

exit /b 1

