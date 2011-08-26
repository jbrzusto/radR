if exist "c:\Program Files (x86)" (set PROGFILES="c:\Program Files (x86)") else (set PROGFILES="c:\Program Files")
set RHOME=%PROGFILES%\R\R-2.5.1
call radRcommon.bat
set PATH=%PATH%;c:\cygwin\usr\bin;c:\cygwin\bin
gdb %RHOME%\bin\Rterm.exe

