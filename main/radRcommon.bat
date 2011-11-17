set TZ="GMT00"
if exist "c:\Program Files (x86)" (
   set PROGFILES="c:\Program Files (x86)"
) else (
   set PROGFILES="c:\Program Files"
)
set RHOME=%PROGFILES:"=%\R\R-%RVERSION%
set PATH=%PATH%;%RHOME%\bin;%RHOME%\Tcl\bin;.;.\libs;.\main
