if exist "c:\Program Files (x86)" (set PROGFILES="c:\Program Files (x86)") else (set PROGFILES="c:\Program Files")
set RHOME=%PROGFILES%\R\R-__RVERSION__
call radRcommon.bat
R --ess
