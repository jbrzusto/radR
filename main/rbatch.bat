@echo off
rem #
rem #  radR : an R-based platform for acquisition and analysis of radar data
rem #  Copyright (C) 2006-2011 John Brzustowski        
rem #
rem #  This program is free software; you can redistribute it and/or modify
rem #  it under the terms of the GNU General Public License as published by
rem #  the Free Software Foundation; either version 2 of the License, or
rem #  (at your option) any later version.
rem #
rem #  This program is distributed in the hope that it will be useful,
rem #  but WITHOUT ANY WARRANTY; without even the implied warranty of
rem #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem #  GNU General Public License for more details.
rem #
rem #  You should have received a copy of the GNU General Public License
rem #  along with this program.  If not, see <http://www.gnu.org/licenses/>.
rem #
rem ###############################################################################
rem 
rem  rbatch.bat: run process_one.R on a set of files; 
rem         e.g. process a bunch of blip movies to extract tracks
rem 
rem  usage:  rbatch [--parm PARMFILE] FILE1 ... FILEN
rem 
rem  e.g.    rbatch --parm d:\parms\myparms.R d:\data\radar\CS*.bma
rem 
rem  Parameter overrides are taken from PARMFILE, if the --parm option is specified.
rem  
rem  NOTE: this file must be executed in a Windows CMD shell window from the top
rem        level radR directory; you might need to do "cd c:\path\to\radR" first.

rem __RVERSION__ in following line is replaced by e.g. 2.5.1 at build time
set RVERSION=__RVERSION__
set PROGRESS=--show-progress

if /i (%1) EQU (--help) goto showhelp
if /i (%1) EQU (-h) goto showhelp
if /i (%1) EQU (/h) goto showhelp
if /i (%1) EQU () goto showhelp
goto nohelp

:showhelp

echo rbatch runs processone.R on a set of files; 
echo        e.g. process a bunch of blip movies to extract tracks
echo usage:  rbatch [--no-progress] [--parm PARMFILE] FILE1 ... FILEN
echo e.g.:   rbatch /array/data1/jbrzusto/data/CS*.bma
echo Parameter overrides are taken from PARMFILE, if the --parms option is specified.
echo Specifying --no-progress prevents a running display of scans processed.

goto alldone

:nohelp

if /i (%1) NEQ (--no-progress) goto noprogressopt
  shift
  set PROGRESS=""

:noprogressopt

if /i (%1) EQU (--parm) goto parms
if /i (%1) EQU (--parms) goto parms
goto noparms

:parms
  shift
  if exist %1 goto validparmfile
  echo The parm file '%1' does not exist.
  pause
  exit /b 1

:validparmfile
  set RADR_PARMS=--parm %1
  shift
  goto done

:noparms
  set RADR_PARMS=""

:done
  call radRcommon.bat

  echo ===== radR batch processor =====
:loop
  if /i (%1) EQU () goto alldone
  for %%f in (%1) do R --ess --no-save --quiet --args --no-gui %PROGRESS% --source main/process_one.R %RADR_PARMS% %%f
  shift
goto :loop

:alldone
rem pause
