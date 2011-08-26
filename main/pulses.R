##  svn $Id: pulses.R 574 2010-05-11 02:07:15Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2010 John Brzustowski
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.

## functions for pulses

## maintain a table of pulse metadata in RSS$pulses
## The table has these columns:
## ts      - timestamp of pulse, in seconds past the epoch (with fractional part)
## azi     - azimuth of beam axis, in degrees clockwise from north
## elev    - elevation of beam axis, in degrees above horizontal
## wg_azi  - azimuth of waveguide long axis , in degrees clockwise from north
## wg_elev - elevation of waveguide long axis, in degrees above horizontal

rss.build.pulse.table <- function () {
  ## ensure the pulses dataframe has the right size

  if (is.null(np <- RSS$scan.info$pulses))
    return()
  if (is.null(dim(RSS$pulses)) || dim(RSS$pulses)[1] != np) {
    RSS$pulses <- data.frame(ts      = numeric(np),
                             azi     = numeric(np),
                             elev    = numeric(np),
                             wg_azi  = numeric(np),
                             wg_elev = numeric(np))
  }    
                             
  .Call("radR_get_pulse_metadata",
        np,
        RSS$scan.info$timestamp,
        RSS$scan.info$duration,
        RSS$scan.info$orientation,
        RSS$scan.info$bearing.offset,
        RSS$scan.info$rotation.axis,
        RSS$scan.info$antenna.angle,
        RSS$pulses,
        NA_real_)
}

