##  svn $Id: batchparm.R 682 2010-11-22 16:31:45Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2009 John Brzustowski
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
##
################################################################################

## batchparm.R:  parameter file for batch runs

## Any parameter values in this file override those in the
## corresponding .conf.R files.  The ones here are a subset that makes
## sense in processing blipmovies for tracks, but you can add any that
## you like from the .conf.R files (but see comments below re:
## antenna).  You should copy this file and supply a modified version
## as a command line parameter to the "rbatch" command.

## This file is in "R" syntax.  Each item definition except for the last in a list
## must be followed by a comma.

## There are six sections:  blip filtering, tracking generic, tracking model, antenna,
## declutter, zonefile.
## Each section and each parameter is optional; those not specified here are
## taken from the corresponding .conf.R files in your radR folders.
## Exception: the zonefile just specifies the name of a file with zone information.

list (

################################################################################
###
### blip finding (from main/radR.conf.R)
###
################################################################################

      find = list (

        ## a cutoff below which all data are treated as noise (forces all sample
        ## values less than this value to be zero).  No effect if noise.cutoff==0

        noise.cutoff = 0,

        ## Are we finding blips?  Counter-intuitively, this should be TRUE for both
        ## blipmovies and raw archives.

        blip.finding = TRUE,

        ## How many scans are used to learn the "background" distribution of echoes

        default.scans.to.learn = 15,

        ## What is the weight of past mean, deviation when updating with new estimates?
        ## 0 means use only the new estimate; 1 means ignore the new estimate and
        ## preserve the old estimate; intermediate values mean k * old estimate + (1-k) * new estimate
        ##

        stats.k = 0.95,

        ## Should background echo distributions be updated on each scan?

        update.stats.while.blipping = TRUE,

        ## What is the size of background cells?  In (samples, pulses)

        cell.dims = c(4, 4),

        ## These parameters set the high and low score thresholds.  If a
        ## sample has a score higher than the first value, or lower than the
        ## second value, it is considered "hot".  For the default radar
        ## scenario, we set the low threshold at its maximum negative value,
        ## which means we're effectively not using it, so that only higher
        ## than normal reflectivity is treated as "hot".  For grayscale video,
        ## we're interested in things either brighter or darker than the
        ## background, so we'd use something like -2.5 for the low threshold.

        blip.score.threshold = c(2.5, -128),

        ## should blip samples be excluded from updating their stats cell?
        ## This should be TRUE to avoid biasing estimates of background
        ## echoes in the presence of slow-moving targets.

        blip.exclude.blips.from.stats.update = TRUE

        ),


################################################################################
###
### blip filtering (from main/radR.conf.R)
###
################################################################################

      blip = list (

        ## Are we filtering blips in scans? (i.e. do the following minmax ranges apply?)

        blip.filtering = FALSE,

        ## To be called a "blip", a contiguous patch of hot samples must
        ## have an apparent area between the following limits (inclusive)
        ## (in square metres)

        blip.area.minmax = c(300, 20000),

        ## and a number of hot samples within the following inclusive limits

        blip.samples.minmax = c(30, 5000),

        ## the angular range (number of pulses) must be within the following inclusive limits

        blip.angular.minmax = c(2, -1),

        ## the radial range (number of sample slots) must be within the following inclusive limits

        blip.radial.minmax = c(1, -1),

        ## should blip centroids be calculated using area weighting?
        ## the alternative is intensity weighting

        blip.area.weighting = FALSE,

        ## should a blip filtering expression be used?

        use.blip.filter.expr = FALSE,

        ## what is the blip filtering expression?

        blip.filter.expr = expression(perim^2 / (area * (4*pi)) < 5)

        ),

################################################################################
###
### generic track building (from plugins/tracker/tracker.conf.R)
###
################################################################################

      ## tracker = list (
      ##   ## are tracks written to the .CSV file?

      ##   save.to.csv = TRUE,

      ##   ## the date/time format to use for the CSV track output
      ##   ## file.  This must include the date and time formats
      ##   ## separated by a comma, in order to be consistent
      ##   ## with what this plugin outputs for each blip.

      ##   date.time.format = "%Y/%m/%d,%H:%M:%S",

      ##   ## Maximum allowed speed of an object (km/h).
      ##   ## Blips whose positions in time and space imply a speed
      ##   ## between them larger than this are not joined into
      ##   ## the same track.

      ##   track.max.speed = 100,

      ##   ## minimum number of blips in a track for it to be saved

      ##   track.min.blips = 4

      ##   ),

################################################################################
###
###  multiframe correspondence track-building model config
###  (from plugins/tracker/multiframecorr.conf.R)
###
################################################################################

      ## mfc = list (

      ##   ## number of frames over which to "backtrack" in building
      ##   ## tracks

      ##   k = 2,

      ##   ## relative weighting of directional coherence in gain
      ##   ## function (the other component of gain, which receives
      ##   ## 1 - alpha as weight, is relative proximity to predicted
      ##   ## location)

      ##   alpha = 0.5,

      ##   ## a small penalty added to the gain function for any
      ##   ## edge connecting blips in non-consecutive scans; this
      ##   ## is meant to prevent exclusion of blips from perfectly
      ##   ## straight-line tracks

      ##   eps = 0.001,

      ##   ## the minimum gain (between 0 and 1) required for a blip
      ##   ## to be considered for joining a track

      ##   min.gain = 0.9090909

      ##   ),

################################################################################
###
###  antenna setup (from antennas/default.antenna.R)
###  Note: any parameters you specify in this section will overwrite those
###  in the source data.  But then why else would you specify it here?
###
################################################################################

      ## antenna = list (

      ##   ## Antenna type.
      ##   ## This must be one of:
      ##   ##   "dish" - a parabolic dish
      ##   ##   "tbar" - an open-array T-bar antenna

      ##   antenna.type = "dish",

      ##   ## Angle of the antenna axis above the horizontal, in degrees.
      ##   ## (should be 0 if type == "tbar")

      ##   antenna.angle = 40,

      ##   ## Horizontal aperture of the antenna, in degrees.  This is the
      ##   ## angular width of the instantaneous region "seen" by the
      ##   ## antenna.

      ##   antenna.aperture.h = 3,

      ##   ## Vertical aperture of the antenna, in degrees.  This is the
      ##   ## angular height of the instantaneous region "seen" by the
      ##   ## antenna.  (should equal aperture.h if type == "dish")

      ##   antenna.aperture.v = 3,

      ##   ## Geographic location of the radar.  This will be overridden by
      ##   ## any GPS information provided by the radar data, and is
      ##   ## optional.

      ##   ## latitude, in degrees North (negative for South of the equator)

      ##   latitude = 43.5,

      ##   ## longitude, in degrees West (negative for east of Greenwich)

      ##   longitude = -65.7,

      ##   ## elevation, in metres above sea level

      ##   elevation = 50,

      ##   ## bearing offset: an angle in degrees clockwise from N.
      ##   ## This is used for two purposes:
      ##   ## 1. For a stationary antenna, (i.e. one for which scan.info$bearing is always 0),
      ##   ##    this is the azimuth angle corresponding to the first pulse in each sweep.
      ##   ##
      ##   ## 2. For a moving antenna, this is the offset between the ship's axis
      ##   ##    (whose azimuth is reported in scan.info$heading) and the antenna's axis.
      ##   ##
      ##   ## In both cases, scan.info$heading + scan.info$bearing.offset will be
      ##   ## the compass angle corresponding to the first pulse in a scan.

      ##   bearing.offset = 0

      ##   ),

################################################################################
###
###  declutter config (from plugins/declutter/declutter.conf.R)
###
################################################################################
      ## declutter = list (

      ##   ## minimum mean sample occupancy at which blip is treated as clutter.
      ##   ## For example, blip.cutoff = 0.01 means any blip having a mean sample
      ##   ## occupancy (across all its samples) greater than 1% is filtered out.

      ##   cutoff = 0.0030,

      ##   ## the default file from which to load a clutter map
      ##   ## If this is NULL, the declutter plugin is disabled.
      ##   ## If this is not NULL, it should be a pathname in quotes
      ##   ## e.g. clutter.filename = "/radR/cluttermaps/my_site.clutter.Rdata"
      ##   ## and the declutter plugin will be enabled. (This won't force it
      ##   ## to be loaded, however, so make sure your radR installation is
      ##   ## set to load the declutter plugin by default.)

      ##   clutter.filename = NULL
      ## ),


################################################################################
###
###  Name of a file with zone information, formatted like zones/default.zone.R
###  The default, "zones/default.zone.R" does not contain any active zones.
###  You can set zonefile = NULL to prevent radR from reading a zone file at all.
###
################################################################################

      ## zonefile = "zones/default.zone.R"

################################################################################
###
###  video config (from plugins/video/video.conf.R)
###
################################################################################

      ## video = list (

      ##  default.width = 320,            ## video is resampled to these dimensions and frame rate

      ##  default.height = 200,

      ##  default.frame.rate = 10,

      ##  default.scale = 5,

      ##  ## one way to obtain differences between consecutive frames
      ##  default.ffmpeg.options.before.input = "-filter_complex 'format = gbrp, tblend=all_mode=difference'",

      ##  ## a different way to obtain differences: frame - average of preceding and following frames
      ##  ## you probably don't want to use both of these methods at the same time
      ##  default.ffmpeg.optios.before.output = "-vf tmix=frames=3:weights=\"-1 2 -1\":scale=1",
      ## )
)
