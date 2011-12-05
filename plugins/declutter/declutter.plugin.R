##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2011 John Brzustowski        
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


## The declutter radR plugin, for removing persistent fluctuating clutter
## from recorded data.  The plugin has two modes:
##
##    learn:  accumulate counts of blip samples in each sample slot, to obtain
##            an occupancy rate
##
##    filter: remove blips with sufficiently high mean sample occupancy rate
##
## Typically, the blipmovie is processed twice:
##   - once in learn mode, to learn the sample slot occupancy rates
##   - once in filter mode, to remove clutter blips, build tracks, etc.
##
## The learned sample slot occupancy rates can be saved and re-used for other blipmovies
## (presumably from the same physical site).  The learning pass does not
## necessarily have to consist of the entire blipmovie.
##
## Currently, operation is manual: the user must make two passes
## through the blipmovie.
##
## Implementation:
##
##   - during the learning pass, each time a sample is classified as a
##     blip (i.e. RSS$class.mat[i, j] == RSS$CLASSVAL$blip, we increment
##     the hits matrix:  hit[i, j] <- 1 + hit[i, j]
##
##   - after the learning pass of N scans, each sample slot [i, j] has an occupancy
##     rate (from 0 to 1) given by: orate[i, j] = hits[i, j] / N
##
##   - in the filtering pass, a blip is assigned a clutter score (from
##     0 to 1), and blips with clutter scores above blip.cutoff are
##     filtered out.  The blip's clutter score is the mean occupancy
##     rate of its samples.  For example, if a blip has 15
##     samples whose sample slot indexes are given by is[1:15] and
##     ip[1:15], then the blip's clutter score is
##        blip.clutter.score = mean(orate[cbind(is, ip)])

about = function() {
  t1 <- "Filter out persistent fluctuating clutter using a simple occupancy threshold.\n\n"
  if (num.scans.learned > 0) {
    t2 <- sprintf("Number of scans learned:%s\n",
                  num.scans.learned
                  )
  } else {
    t2 <- "No clutter learned.\n"
  }
  return (paste(t1, t2, sep=""))
}

get.menus = function() {
  list(
       plugin = list(
         "Current mode:",
         c(list(currmode = "choose.one",
                set.or.get = "gui.set.mode",
                on.set = function(n) {
                  current.mode <<- available.modes[n]
                  update()
                }),
           structure(current.mode == available.modes, names=available.modes)
           ),
         list (cutoff="gauge",
               label = "blip cutoff for mean occupancy rate",
               range = c(0, 1),
               increment = 0.0001,
               value = cutoff,
               on.set = function(x) {
                 cutoff <<- x
                 update()
               }
               ),
         "---",
         "Load a learned clutter file..." =
         list ("file",
               type = "open.one",
               title = "Choose a saved clutter map", 
               file.types = file.types.list,
               on.set = load.clutter.file,
               init.file = function() clutter.filename
               ),
         "Reload current image file" = function() {
           load.clutter.file(clutter.filename)
           update()
         },
         "Save clutter map" = function() save.clutter.file(),
         "Save as ..." =
         list ("file",
               type = "save",
               title = "Save current clutter map", 
               file.types = file.types.list,
               on.set = function(f) {
                 if (length(grep(names(file.types.list)[1] %:% "$", f)) == 0)
                   f <- f %:% names(file.types.list)[1]
                 save.clutter.file(f)
               },
               init.file = function() clutter.filename
               ),
         "---",
         list(show="choose.any",
              "Plot occupancy rate as class 'other' (SLOW!)" = plot.occrate,
              set.or.get = "gui.set.plot.occrate",
              on.set = function(n, v) {
                set.occrate.plotting(v, enabled)
              }
              ),
         "Forget learned clutter" = function() {
           hits <<- NULL
           occrate <<- NULL
           num.scans.learned <<- num.scans.occrate <<- 0
         }
         )
       )
}

update = function() {
  if (RSS$play.state < RSS$PS$PLAYING && RSS$have.valid$scan.data)
    rss.process.scan(put.scan = FALSE,
                     calculate.scores = FALSE, classify.samples = FALSE,
                     convert.scan = TRUE, is.preview = TRUE)
}
                    
enable = function(enab) {
  rss.enable.hook("PATCH_STATS", name, enab)
  rss.enable.hook("DONE_SCAN", name, enab)
  rss.enable.hook("PLOT_CURSOR_MOVED", name, enab)
  set.occrate.plotting(plot.occrate, enab)
}

set.occrate.plotting = function(enab, plugin.enabled = enabled) {
  if (enab) {
    saved.show.other <<- RSS$show.class$other
    saved.other.palette <<- RSS$class.palette$other
    saved.other.gamma <<- RSS$class.gamma$other
    rss.gui(SET_CLASS_DISPLAY, "other",  TRUE)
    rss.gui(SET_CLASS_PALETTE, "other", default.palette, default.gamma)
  } else if (!is.null(saved.show.other)) {
    default.gamma <<- RSS$class.gamma$other
    default.palette <<- RSS$class.palette$other
    rss.gui(SET_CLASS_DISPLAY, "other", saved.show.other)
    rss.gui(SET_CLASS_PALETTE, "other", saved.other.palette, saved.other.gamma)
  }
  rss.enable.hook("PRE_SCAN_CONVERT", name, plugin.enabled && enab)
  plot.occrate <<- enab
  if (exists("gui.set.plot.occrate"))
    gui.set.plot.occrate(1, enab)
  paint.occrate(plugin.enabled && enab)
  update()
}

load = function() {
  load.clutter.file()
  set.occrate.plotting(plot.occrate)
}

ensure.matrices = function() {
  ## make sure trail storage matrices are of
  ## sufficient size
  if (!identical(dim(hits), dim(RSS$class.mat))) {
    if (num.scans.learned > 0)
      rss.gui(POPUP_MESSAGEBOX,
              "Clutter map size mismatch",
            "The dimensions of the clutter map don't match those of the current data source.\nradR is discarding the clutter map.\n\n" %:%
              sprintf ("   clutter map size: (%5d samples, %5d pulses) \n   data matrix size: (%5d samples, %5d pulses) \n", dim(hits)[1], dim(hits)[2], dim(RSS$class.mat)[1], dim(RSS$class.mat)[2]),
            time.to.live=30)
    num.scans.learned <<- num.scans.occrate <<- 0
    hits <<- array(0L, dim=dim(RSS$class.mat))
  }
}

load.clutter.file = function(f = clutter.filename) {
  if (is.null(f))
    return()
  if (! file.exists(f)) {
    rss.gui(POPUP_MESSAGEBOX,
            "Clutter file  missing",
            "radR was unable to find the clutter file '" %:% f %:% "'",
            time.to.live=30)
    return()
  }
  base::load(f, DECLUTTER)
  num.scans.occrate <<- 0 ## force recomputation, if/when required
  clutter.filename <<- f
  current.mode <<- "filtering"
  if (exists("gui.set.mode"))
    gui.set.mode(2)
  update()
}

save.clutter.file = function(f = clutter.filename) {
  save (num.scans.learned, hits, file=f)
}

paint.occrate <- function(yes) {
  if (yes) {
    if (num.scans.occrate != num.scans.learned || !identical(dim(hits), dim(occrate))) {
      ## re-compute occupancy rate
      occrate <<- extmat("sample occupancy rate", type=RSS$types$sample, dim=dim(hits))
      occrate[] <- (hits * as.integer(2^RSS$scan.info$bits.per.sample - 1)) %/% as.integer(num.scans.learned)
      num.scans.occrate <<- num.scans.learned
    }
    i <- which(RSS$class.mat[] == RSS$CLASS.VAL$cold)
    RSS$class.mat[i] <- RSS$CLASS.VAL$other
    RSS$scan.mat[i] <- occrate[i]
  } else {
    i <- which(RSS$class.mat[] == RSS$CLASS.VAL$other)
    RSS$class.mat[i] <- RSS$CLASS.VAL$cold
    RSS$scan.mat[i] <- 0L
  }
}
    

hooks = list(

  PATCH_STATS = list( enabled = FALSE, read.only = FALSE,
    f= function(is.blip) {
      ## if we have learned and are in filter mode, filter out blips
      ## with excessive occupancy scores
      
      if (!length(is.blip) || current.mode != "filtering" || num.scans.learned == 0)
        return(is.blip)

      bc <- rss.get.all.blips(blip.nums = TRUE, linear.coords = TRUE, collapse = FALSE, which.patches = is.blip)

      occ.score <- tapply(hits[bc[,2]], bc[,1], mean)

      is.blip[which(is.blip)[occ.score >= cutoff * num.scans.learned]] <- FALSE
      return(is.blip)
    }),
      
  DONE_SCAN = list (enabled = FALSE, read.only = FALSE,
    f = function(...) {

      ## Acquire the new blip data for this scan if we are processing
      ## it for real (i.e. not previewing)

      if (RSS$play.state < RSS$PS$PLAYING || RSS$previewing || current.mode != "learning")
        return()
      
      ## check that saved matrix dimensions match current data
      ## if not, redimension and zero the data
      
      ensure.matrices()

      if (length(RSS$blips) > 0) {
        sc <- rss.get.all.blips(blip.nums = FALSE, linear.coords = TRUE)
        hits[sc] <<- hits[sc] + 1L
      }
      num.scans.learned <<- num.scans.learned + 1
    }),

    
  PRE_SCAN_CONVERT = list( enabled = FALSE, read.only = TRUE,

    f = function(...) {

      ## Paint sample slot occupancy rates into pixel matrix
      ##
      ## scan convert from the occupancy rates into the pixel matrix
      ## but only for pixels of class "other", and only if RSS$show.class$other is FALSE
      ## (otherwise the occupancy rate plot will be overwritten by the other class scan conversion)
      ## As this is done before conversion of the actual data for the current scan,
      ## hot and blip pixels from the current scan can end up overlaying
      ## the image created here.

      if (RSS$show.class$other && num.scans.learned > 0) {
        paint.occrate(TRUE)
      }
    }),
  
  PLOT_CURSOR_MOVED = list( enabled = FALSE, read.only = TRUE,

    ## show occupancy rate information for underlying sample and patch
    
    f = function(plot.coords, spatial.coords, sample.coords, cell.coords) {
      
      if (enabled && !is.null(sample.coords) && !any(is.na(sample.coords)) && num.scans.learned > 0) {
        occ.info <- sprintf("Sample slot occupancy rate: %8.6f", hits[sample.coords[1],sample.coords[2]] / num.scans.learned)
        patch.coords <<- rss.patch.at.sample.pulse(sample.coords)
        if (!is.null(patch.coords)) {
          occ.info <- paste(occ.info, sprintf("  Patch mean occupancy rate: %8.6f", mean(hits[patch.coords]) / num.scans.learned), sep="")
        }
      } else {
        occ.info <- ""
      }
      return(c(occ.info, ""))
    })
  
  ) ## end of hooks list

## state variables

file.types.list = list(".clutter.Rdata" = "radR clutter map", ".*" = "All files")
num.scans.learned = 0
num.scans.occrate = 0
saved.show.other = NULL
saved.other.palette = NULL
saved.other.gamma = NULL
hits = NULL
occrate = NULL

