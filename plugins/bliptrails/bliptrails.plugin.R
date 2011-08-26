##  svn $Id: bliptrails.plugin.R 256 2009-03-02 20:33:09Z john $
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


## The bliptrails radR plugin, for displaying blips from previous
## scans.  The approach is to paint retained data into shadow copies
## of the scan data, class, and score matrices, then scan convert into
## the pixel matrix before the current scan's data are scan converted.
##
## There are two basic modes:
##
##  last-n-scans: retain blip geometry, samples, and (optionally) scores
##                from the previous n scans.  These are painted into
##                the shadow scan matrix in order from least to most recent
##                with class "other".  So if there are overlaps, the
##                most recent data/scores dominate.
##                Old blip sample data is faded for display.
##
##  storage-tube: retain blip geometry, samples, and (optionally) scores
##                from all previous scans until cleared.
##                This is painted into the class matrix with
##                class "other".
##
##  In all cases, RSS$scan.mat, RSS$score.mat, and RSS$class.mat are
##  left untouched - this plugin only modifies RSS$pix.mat.
##
##  Shadow matrices stored.samples, stored.classes, and stored.scores
##  either retain values for all blips for all scans (until cleared),
##  or are painted after each scan with the values for the preceding
##  n scans.
##
##  This scan conversion occurs before the one for current data, and only
##  if RSS$show.class$other is TRUE.
##
##  The PLOT_CURSOR_MOVED hook creates additional lines for the
##  pointerinfo window from old sample and old score data, if these are retained.


about = function() {
  return("Display blips from the previous n or all previous scans.")
}

get.menus = function() {
  list(
       plugin = list(
         "Current mode:",
         c(list(option="choose.one",
                on.set = function(n) {
                  current.mode <<- available.modes[n]
                  if (enabled)
                    rss.gui(SET_CLASS_PALETTE, "other", default.palette[[current.mode]], default.gamma[[current.mode]])
                }),
           structure(current.mode == available.modes, names=available.modes)
           ),
         list (nsk="gauge",
               label = "scans to retain",
               range = c(1, 500),
               increment = 1,
               value = num.scans.kept,
               on.set = function(x) {
                 rss.defer.assign(num.scans.kept, x, BLIPTRAILS)
                 update()
               }
               ),
         list (ff="gauge",
               label = "fadeout factor",
               range = c(0, 1),
               increment = 0.02,
               value = fadeout,
               on.set = function(x) { fadeout <<- x; update()}
               ),
         "Clear trails" = function() {
           zero.data()
           rss.gui(UPDATE_PLOT_WINDOW, reconv=TRUE)
         },
         "---",
         list(keep="choose.any",
              on.set = function(n, v) {
                ## enable retaining scores
                ## and fill an empty list of retained data
                ## to the same length as currently retained blip
                ## geometry.  This keeps code below simpler.
                switch(n,
                       {
                         retain.scores <<- v
                         if (v) {
                           blip.scores <<- rep(as.integer(0), length(blip.geom))
                           dim(stored.scores) <- dim(stored.classes)
                         }
                       }
                       )
              },
              "retain scores from old blips"
              )
         )
       )
}

update = function() {
  if (RSS$play.state < RSS$PS$PLAYING)
    gui.update.plot.window(TRUE)
}
                    
enable = function(enab) {
  if (enab) {
    saved.show.cold <<- RSS$show.class$cold
    ensure.matrices()
    for (h in names(hooks))
      rss.enable.hook(h, name)
    rss.gui(SET_CLASS_DISPLAY, "other", TRUE)
    rss.gui(SET_CLASS_DISPLAY, "cold",  FALSE)
    rss.gui(SET_CLASS_PALETTE, "other", default.palette[[current.mode]], default.gamma[[current.mode]])
  } else {
    zero.data()
    ## save the current palette and gamma for this mode
    default.gamma[[current.mode]] <<- RSS$class.gamma$other
    default.palette[[current.mode]] <<- RSS$class.palette$other
    for (h in names(hooks))
      rss.disable.hook(h, name)
    rss.gui(SET_CLASS_DISPLAY, "other", FALSE)
    rss.gui(SET_CLASS_DISPLAY, "cold",  saved.show.cold)
  }
}

load = function() {
  stored.classes    <<- extmat("bliptrails class", type=RSS$types$class)
  stored.samples    <<- extmat("bliptrails raw samples",  type=RSS$types$sample)
  stored.scores     <<- extmat("bliptrails scores",  type=RSS$types$score)
}

zero.data = function () {
  blip.geom         <<- integer(0)
  blip.samples      <<- integer(0)
  blip.scores       <<- integer(0)
  scan.sample.count <<- integer(0)
  zero(stored.classes)
  zero(stored.samples)
  zero(stored.scores)
}

ensure.matrices = function() {
  ## make sure trail storage matrices are of
  ## sufficient size
  if (!identical(dim(stored.classes), dim(RSS$class.mat))) {
    dim(stored.classes)  <<- dim(RSS$class.mat)
    dim(stored.samples)  <<- dim(RSS$class.mat)
    dim(stored.scores)   <<- dim(RSS$class.mat)
    zero.data()
  }
}


drop.scans = function (n) {
  ## remove the oldest n scans of data
  if (n >= 1) {
    ## make sure to use the same value for all arrays!
    ## (a GUI callback can change num.scans.kept in the middle
    ## of this function)
    
    drop.samples <- - seq(length = sum(scan.sample.count[1:n]))
    scan.sample.count <<- scan.sample.count[-(1:n)]
    if (length(drop.samples) > 0) {
      blip.geom       <<- blip.geom[drop.samples]
      blip.samples    <<- blip.samples[drop.samples]
      if (retain.scores)
        blip.scores   <<- blip.scores[drop.samples]
    }
  }
}
  
hooks = list(
  ## Two main jobs are accomplished by hooks:

  DONE_SCAN = list (enabled = FALSE, read.only = FALSE,
    f = function(...) {

      ## 1. Acquire the new blip data for this scan if we are processing
      ##    it for real (i.e. not previewing)

      if (RSS$play.state < RSS$PS$PLAYING || RSS$previewing)
        return()
      
      ## check that saved matrix dimensions match current data
      ## if not, redimension and zero the data
      
      ensure.matrices()
      
        switch(current.mode,
               "last N scans" = {
                 ## get rid of oldest retained blips, if necessary
                 ## Note that if retain.X is TRUE, then
                 ## length(blip.X) == length(blip.geom),
                 ## due to the code in enable() above
                 ## We keep one extra scan's worth of blips around,
                 ## because we want N tail scans, but we also want to
                 ## not change the stored data after painting it, because
                 ## then a re-render will lose the oldest tail.
                 ## A re-render would occur if the user zoomed while paused, for
                 ## example.
                 drop.scans(length(scan.sample.count) - num.scans.kept + 1)
                 
                 ## grab the blip geometry
                 geom <- rss.get.all.blips(linear=TRUE, blip.num=FALSE)
                 ## record how many samples are for this scan
                 scan.sample.count <<- c(scan.sample.count, length(geom))
                 blip.geom <<- c(blip.geom, geom)
                 blip.samples <<- c(blip.samples, RSS$scan.mat[geom])
                 if (retain.scores)
                   blip.scores <<- c(blip.scores, RSS$score.mat[geom])
               },
               "storage tube" = {
                 geom <- rss.get.all.blips(linear=TRUE, blip.num=FALSE)
                 stored.classes[geom] <<- RSS$CLASS.VAL$other
                 stored.samples[geom] <<- RSS$scan.mat[geom]
                 if (retain.scores)
                   stored.scores[geom] <<- RSS$score.mat[geom]
               })
      }
    ),
    
  PRE_SCAN_CONVERT = list( enabled = FALSE, read.only = TRUE,

    f = function(...) {

      ##  2.  paint current saved trail data into pixel matrix 
      ##
      ## If sample data has been retained, and the "other" class is being shown,
      ## scan convert from the retained data into the pixel matrix
      ## but only for pixels of class "other", and only if RSS$show.class$other is TRUE
      ## As this is done before conversion of the actual data for the current scan,
      ## cold, hot, and blip pixels from the current scan can end up overlaying
      ## the image created here.
      ## check that saved matrix dimensions match current data
      ## if not, redimension and zero the data
      
      if (RSS$show.class$other) {
        ## we paint saved data into the pixel matrix
        switch(current.mode,
               "last N scans" = {
                 ## paint n scans' worth of blip data into the
                 ## storage tube class, sample, and possibly score matrices
                 zero(stored.classes)
                 zero(stored.samples)
                 if (retain.scores)
                   zero(stored.scores)
                 ## paint in the "other" class
                 if (length(blip.geom) > 0) {
                   ## if we've retained more scans of data than we're currently
                   ## supposed to display, make truncated local copies of relevant variables
                   if ((n <- length(scan.sample.count) - num.scans.kept) > 0 ) {
                     scan.sample.count <- tail(scan.sample.count, num.scans.kept) ## local assignment
                     keep <- sum(scan.sample.count)
                     blip.geom <- tail(blip.geom, keep) ## local assignment
                     blip.samples <- tail(blip.samples, keep) ## local assignment
                     if (retain.scores)
                       blip.scores <- tail(blip.scores, keep) ## local assignment
                   }
                   if (num.scans.kept > 0) {
                     ## if we have something to actually show
                     stored.classes[blip.geom] <- RSS$CLASS.VAL$other
                     if (fadeout == 1.0)
                       stored.samples[blip.geom] <- blip.samples
                     else {
                       fade.factor <- rep(1024 / fadeout ^ (rev(seq(along=scan.sample.count))), times = scan.sample.count)
                       stored.samples[blip.geom] <- (blip.samples * 1024L) %/% fade.factor
                     }
                     if (retain.scores)
                       stored.scores[blip.geom] <- blip.scores
                   }
                 }
               },
               "storage tube" = {}
               )
        ## scan convert the storage tube data using the
        ## storage tube class matrix, but only rendering
        ## pixels of class "other" in order to preserve
        ## background in the pixel matrix.
        ## Disable this hook so it is not called recursively.
        
        rss.disable.hook("PRE_SCAN_CONVERT", name)
        rss.convert.scan(reconv=TRUE, ## force a scan conversion
                         scan.mat         = stored.samples,
                         class.mat        = stored.classes,
                         show.class       = RSS$CLASS.VAL == RSS$CLASS.VAL$other,
                         paint.background = FALSE
                         )
        rss.enable.hook("PRE_SCAN_CONVERT", name)
      }
    }),

  ONSTOP = list( enabled = FALSE, read.only = FALSE,
    f = function(...) {
      ## delete saved info
      zero.data()
    }),
  
  START_SOURCE = list( enabled = FALSE, read.only = FALSE,
    f = function(...) {
      ## delete saved info
      zero.data()
    }),
  
  PLOT_CURSOR_MOVED = list( enabled = FALSE, read.only = TRUE,
    f = function(plot.coords, spatial.coords, sample.coords, cell.coords) {
      txt <- c()
      ## We must call ensure.matrices(), since there's currently no way to trigger
      ## this immediately after scan.matrix etc. change in shape, after which
      ## point the coords valid for them may no longer be valid for this plugin's
      ## matrices.
      ## (There's a TODO entry about this).
      
      ensure.matrices()
      if (enabled && !is.null(sample.coords) && !any(is.na(sample.coords))) {
        if (stored.classes[t(sample.coords)] == RSS$CLASS.VAL$other) {
          txt <- c(txt, formatter$old.value(stored.samples[t(sample.coords)]))
          txt <- c(txt, clipboard.formatter$old.value(stored.samples[t(sample.coords)]))
          if (retain.scores) {
            txt <- c(txt, formatter$old.score(stored.scores[t(sample.coords)] / RSS$score.scale))
            txt <- c(txt, clipboard.formatter$old.score(stored.scores[t(sample.coords)] / RSS$score.scale))
          }
        }
      }
      return(txt)
    })
  
  ) ## end of hooks list

## state variables
scan.sample.count = integer(0)
blip.geom         = integer(0)
blip.samples      = integer(0)
blip.scores       = integer(0)
saved.show.cold   = FALSE

stored.classes  = NULL
stored.samples  = NULL
stored.scores   = NULL
