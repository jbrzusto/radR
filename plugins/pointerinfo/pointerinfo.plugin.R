######################################################################
###
### pointerinfo.plugin.radR
###  
### the pointerinfo radR plugin, for displaying location and patch
### information near the cursor in the plot window
###
### Note: this plugin only functions if the radR GUI is installed
###
######################################################################

about = function() {
  return("What is at the cursor in the plot window?\nDescribe it in a popup window and copy details to the clipboard.")
}

get.menus = function() {
  list(
       plugin = list(
         list(option="choose.any",
              on.set = function(n, s) {
                GUI$info.window.follows.mouse <- s
                GUI$info.window.coords <- GUI$last.pointer.coords
                gui.show.pointer.info()
              },
              "Info window follows the mouse" = GUI$info.window.follows.mouse
              ),
         "Data displayed at plot cursor:",
         list(option="choose.any",
              on.set = function(n, s) {
                show[[n]] <<- s
                rss.gui(SET_PLOT_CURSOR, if (any(show)) "arrow" else "hand2")
              },
              "Range, bearing, elevation angle"   = show$range.bearing,
              "Spatial coordinates" = show$spatial.coords,
              "Sample details"      = show$sample.details,
              "Cell statistics"     = show$cell.stats,
              "Patch statistics"    = show$patch.stats
              ),
         "---",
         list(option="choose.any",
              on.set = function(n, s) auto.clipboard <<- s,
              "Automatically copy info to clipboard" = auto.clipboard
              )
         )
       )
}

enable = function(enab) {
  ## enable or disable the plugin, according to the value of "enable"
  ## FIXME: save the current cursor, rather than assuming it is hand2
  if (enab) {
    rss.gui(SET_PLOT_CURSOR, "arrow")
    for (n in names(RSS$hooks$PLOT_CURSOR_MOVED))
      if (RSS$hooks$PLOT_CURSOR_MOVED[[n]]$read.only)
        rss.enable.hook("PLOT_CURSOR_MOVED", n)
  } else {
    rss.gui(SET_PLOT_CURSOR, "hand2")
    for (n in names(RSS$hooks$PLOT_CURSOR_MOVED))
      if (RSS$hooks$PLOT_CURSOR_MOVED[[n]]$read.only)
        rss.disable.hook("PLOT_CURSOR_MOVED", n)
  }
}

hooks = list(

  PLOT_CURSOR_MOVED = list( enabled = FALSE, read.only = TRUE,
    f = function(plot.coords, spatial.coords, sample.coords, cell.coords) {
      if (!any(show)) {
        return (NULL)
      }
      txt <- c()
      clip <- c()
      n <- 0
      if (show$range.bearing && !is.null(spatial.coords$rb)) {
        n <- n + 1
        txt[n] <- formatter$range.bearing(spatial.coords$rb)
        clip[n] <- clipboard.formatter$range.bearing(spatial.coords$rb)
      }
      if (show$spatial.coords && !is.null(spatial.coords$xyz)) {
        n <- n + 1
        txt[n] <- formatter$spatial(spatial.coords$xyz, spatial.coords$t)
        clip[n] <- clipboard.formatter$spatial(spatial.coords$xyz, spatial.coords$t)
      }
      if (RSS$have.valid$scan.data) {
        if (!is.null(sample.coords) && !any(is.na(sample.coords))) {
          ## get the patch and blip index for this patch
          current.patch <<- attr(rss.patch.at.sample.pulse(sample.coords), "index")
          if (RSS$have.valid$stats) {
            ## get cell stats
            cell.stats <- c(RSS$mean.mat[t(cell.coords)], RSS$dev.mat[t(cell.coords)])
          }
          if (show$sample.details) {
            ## format sample value and z score 
            n <- n + 1
            sample.details <- RSS$scan.mat[t(sample.coords)]
            if (RSS$have.valid$scores)
              sample.details[2] <- RSS$score.mat[t(sample.coords)] / RSS$score.scale
            else
              sample.details[2] <- 0
            txt[n] <- formatter$sample.details(sample.coords, sample.details)
            clip[n] <- clipboard.formatter$sample.details(sample.coords, sample.details)
          }
          if (show$cell.stats && RSS$have.valid$stats) {
            ## format cell stats
            n <- n + 1
            txt[n] <- formatter$cell.stats(cell.coords, cell.stats)
            clip[n] <- clipboard.formatter$cell.stats(cell.coords, cell.stats)
          }
          if (show$patch.stats && RSS$have.valid$patches) {
            ## format patch stats
            if (!is.null(current.patch)) {
              n <- n + 1
              patch.stats <- RSS$patches[current.patch[1],]
              txt[n] <- formatter$patch.stats(current.patch, patch.stats)
              clip[n] <- clipboard.formatter$patch.stats(current.patch, patch.stats)
            }
          }
        }
      }
      return(c(paste(txt, collapse="\n"),
               if (auto.clipboard) paste(clip, collapse=GUI$clipboard.element.separator) else ""
               ))
    })
  )  ## end of hooks

## state variables
current.patch = NULL

