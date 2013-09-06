##
## Fix the duration of each sweep in each blipmovie in a folder (and subfolders).
##
## Due to an as-yet undetermined bug, when a set of XIR3000ARCH files has been
## converted into a blipmovie, a scan occasionally has a wildly wrong duration.
## This throws the tracker for a serious loop, causing all manner of wild tracks.

## This script finds every blipmovie in a folder and subfolders, and corrects
## durations on all but the last sweep using the timestamps of sweeps.
## If the duration on the last sweep is wonky (more than 20% above the average
## sweep duration), it is set to the average sweep duration.

folder = rss.gui("FILE_DIALOG", mode="open.dir", title="Choose a folder of blipmovies for correcting sweep durations", init.file=".")
bms = dir(folder, full.names=TRUE, pattern="\\.[Bb][Mm]$", recursive=TRUE)

if (length(bms) == 0) {
  rss.gui("POPUP_MESSAGEBOX", "No blipmovies found",
          sprintf ("The folder\n   %s\n does not contain any blipmovies, not even in subfolders."))
} else {

  btn = rss.gui("POPUP_DIALOG", "Fix blipmovie sweep durations?",
    sprintf("You have selected the folder\n   %s\nwhich contains %d blipmovies in total.\n\nShould I fix sweep durations in all of these?", folder, length(bms)),
    buttons=c("  Yes  ", "Cancel"))

  gui.print.cons(btn)

  if (btn == 1) {

    id = -1

    for (bm in bms) {
      if (id >= 0)
        rss.gui("DELETE_MESSAGEBOX", id)
      bl = biglist(bm)
      toc = bl[[1]]
      if (is.null(toc)) {
        msg = sprintf("Blipmovie\n   %s\nis empty!", bm)
      } else {
        msg = sprintf("Blipmovie\n   %s\nhas %d segments with a total of %d sweeps.\n", bm, nrow(toc), sum(toc$num.scans))
      }
      id = rss.gui("POPUP_MESSAGEBOX", "Fixing blipmovie sweep durations", msg)
      if (is.null(toc)) {
        Sys.sleep(2)
        next
      }
      for (seg in 1:nrow(toc)) {
        ns = toc$num.scans[seg] 
        if (ns < 2) {
          ## skip if we can't correct
          next
        }
        fs = toc$first.slot[seg]
        step = bl[[fs]]$n.slot
        ## first scan info
        sind1 = fs + 1
        si1 = bl[[sind1]]
        last.ts = si1$timestamp
        last.i.duration = which(names(si1) == "duration")

        ## total duration, for calculating mean duration
        totdur = 0
        sind = sind1
        ## step through subsequent sweeps
        for (i in seq(1:(ns-1))) {
          ## index of next scan information
          sind = sind + step
          ## next scan info
          sin = bl[[sind]]
          ## names of next scan info items
          nsin = names(si1)[as.logical(rawToBits(sin[[1]]))]
          ## which are timestamp and duration
          i.timestamp = 1 + which(nsin == "timestamp")
          i.duration = 1 + which(nsin == "duration")
          dur = 1000 * as.numeric(sin[[i.timestamp]] - last.ts)
          totdur = totdur + dur
          ## correct duration of previous scan
          bl[[sind - step]][[last.i.duration]] = dur

          ## remember timestamp and slot for duration
          last.ts = sin[[i.timestamp]]
          last.i.duration = i.duration
        }
        mean.dur = totdur / (toc$num.scans[seg] - 1)
        last.dur = sin[[i.duration]]
        ## if last duration is more than 20% off mean, then set it to mean
        if (abs(last.dur - mean.dur) / mean.dur > 0.2)
          bl[[sind]][[i.duration]] = mean.dur

      }
      ## finished all segments
      close(bl)
    }
    rss.gui("POPUP_DIALOG", "Finished running script", sprintf("Fixed sweep durations in %d blipmovies.", length(bms)), buttons="   Ok   ")
  }
}
