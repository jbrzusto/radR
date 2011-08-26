## svn: $Id: create_rawarch_TOC.R 596 2010-05-25 00:02:15Z john $

## Create a table of contents for a raw archive which does not have one,
## presumably because recording stopped outside of program control.
## This should be done automatically by the rawarch plugin...


f <- rss.gui("FILE_DIALOG",
                  mode = "open one",
                  title = "Choose rawarch file",
                  types = list(".raw.biglist" = "radR raw archive", ".*" = "All files"),
                  init.file = GUI$default.dir)

if (length(f) > 0) {
  bl <- biglist(f)
  ns <- floor((dim(attr(bl, "ndx"))[1] - 1) / 2)
  start <- as.integer(floor(as.numeric(bl[[2]]$timestamp)))
  end <- as.integer(floor(as.numeric(bl[[2 + (ns - 1)*2]]$timestamp)))
  bl[[1]] <- list(num.scans=ns, start.time=start, end.time=end)
  close(bl)

  rss.gui("POPUP_MESSAGEBOX", "Finished", sprintf("The raw archive %s\nshould now open correctly.", f))
  
}
