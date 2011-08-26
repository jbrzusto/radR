######################################################################
###
### OBSOLETE: savescores.plugin.radR
### NOW HANDLED BY plugins/interfaces/blipmoviearch.plugin.R
###
### a (temporary) radR plugin for saving scores of blips
###
### a short machine-readable description of this plugin is available
### in the file savescores.desc.R
###
######################################################################

about = function() {
  if (num.scoremats > 0) {
    first <- first.timestamp
    class(first) <- "POSIXct"
    first <- paste("\nEarliest scores from scan: ",
                   format(first, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
    last <- last.timestamp
    class(last) <- "POSIXct"
    last <- paste( "\nLatest scores from scan:   ",
                  format(last, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE))
  } else {
    first <- last <- ""
  }
  return(plugin.label %:% "\nVersion " %:% version %:% "\n" %:%
         ifelse(new.file, "\nWriting new file.", "\nAppending to existing file.") %:%
         "\nCurrent output file is " %:% ifelse(length(score.filename) > 0, score.filename, "not defined") %:%
         "\nNumber of scans for which scores written:  " %:% num.scoremats %:%
         first %:% last %:%
         ifelse(have.file.data, "", "\n**File data has not been read; timestamps and count apply to the current run.") %:%
         "\n\nCurrently " %:% ifelse(enabled, "", "not ") %:% "saving scores." %:%
         "\n")
  
}

get.menus = function() {
  list(
       plugin = list(

         "Choose output file..." = list(
           "file",
           type="save",
           title = "Choose a file for saving scores",
           file.types = list(".scr" = "radR score files", ".*" = "All files"),
           on.set = function(f) {
             open.file(f)
           },
           init.file = function() score.filename,
           init.dir  = function() score.filename
           ),
         
         "Delete all scores from current file" = function() {
           gui.popup.dialog("Confirm deletion",
                            "Delete all blips from file " %:% score.filename %:% " ?",
                            function(button) {
                              if (button == 1)
                                delete.blips()
                            },
                            buttons=c("Yes", "No"),
                            default=2)
         }
         )
       )
}

## open the blip file when plugin gets enabled,
## close it when plugin gets disabled

enable = function(enab) {
  enabled <<- enab
  if (!enabled) {
    close_file()
  }
}

## try to open blip file for output

open.file = function(f) {

  close_file()

  score.filename <- f
  
  ## if the file doesn't exist, we'll need to write a header
  new.file <<- !file.exists(f)
  score.file <<- file(f, if (new.file) "wb" else "ab")

  if (!is.null(score.file)) {
    if (!enabled)
      rss.enable.plugin("savescores")
  } else {
    gui.error(paste("Unable to open file", f))
  }
  return()
}

## close the current output file

close_file = function () {
  if (!is.null(score.file))
    close(score.file)
  score.file <<- NULL
  reset.file.parms()
}

reset.file.parms = function () {
  num.scoremats <<- 0
  first.timestamp <<- Inf
  latest.timestamp <<- -Inf
}

delete.blips = function() {
  close_file()
  try(file.remove(score.filename), silent=TRUE)
  plot.min.timestamp <<- NULL
  plot.max.timestamp <<- NULL
}

load = function() {
  ## initialize state variables here
  reset.file.parms()
  rss.dyn.load("savescores", in.dir.of=plugin.file, local=TRUE)
}

unload = function(save.config) {
  rss.dyn.unload("savescores")
  close_file()
}

hooks = list(
  

  ONPLAY = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## if there is no file open and there is a blip filename
      ## then open it for appending

      if (is.null(score.file) && !is.null(score.filename))
        open.file(score.filename)
    }),

  ONSTOP = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## close the blip file when we stop.
      
      ## if the plugin is disabled or no output file is open, bail out
      if (! enabled || is.null(score.file))
        return ()

      close_file()
    }),

  FULL_SCAN = list (enabled = TRUE, read.only = TRUE,
    f = function(...) {
      ## write the timestamp, scorelength (in bytes) and scores in R binary format
      ## to the current file
      if (!is.null(score.file) && RSS$have.valid$scores) {
        raw.scores <- .Call("score_mat_to_raw", RSS$patch.buff, RSS$score.mat, PACKAGE="savescores")
        writeBin(c(as.integer(RSS$scan.info$timestamp), length(raw.scores)), score.file, endian="little")
        writeBin(raw.scores, score.file);
      }
    })
  )  ## END OF HOOKS

## state variables

score.file         = NULL
first.timestamp    = NULL
have.file.data     = NULL
latest.timestamp   = NULL
new.file           = NULL
num.scoremats      = NULL
plot.max.timestamp = NULL
plot.min.timestamp = NULL
