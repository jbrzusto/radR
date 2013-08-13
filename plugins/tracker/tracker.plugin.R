##  svn $Id: tracker.plugin.R 574 2010-05-11 02:07:15Z john $
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
##
##   A radR plugin for calculating, saving, and plotting tracks.
##
## A short machine-readable description of this plugin is available
## in the file tracker.desc.R
##
## Key Objects:
##
##        all.scans   (big)list of complete scan info;
##                    Scans are added to this list in the order they are processed.
##                    Fields in each (big)list item:
##                     - timestamp  : double: seconds (including fractional) past the epoch
##                     - tracks     : int   : index in all.tracks of tracks active in this scan
##                     - first.blip : int   : index into blips of first blip
##                     - num.blips  : int   : number of blips
##
##        all.blips   (big)frame of blip info;
##                    Written in the order they are processed, scan by scan.
##                    Fields:
##                     - x         : double
##                     - y         : double
##                     - z         : double
##                     - t         : double
##                     - ns        : int
##                     - area      : double
##                     - intensity : double
##                     - max       : double
##                     - aspan     : int
##                     - rspan     : int
##                     - perim     : double
##                     - range     : double
##                     - scan      : int
##
##        num.blips   counter of blips processed
##
##        all.tracks  (big)list of complete tracks;
##                    Tracks are indexed in the order they are begun, but are written
##                    to the biglist file in the order they are completed.
##                    Fields in each (big)list item:
##                     - blips : int :  absolute blip indexes (i.e. index in bigframe "all.blips")
##   
##        tracks      list of nascent or active tracks;
##                    Tracks are begun with orphan blips.
##                    Each list item is a strictenv, with these symbols:
##                     - state  : integer:  state of the track; one of the TS.XXX constants
##                     - index  : int    :  absolute index of this track in all.tracks
##                     - blips  : int    :  absolute index of each blip (i.e. index in bigframe "all.blips")
##                     - gain   : double :  gains achieved by adding each blip
##                     - info   : list   :  list of state variables for target at each point in track
##                     - expiry : double :  time at which this track will be forced to complete.
##                                          Reset when a blip is added to it.
##                                          Affected by flag auto.expire.tracks.
##
##        num.tracks  counter of tracks begun; the index field of each item in tracks
##                    is taken from this variable
##
##        gui.tracks  dataframe holding gui objects that represent tracks.
##                    Any track visible on the plot (i.e. having a Tk canvas item)
##                    will have an entry in this table.  An entry in this table
##                    need not have a corresponding entry in all.tracks or tracks,
##                    because the track was ended/removed while plot updating was off.
##                    In that case, the record retained here will be available for
##                    cleaning up the screen if plot updating is turned back on.
##
##                    Columns:
##                    - tkid      : integer: tk canvas id of a track item
##                    - tid       : integer: index of a track in all.tracks biglist
##                    - atid      : integer: index of a track in the tracks object
##                    - state     : integer: same as the state field for the track object
##                    - last.time : double : last timestamp of a blip in that track
##                    - np        : integer: number of points in this track
##
##       saved.state  list with items as they were before a "preview" operation:
##                    - tracks     : a copy of the (nascent or active) tracks list (each strictenv item gets duplicated)
##                    - num.blips  : length of all.blips before blips from the preview scan are considered
##
##   gui.saved.state  list with gui items as they were before a "preview" operation:
##                    - shadow.tkid : integer : tk canvas ids of "shadow" tracks; i.e. copies
##                                              of tracks for use in previewing
##                    - tracks      : frame   : same structure as gui.tracks; holds records for those
##                                              tracks which were nascent/active before previewing
##
## tracks.completed.in.preview - list of strictenv() with same fields as tracks; this is the
##                    list of tracks completed or discarded during the most recent preview, and is
##                    used so that the gui can locate track information when the user points to a
##                    track completed or discarded in the preview.
##
##  Conventions:
##
##   i.blip: index of a blip in the object all.blips
##   gid:    a gui-track id; i.e. index of a row in gui.tracks frame
##   tid:    a track index; i.e. index of a track in all.tracks, the list of complete tracks
##   tkid:   the tk index of a canvas item representing a track
##   atid:   active track index; i.e. index of a track in tracks, the list of nascent/active tracks
##           this id is valid until the track is complete



about = function() {
  if (num.scans > 0) {
    times <- format(structure(as.integer(all.scans[c(1, num.scans)] %$0% timestamp), class="POSIXct"),
                    format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE)
    
    scan.info <- paste(num.scans, "scans processed\nfrom: ", times[1], "\nto:   ", times[2])
  } else {
    scan.info <- "No scans processed."
  }
  blip.info <- paste(num.blips, "blip(s) processed.")
  track.info <- paste(num.tracks, "track(s) begun;", length(all.tracks), "track(s) completed")
  return(paste(plugin.label,
               "\nDeveloped with the assistance of Environment Canada\nunder contract KR203-06-0102",
               "Version " %:% version,
               "Current CSV output file is " %:% ifelse(length(csv.filename) > 0, csv.filename, "not defined"),
               scan.info,
               blip.info,
               track.info,
               paste("\nCurrently ", ifelse(save.to.csv, "", "not "), "saving tracks to CSV file.", sep=""),
               sep="\n"))
}

get.menus = function() {
  list(
       plugin = c(list(
         list(keep="choose.any",
              on.set = function(n, v) {
                ## enable saving to .CSV
                switch(n,
                       {
                         if (v)
                           open.csv.file()
                         else
                           close.csv.file()
                         save.to.csv <<- v
                       }
                       )
              },
              ## name of a local function created to allow program setting of this option
              set.or.get = "set.csv.menubutton",
              "Save tracks to .CSV file" = save.to.csv
              ),
         "Choose .CSV output file..." = list(
           "file",
           type="save",
           title = "Choose a .CSV file for saving tracks",
           init.file = function() csv.filename,
           init.dir = function() csv.filename,
           file.types = structure(list("CSV files", "All files"), names=c(".csv", ".*")),
           on.set = function(f) {
             ## remove any trailing scanfile.ext (escaping the ".")
             if (length(grep("\\.csv$", f)) == 0)
               f <- f %:% ".csv"
             csv.filename <<- f
             open.csv.file()
           }),
         "---",
         "Model",
         c(list(option="choose.one",
                on.set = function(n) {
                  models[[current.model]]$deselect()
                  current.model <<- model.names[n]
                  models[[current.model]]$select()
                }),
           structure(current.model == model.names, names=paste(model.names, ":", models %$$% desc))
           )
         ),
         structure(lapply(models, function(m) m$get.menu()), names=model.names),
         list("---",
              "Choose output file..." = list(
                "file",
                type="save",
                title = "Choose a file for saving tracks",
                init.file = function() track.filename,
                init.dir = function() track.filename,
                file.types = structure(list("Tracker scan files", "All files"), names=c(scanfile.ext, ".*")),
                on.set = function(f) {
                  set.track.filename(f)
                }),

              list ("gauge",
                    label = "minimum number of blips required for a track",
                    range = c(2, 1000),
                    increment = 1,
                    value = track.min.blips,
                    on.set = function(x) { track.min.blips <<- x; reprocess.scan() }
                    ),

              list ("gauge",
                    label = "maximum speed of tracked objects (km/h)",
                    range = c(0, 1000),
                    increment = 1,
                    value = track.max.speed,
                    on.set = function(x) { track.max.speed <<- x; models[[current.model]]$set.max.speed(x); reprocess.scan() }
                    ),

              list ("gauge",
                    label = "minimum number of blips before a track is plotted",
                    range = c(1, 1000),
                    increment = 1,
                    value = min.blips.to.show,
                    on.set = set.min.blips.to.show
                    ),


              list ("gauge",
                    label = "how long to retain plots of complete tracks (seconds)",
                    range = c(0, 10000),
                    increment = 10,
                    value = retain.complete.tracks.for,
                    on.set = gui.set.retention.time
                    ),
              "---",
              
              "Remove all complete tracks from plot" = function() gui.drop.all.tracks(TRUE),

              "---",
              
              "Delete all tracks from current file" = function() {
                gui.popup.dialog("Confirm deletion",
                                 "Delete all tracks from file " %:% track.filename %:% " ?",
                                 function(button) {
                                   if (button == 1) {
                                     delete.track.file()
                                   }
                                 },
                                 buttons = c("Yes", "No"),
                                 default = 2)
              }
              )
         )
       )
}

enable = function(enab) {
  enabled <<- enab
  if (!enabled) {
    gui.takedown()

    ## disable all hooks
    for (hook in c("ONPLAY", "ONPAUSE", "ONSTOP", "DONE_SCAN", "PLOT_CURSOR_MOVED"))
      rss.disable.hook(hook, name)

    end.all.tracks()
  } else {
    ## enable the hooks that control the DONE_SCAN hook
    for (hook in c("ONPLAY", "ONPAUSE", "ONSTOP", "PLOT_CURSOR_MOVED"))
      rss.enable.hook(hook, name)

    gui.setup()
    
    ## open appropriate files and/or init variables
    
    if (is.null(all.blips) && !is.null(track.filename))
      open.track.files()

    if (save.to.csv)
      open.csv.file(no.gui.update=TRUE)

    ## If the all.blips bigframe was successfully opened,
    ## enable the DONE_SCAN hook to process each scan.
    
    if (!is.null(all.blips))
      rss.enable.hook("DONE_SCAN", name)
  }
}

reprocess.scan = function() {
  ## reprocess the current scan, presumably with new
  ## parameter values; called by parameter gauges
  if (RSS$play.state < RSS$PS$PLAYING) {
    rss.process.scan(put.scan         = FALSE,
                     calculate.scores = FALSE,
                     convert.scan     = FALSE,
                     update.plot      = FALSE,
                     is.preview       = TRUE
                     )
  }
}

set.track.filename = function(f) {
  ## set the base filename for recording
  ## scan, blip, and track info to f
  
  end.all.tracks()
  
  ## remove any trailing scanfile.ext (escaping the ".")
  f <- sub("\\" %:% scanfile.ext %:% "$", "", f)
  track.filename <<- f
  open.track.files()
}

delete.track.file = function() {
  ## delete all track info from the current bigframe/biglist objects
  ## and re-open them afresh.  Play will behave as if the currently-
  ## previewed scan is the first one.
  end.all.tracks()
  open.track.files(overwrite=TRUE)
}

load = function() {
  ## load all tracker models
  load.models()

  ## if there's no gui, reset all gui.XXX functions to No-ops

  noop <- function(...){}
  
  if (!RSS$have.gui) {
    for (n in ls(TRACKER, pattern="^gui\\..*"))
      if (is.function(get(n, envir=TRACKER, inherits=FALSE)))
        assign(n, noop, envir=TRACKER)
  }
}

unload = function(save.config=FALSE) {
  end.all.tracks()
  unload.models(save.config)
}

load.model = function(f) {
  m <- strictenv(PARENT=TRACKER)
  m <- rss.load.object.and.config(f, m)
  ### WARNING: the function parent.env<- might be removed in a later version of R
  ### because it is dangerous.  We use it here to get around the problem of 
  parent.env(m) <- TRACKER
  if ("load" %in% m)
    m$load()
  return(m)
}
  
load.models = function() {
  model.filenames <<- dir(tracker.model.pathlist, pattern=".*\\.model\\.R$", full.names=TRUE)
  model.names <<- gsub(".*[./]([^./]+)\\.model.R", "\\1", model.filenames, perl=TRUE)
  models <<- lapply(model.filenames, load.model)
  names(models) <<- model.names
}

unload.models = function(save.config) {
  for (i in seq(along=models)) {
    if ("unload" %in% models[[i]])
      models[[i]]$unload(save.config)
    if (save.config)
      rss.save.config(models[[i]], filename=sub("\\.model\\.R$", ".conf.R", model.filenames[[i]]))
  }
}

open.csv.file = function(no.gui.update = FALSE) {
  ## open a .csv file for writing tracks
  close.csv.file()
  if (!is.null(csv.filename)) {
    tryCatch (
              csv.file <<- file(csv.filename, "wt"),
              error = function(e) csv.file <<- NULL )
    if (!is.null(csv.file)) {
      save.to.csv <<- TRUE
      if (!no.gui.update)
        set.csv.menubutton(1, TRUE)
      cat (csv.file.header, file=csv.file)
      rss.enable.hook(RSS$TRACK_HOOK, name)
    } else {
      save.to.csv <<- FALSE
      if (!no.gui.update)
        set.csv.menubutton(1, FALSE)
    }
  }
}

close.csv.file = function() {
  ## close the .csv file, if open
  if (!is.null(csv.file)) {
    close(csv.file)
    csv.file <<- NULL
  }
  rss.disable.hook(RSS$TRACK_HOOK, name)
}

open.track.files = function (overwrite=TRUE) {
  ## template for the all.blips bigframe
  ## if overwrite is TRUE, any existing files are emptied
  ## otherwise, existing files are opened with contents
  ## preserved.  overwrite=FALSE will be useful if track building is
  ## stopped at a certain scan and later resumed there, or if
  ## we are opening the files to play back their contents (FIXME: implement this).
  
  blips <- data.frame(x=double(0), y=double(0), z=double(0), t=double(0), ns=integer(0), area=double(0), int=double(0), max=double(0), aspan=integer(0), rspan=integer(0), perim=double(0), range=double(0), scan=integer(0))
  ## FIXME: what, if anything, should go in the blips file header? radar geometry?
  all.blips <<- bigframe(track.filename %:% blipfile.ext, if (overwrite) blips else NULL)
  all.scans <<- biglist(track.filename %:% scanfile.ext, names=c("timestamp", "tracks", "first.blip", "num.blips"),
                        overwrite=overwrite)
  all.tracks<<- biglist(track.filename %:% trackfile.ext, names="points", overwrite=overwrite)
  ##  for version not using bigframe/biglist:
  ##        all.blips <<- if (overwrite) blips else NULL
  ##        all.scans <<- list()
  ##        all.tracks<<- list()
  num.tracks <<- 0
  num.blips <<- 0
  num.scans <<- 0
  drop.saved.state()
}

atid2tid = function(atid) {
  tracks[atid] %$0% index
}

## Gain has range [0, 1].  Often, the relevant values are tightly packed near 0 or 1,
## so we provide log units for the user interface.  These are the odds
## ratio in decibels, and have range [-Inf, Inf]

gain.to.log.units = function(g) 10 * log10(g/(1-g))
log.units.to.gain = function(u) {
  ## split into cases so that 10^u doesn't overflow for large positive u
  ifelse (u >= 0,
          1 / (1 + 10^(-u/10)),
          10^(u/10) / (1 + 10^(u/10)))
}


gui.tid2gid = function(tid) {
  ## given one or more track ids
  ## return the corresponding gid(s)
  which <- match(tid, gui.tracks$tid)
  which[!is.na(which)]
}

gui.tid2tkid = function(tid) {
  ## given one or more track ids
  ## return the corresponding tkid(s)
  which <- match(tid, gui.tracks$tid)
  gui.tracks$tkid[which[!is.na(which)]]
}


gui.gid2atid = function(gid) {
  ## given one or more track gui ids, return the
  ## active track id(s) associated
  ## with them.
  rv <- which(tracks %$0% index == gui.tracks$tid[gid])
  if(length(rv)) return (rv) else return (NULL)
}

gui.gid2tid = function(gid) {
  ## given one or more track gui ids, return the
  ## track id(s) associated
  ## with them.
  gui.tracks$tid[gid]
}

gui.atid2tkid = function(atid) {
  ## given one or more active track ids,
  ## return the corresponding tkids or NA
  gui.tracks$tkid[match(atid2tid(atid), gui.tracks$tid)]
}

gui.atid2gid = function(atid) {
  ## given one or more active track ids,
  ## return the corresponding gids or NA
  match(atid2tid(atid), gui.tracks$tid)
}

gui.gid2tkid = function(gid) {
  ## given one or more track gui ids, return the
  ## tk canvas item id(s) associated
  ## with them.
  gui.tracks$tkid[gid]
}

gui.tkid2gid = function(tkid) {
  ## given one or more tk canvas item ids, return the
  ## track id(s) associated
  ## with them.
  which <- match(tkid, gui.tracks$tkid)
  which[!is.na(which)]
}

gui.delete = function(gid=NULL, tid=NULL, tkid=NULL) {
  ## delete the gui track items from gui.tracks
  ## if gid is null, use tid;
  ## if both gid and tid are NULL, use tkid

  if (is.null(gid)) {
    if (is.null(tid))
      gid <- gui.tkid2gid(tkid)
    else
      gid <- gui.tid2gid(tid)
  }
  if (length(gid) > 0)
    gui.tracks <<- gui.tracks[- gid,]
}

gui.gid2track = function(gid) {
  ## return the track whose gui id is gid
  ## or NULL if none is found
  if (length(gid)==0)
    return(NULL)
  tid <- gui.gid2tid(gid)
  if (length(tid) == 0)
    return(NULL)
  if (tid <= length(all.tracks)) {
    tr <- all.tracks[[tid]]
    if (!is.null(tr)) {
      ## put the index number into the track,
      ## so we can later tell from whence it came
      tr$index <- tid
      return(tr)
    }
  }
  atid <- which(tracks %$0% index == tid)
  if (length(atid) == 1)
    return (tracks[[atid]])
  atid <- which(tracks.completed.in.preview %$0% index == tid)
  if (length(atid) == 1)
    return (tracks.completed.in.preview[[atid]])
  return(NULL)
}

gui.current.tktag = function() {
  ## return the tkid of the current canvas item
  return (tclint(GUI$plot, "find", "withtag", "current")[1])
}

gui.current.track.gid = function() {
  ## return the gid of the current canvas track item 
  tkid <- gui.current.tktag()
  if (!identical(tclchar(GUI$plot, "type", tkid),"line"))
    return(NULL)
  return(gui.tkid2gid(tkid))
}

gui.toggle.unselected.tracks = function() {
  ## The user has right-clicked on the selected track;
  ## Toggle visibility of all other tracks.

  if (nonselected.tracks.hidden) {
    tcl(GUI$plot, "itemconfigure", "hiddentrack", state="normal")
    tcl(GUI$plot, "dtag", "hiddentrack")
    nonselected.tracks.hidden <<- FALSE
  } else {
    tcl(GUI$plot, "itemconfigure", "track&&!selectedtrack", state="hidden")
    tcl(GUI$plot, "addtag", "hiddentrack", "withtag", "track&&!selectedtrack")
    nonselected.tracks.hidden <<- TRUE
  }
}

gui.toggle.circle = function(x, y=NULL) {
  ## The user has clicked on a circle representing a point on the currently
  ## selected track.  Toggle its appearance, and if it is highlighted,
  ## store its index in selected.point

  tkid <- if(is.null(y)) x else gui.current.tktag()
  
  ## figure out the index of this point within the track
  ## by examining its "indexXXX" tag (XXX is the numeric index)
  tags <- tclchar(GUI$plot, "gettags", tkid)
  for (i in 1:length(tags))
    if (substring(tags[i], 1, 6) == "pindex")
      break
  i <- as.integer(substring(tags[i], 7))
  if (is.null(selected.point) || i != selected.point) {
    if (!is.null(selected.point))
      tryCatch({
        tcl(GUI$plot, "itemconfigure", "selectedcircle", outline=circle.colour$normal, width=1)
        tcl(GUI$plot, "dtag", "selectedcircle")
      }, error = function(e) {})
    tcl(GUI$plot, "itemconfigure", tkid, outline=circle.colour$selected, width=2)
    tcl(GUI$plot, "addtag", "selectedcircle", "withtag", tkid)
    selected.point <<- i
  } else {
    tcl(GUI$plot, "itemconfigure", tkid, outline=circle.colour$highlighted, width=2)
    tcl(GUI$plot, "dtag", "selectedcircle")
    selected.point <<- NULL
  }
}

gui.highlight.circle = function(x, y=NULL) {
  tkid <- if (is.null(y)) x else gui.current.tktag()
  tcl(GUI$plot, "itemconfigure", tkid, outline=circle.colour$highlighted, width=2)
}

gui.unhighlight.circle = function(x, y) {
  tcl(GUI$plot, "itemconfigure", "point&&!selectedcircle", outline=circle.colour$normal, width=1)
}

gui.export.track.to.R = function() {
  ## export data for the currently highlighted track to the
  ## R variable "tr", and set this track as the one from whose
  ## active end the "gain" is calculated when mousing over the plot
  ## Create ovals at each point in the track, which when themselves activated,
  ## are the origin for the gain calculation.
  
  track <- gui.current.track()
  if (!is.null(track)) {
    if (is.null(gui.track.var))
      gui.put.cons("\n" %:% format.full.for.console(track) %:% "\n")
    else {
      dat <- structure(all.blips[track$points,], index=track$index)
      dat$t <- structure(dat$t, class="POSIXct", tzone=RSS$timezone)
      assign(gui.track.var, dat, .GlobalEnv)
    }
    tkid <- gui.gid2tkid(gui.tid2gid(track$index))
    coords <- tclreal(GUI$plot, "coords", tkid)
    if (length(coords) > 0) {
      atid <- gui.gid2atid(gui.current.track.gid())
      if (!is.null(atid) && (is.null(selected.atid) || selected.atid != atid)) {
        if (!is.null(selected.atid)) {
          ## unhighlight the old selected track
          gid <- gui.atid2gid(selected.atid)
          gui.unhighlight.track(gid)
          tcl(GUI$plot, "dtag", "selectedtrack")
          gid <- gui.atid2gid(atid)
          gui.highlight.track(gid)
        }
        tcl(GUI$plot, "addtag", "selectedtrack", "withtag", tkid)
        tcl(GUI$plot, "delete", "point")
        ovdelta <- circle.radius  * GUI$default.mpp / GUI$mpp
        for(i in 1:(length(coords)/2))
          tcl(GUI$plot, "create", "oval", coords[i * 2 - 1] - ovdelta, coords[i * 2] - ovdelta, coords[i * 2 - 1] + ovdelta, coords[i*2]+ovdelta, outline=circle.colour$normal, tags=c("zoom", "pan", "rotate_center", "point", paste("point", tkid, sep=""), paste("pindex", i, sep="")))
        tkid <- tclchar(GUI$plot, "find", "withtag", paste("pindex", i, sep=""))
        selected.point <<- NULL
        selected.atid <<- atid
        gui.highlight.circle(tkid)
        gui.toggle.circle(tkid)
        tcl(GUI$plot, "lower", "point", tkid)
        gui.set.clipboard(format.full.for.clipboard(track))
        if (!is.null(atid) && ! is.null(gui.track.obj.var)) {
          to <- as.list(tracks[[atid]])
          to$gain <- gain.to.log.units(to$gain)
          assign(gui.track.obj.var, to, .GlobalEnv)
        }
      } else {
        if (!is.null(atid)) {
          tcl(GUI$plot, "delete", "point")
          tcl(GUI$plot, "dtag", "selectedtrack")
          selected.atid <<- NULL
        }
      }
    }
  }
}

gui.highlight.track = function(x, y=NULL) {
  ## we keep track of the previous x and y coordinates at which
  ## this track was highlighted, because sometimes highlighting a track
  ## leads to an infinite sequence of Enter/Leave events on that track.
  ## (reported Tk bug #1660674:  there are some path/line combinations such that
  ## thickening the line and enabling arrows has the effect of making that
  ## point no longer "inside" the line, and so triggering a Leave event
  ## which, in turn, thins the line, making the point re-enter it, and
  ## so triggering an "Enter" event).
  ##
  ## Tk "bug" Update: in fact, the way Tk draws arrow heads, pixels at the ends
  ## of a line item are sometimes not part of the same line item with arrows.
  ## So if it's a bug, that's the nature of it (i.e. "adding arrows" does not
  ## imply "not removing pixels").

  if (!is.null(y)) {
    if (identical(prev.xy, as.numeric(c(x, y))))
      return()
    if (!is.null(selected.atid))
      return()
    gid <- gui.current.track.gid()
    prev.xy <<- as.numeric(c(x, y))
  } else {
    gid <- x
  }
  tkid <- gui.gid2tkid(gid)
  tcl(GUI$plot, "itemconfigure", tkid, arrow="last", fill=track.colour[TS.SELECTED], width=2)
}

gui.unhighlight.track = function(x, y=NULL) {
  if (!is.null(y)) {
    gid <- gui.current.track.gid()
    if (is.null(gid))
      return()
    if (!is.null(selected.atid))
      return()
  } else {
    gid <- x
  }
  tkid <- gui.gid2tkid(gid)
  if (length(tkid) == 0) {
    gui.print.cons("warning: canvas line item has no associated gui item:" %:% gid)
    return()
  }
  tcl(GUI$plot, "itemconfigure", tkid, arrow="none", fill=track.colour[gui.tracks$state[gid]], width=1)
}

  
gui.track.started = function(track, tid = track$index, state=track$state) {

  if (is.null(track))
    return()
  ## track is a new track object 

  np <- length(track$points)
  
  ## use all points in the new track so far to create a canvas line item
  ## return the gui id of the new canvas item, or NULL if it is not created
  ## we transpose and c() to get coordinates in X1, Y1, X2, Y2, ... order

  blip.coords <- c(t(GUI$tx.xy.to.plot(as.matrix(all.blips[track$points, COL.X:COL.Y]))))

  ## if this is a nascent track, replace coords with the nascent track symbol

  if (np == 1)
    blip.coords <- blip.coords + nascent.track.symbol
  tags <- c("track", "zoom", "rotate", "pan", if (state < TS.COMPLETE) "active" else NULL)

  ## record the gui track id in the gui slot of the track
  tkid <- tclint(GUI$plot, "create", "line",
                 blip.coords,
                 fill=track.colour[state],
                 state= if (np >= min.blips.to.show) "normal" else "hidden",
                 smooth=TRACKER$smooth.plotted.tracks,
                 tags=tags)
  last.time = all.blips[track$points[np], COL.T][[1]]
  
  if (length(tkid) > 0 && length(tid) > 0 && length(state) > 0 && length(last.time) > 0 && length(np) > 0) {
    gui.tracks <<- rbind(gui.tracks, data.frame(tkid=tkid,
                                                tid=tid,
                                                state=state,
                                                last.time=last.time,
                                                np=np))
  } else {
    cat(sprintf("gui.track.started error: %d, %d, %d, %f, %d\n", tkid, tid, state, last.time, np))
  }
  return(dim(gui.tracks)[1])
}       

gui.blip.added.to.track = function(track) {
  ## a new blip has been added to the track

  ## if this track was started with a single point,
  ## then its canvas item wasn't actually created; do so now.

  np <- length(track$points)
  gid <- gui.tid2gid(track$index)
  if (length(gid) == 0) {
    gid <- gui.track.started(track)
    tkid <- gui.gid2tkid(gid)
  } else {
    tkid <- gui.gid2tkid(gid)
  }
  if (!length(tkid))
    return()
  if (gui.tracks$state[gid] == TS.NASCENT) {
    ## delete the nascent track symbol
    tcl(GUI$plot, "dchar", tkid, 0, "end")
    ## change the colour and tags to those for an active track
    tcl(GUI$plot, "itemconfigure", tkid, fill=track.colour[TS.ACTIVE])
    ## set the state to active
    gui.tracks$state[gid] <<- TS.ACTIVE
    ## get the coordinates of all points
    blip.coords <- t(GUI$tx.xy.to.plot(as.matrix(all.blips[track$points, COL.X:COL.Y])))
  } else {
    blip.coords <- GUI$tx.xy.to.plot(as.matrix(all.blips[track$points[np], COL.X:COL.Y]))
  }
  tcl(GUI$plot, "insert", tkid, "end", paste(blip.coords, collapse=" "))
  gui.tracks$np[gid] <<- np
  ## show the track if it is sufficiently long
  if (np >= min.blips.to.show && !is.null(tkid))
    tcl(GUI$plot, "itemconfigure", tkid, state="normal")
  ## record the latest time of a blip in this track
  gui.tracks$last.time[gid] <- all.blips[track$points[np], COL.T][[1]]
}

gui.forall.tracks = function(tkid=NULL, tid=NULL, command="itemconfigure", args) {
  ## apply a command (default is "itemconfigure") to every track in the list tkid,
  ## or 
  if (is.null(tkid))
    tkid <- gui.tid2tkid(tid)
  if (length(tkid) > 0)
    .Tcl(paste("foreach {i} {", paste(tkid, collapse=" "), "} {", GUI$plot, command, "$i", args, "}", sep=" "))
}

gui.show.tracks = function(tkid=NULL, tid=NULL) {
  ## show tracks with track ids in tid
  gui.forall.tracks(tkid=tkid, tid=tid, args="-state normal")
}

gui.hide.tracks = function(tkid=NULL, tid=NULL) {
  ## hide tracks with track ids in tid
  gui.forall.tracks(tkid=tkid, tid=tid, args="-state hidden")
}

gui.drop.tracks = function(tkid=NULL, gid=NULL, tid=NULL, retain.dead.image = RSS$previewing) {
  ## the tracks indexed by tkid, or gid if tkid==NULL, or
  ## tid if both tkid and gid are NULL
  ## are about to be deleted
  if (is.null(tkid)) {
    if (is.null(gid))
      gid <- gui.tid2gid(tid)
    tkid <- gui.gid2tkid(gid)
  }
  if (!length(tkid))
    return()
  if (is.null(gid))
    gid <- gui.tkid2gid(tkid)
  if (retain.dead.image) {
    if (!is.null(gid)) {
      gui.tracks$state[gid] <<- TS.DISCARDED
      gui.forall.tracks(tkid=tkid, args="-fill " %:% track.colour[TS.DISCARDED])
    }
  } else {
    .Tcl(paste(GUI$plot, "delete", paste(tkid, collapse=" "), paste(paste("point", tkid, sep=""), collapse=" "), sep=" "))
    gui.delete(gid=gid, tkid=tkid)
  }
}

gui.drop.old.tracks = function(time.now, force=FALSE) {
  ## drop complete tracks that have been retained
  ## long enough
  if (force || sum(gui.tracks$state >= TS.COMPLETE) > 0)
    gui.drop.tracks(gid=which((time.now - gui.tracks$last.time > retain.complete.tracks.for) & (force | gui.tracks$state >= TS.COMPLETE)))
}

gui.drop.all.tracks = function(just.complete = TRUE) {
  ## all tracks are to be deleted
  tcl(GUI$plot, "delete", if (just.complete) "point||track&&!active" else "point||track")
  if (just.complete) {
    ## retain gui info for active tracks
    gui.tracks <<- gui.tracks[gui.tracks$state < TS.COMPLETE, ]
  } else {
    gui.tracks <<- gui.tracks[c(), ]
  }
}

gui.truncate.track.tail = function(atid, first.to.drop) {
  ## truncate the track specified by atid.
  ## The tail starting at index first.to.drop is removed from the track.
  ## If first.to.drop == 1, the entire track is dropped
  gid <- gui.tid2gid(tracks[[atid]]$index)
  if (!length(gid))
    return()
  if (first.to.drop == 1) {
    gui.drop.tracks(gid=gid)
  } else {
    tkid <- gui.gid2tkid(gid)
    ## if the track has actually been started in the gui,
    ## then tkid is not NA
    if (!is.na(tkid)) {
      ## n is twice the number of points in the line item
      n <- tclint(GUI$plot, "index", tkid, "end")
      tcl(GUI$plot, "dchar", tkid, 2 * (first.to.drop - 1), n - 1)
      ## as per the CAUTION, the track has not been removed from the
      ## active track list, nor has it had its tail dropped; we grab
      ## the time for the last point that won't be dropped
      gui.tracks$last.time[gid] <- all.blips[tracks[[atid]]$points[first.to.drop - 1], COL.T]
    }
  }
}

gui.end.tracks = function(tid, drop.from.tail = 0) {
  ## end the given tracks; currently just drops the last drop.from.tail
  ## points from the appropriate canvas line items
  ## Any tracks which aren't sufficiently long are also dropped.
  ## CAUTION:  this assumes that the tracks given by tid
  ## are still present in the global list "tracks" and that
  ## tracks[[i]]$index == tid[i] for all i in seq(along=tid)
  
  gid <- gui.tid2gid(tid)
  if(!length(gid))
    return()
  ## record which of these ending tracks are too short to keep
  short <- gui.tracks$np[gid] - drop.from.tail < track.min.blips
  gid.short <- gid[short]
  gid <- gid[!short]
  tkid <- gui.gid2tkid(gid)
  if (drop.from.tail > 0) {
    for (i in seq(along=gid)) {
      ## if the track has actually been started in the gui,
      ## then tkid[i] is not NA
      if (!is.na(tkid[i])) {
        ## n is twice the number of points in the line item
        n <- tclint(GUI$plot, "index", tkid[i], "end")
        tcl(GUI$plot, "dchar", tkid[i], n - 2 * drop.from.tail, n - 1)
        ## as per the CAUTION, the track has not been removed from the
        ## active track list, nor has it had its tail dropped; we grab
        ## the time for the last point that won't be dropped
        gui.tracks$last.time[gid[i]] <- all.blips[tracks[[i]]$points[n / 2 - drop.from.tail], COL.T]
      }
    }
  }
  ## all non-short tracks being ended are in the same state
  state <- if (RSS$previewing) TS.COMPLETED.IN.PREVIEW else TS.COMPLETE
  gui.forall.tracks(tkid=tkid, args="-fill " %:% track.colour[state])
  gui.forall.tracks(tkid=tkid, command="dtag", args="active")
  gui.tracks$state[gid] <<- state
  gui.tracks$np[gid] <<- gui.tracks$np[gid] - drop.from.tail
  ## delete the too-short tracks (we delay this until here in order not to
  ## invalidate the gids, since these are row indexes into gui.tracks)
  gui.tracks$state[gid.short] <<- TS.DISCARDED
  gui.drop.tracks(gid=gid.short)
}
  
gui.current.track = function() {
  return(gui.gid2track(gui.current.track.gid()))
}

gui.track.summary = function(track) {
  pts <- all.blips[track$points,]
  t1 <- pts$t[1]
  t2 <- tail(pts$t, 1)
  length <- sum(sqrt(diff(pts$x) ^ 2 + diff(pts$y) ^ 2 + diff(pts$z)^2))
  speed <- length / (t2 - t1) * 3.6
  class(t1) <- class(t2) <- "POSIXct"
  state <- if (is.null(track$state)) TS.COMPLETE else track$state
  index <- track$index
  active.index <- tid2atid(index) 
  np <- length(track$points)
  return(c(format.for.popup    (index, active.index, state, np, length, t1, t2),
           format.for.clipboard(index, active.index, state, np, length, t1, t2)
           ))
}

gui.set.retention.time = function(t.retain) {
  ## set a new value for the complete track retention time
  ## If the new retention time is less than the old one,
  ## delete any canvas track items which are too old.
  ## Otherwise, create canvas track items for complete tracks
  ## that are young enough to appear.

  if (t.retain == retain.complete.tracks.for)
    return()
  time.now <- rss.time.now()
  if (t.retain < retain.complete.tracks.for) {
    gui.drop.tracks(gid=which((time.now - gui.tracks$last.time > t.retain) & gui.tracks$state >= TS.COMPLETE), retain.dead.image=FALSE)
  } else {
    tid <- tracks.active.in.interval(time.now - t.retain, time.now - retain.complete.tracks.for)
    ## filter out those for which we already have canvas items
    tid <- tid[is.na(match(tid, gui.tracks$tid))]
    for (i in tid)
      gui.track.started(all.tracks[[i]], tid=i, state=TS.COMPLETE)
  }
  retain.complete.tracks.for <<- t.retain
}

gui.restore.state = function() {

  ## given that at least one preview has been done for this scan,
  ## restore gui.tracks and the actual canvas items to their
  ## pre-preview states
  ## If RSS$previewing is TRUE, we recreate copies of the original tracks,
  ## saving their new ids in gui.saved.state$shadow.tkid.
  ## If RSS$previewing is FALSE, we restore the original tracks.
  
  ## delete shadow canvas items and those corresponding to newly added tracks
  ## We don't just delete those elements of gui.tracks with active=TRUE
  ## because any tracks completed during the preview will have active=FALSE
  ## Note that we don't alter canvas items for complete tracks, except for those
  ## that were completed during the preview scan. (Which would still have their
  ## tkid in gui.saved.state$shadow.tkid).  This is because the user might have
  ## changed how far back complete scans are displayed.     
  
  gui.drop.tracks(tkid = unique(sort(c(gui.saved.state$shadow.tkid, gui.tracks$tkid[gui.tracks$state != TS.COMPLETE]))), retain.dead.image= FALSE)

  if (is.null(gui.saved.state)) return()

  if (RSS$previewing) {
    ## record the # of gui table rows
    n <- dim(gui.tracks)[1]

    ## create new "shadow" copies of saved active tracks
    lapply(saved.state$tracks, gui.track.started)

    ## record the tkids of these new shadow tracks
    gui.saved.state$shadow.tkid <<- gui.tracks$tkid %drop% n
  } else {
    ## restore the original saved active tracks
    gui.tracks <<- rbind(gui.tracks, gui.saved.state$tracks)

    ## re-show any of the original active track canvas items that
    ## are long enough under the current criterion
    
    gui.show.tracks(tkid=gui.saved.state$tracks$tkid[gui.saved.state$tracks$np >= min.blips.to.show])
  }
}

gui.save.state = function() {
  ## given we're about to preview this scan,
  ## save the state of gui.tracks and actual 
  ## canvas items.  We don't save the gui state of the complete tracks
  ## since this can be changed independently of previewing, e.g. by
  ## user control of retain.complete.tracks.for 

  if (!RSS$have.previewed.scan) {
    ## save the active part of the gui track table
    gui.saved.state$tracks <<- gui.tracks[gui.tracks$state < TS.COMPLETE, ]
    
    ## hide the active track canvas items
    gui.hide.tracks(tkid=gui.saved.state$tracks$tkid)
    
    ## remove active tracks from the tracks table
    gui.tracks <<- gui.tracks[gui.tracks$state >= TS.COMPLETE, ]
    
    ## create new "shadow" copies of active tracks and record their tkids
    
    n <- dim(gui.tracks)[1]
    lapply(tracks, gui.track.started)
    gui.saved.state$shadow.tkid <<- gui.tracks$tkid %drop% n
  }
}

gui.drop.saved.state = function() {
  ## delete any saved state information
  gui.saved.state <<- NULL
}

gui.end.source = function() {
  ## delete saved state information
  gui.tracks <<- gui.tracks[c(),]
  gui.drop.saved.state()
}

gui.setup = function () {
  ## make sure we're in pure tk plotting mode
  gui.set.plot.is.tk.image(TRUE)
  
  ## bind "track" plot canvas items 
  tcl(GUI$plot, "bind", "track", "<Button-1>", gui.export.track.to.R)
  tcl(GUI$plot, "bind", "track", "<Enter>", gui.highlight.track)
  tcl(GUI$plot, "bind", "track", "<Leave>", gui.unhighlight.track)
  tcl(GUI$plot, "bind", "point", "<Button-1>", gui.toggle.circle)
  tcl(GUI$plot, "bind", "point", "<Enter>", gui.highlight.circle)
  tcl(GUI$plot, "bind", "point", "<Leave>", gui.unhighlight.circle)
  tcl("bind", ".plot", "h", gui.toggle.unselected.tracks)
}

gui.takedown = function() {
  ## unbind "track" plot canvas items 
  tcl(GUI$plot, "bind", "track", "<Button-1>", "")
  tcl(GUI$plot, "bind", "track", "<Enter>",  "")
  tcl(GUI$plot, "bind", "track", "<Leave>",  "")
  tcl(GUI$plot, "bind", "point", "<Button-1>", "")
  tcl(GUI$plot, "bind", "point", "<Enter>",  "")
  tcl(GUI$plot, "bind", "point", "<Leave>",  "")
  tcl(GUI$plot, "bind", "selectedtrack", "<Button-3>", "")
}

set.min.blips.to.show = function(x) {
  ## set a new value for the minimum number of blips required
  ## for a track to be displayed
  if (x == min.blips.to.show)
    return()
  if (x > min.blips.to.show) {
    ## some tracks might have to be hidden
    gui.hide.tracks(tkid=gui.tracks$tkid[gui.tracks$np < x])
  } else {
    gui.show.tracks(tkid=gui.tracks$tkid[gui.tracks$np >= x])
  }
  min.blips.to.show <<- x
}

tid2atid = function(tid) {
  ## return the active track index corresponding
  ## to the given absolute track index; return NA
  ## if the track is not active
  atid <- which(tracks %$0% index == tid)
  return(if (length(atid) > 0) atid else NA)
}

tracks.active.in.interval = function(t1, t2) {
  ## return the list of absolute track indexes
  ## for those tracks which have at least one blip in
  ## the time interval from t1 to t2
  ## Times are given as (double) timestamps.

  time.now <- rss.time.now()

  ## process previous scans, looking for complete tracks which
  ## were active in that scan. We look at scans to find tracks
  ## because each scan knows what tracks are active in it.  This
  ## pulls in a few extra scans which we filter below.

  tid <- integer(0)
  if (num.tracks == 0)
    return(tid)
  ## estimate the latest scan in the interval, based on the current
  ## scan's timestamp and duration
  i.scan <- max(1, num.scans - floor((time.now - t2) / (RSS$scan.info$duration / 1000)))
  ## sharpen the estimate
  while (i.scan < num.scans && all.scans[[i.scan]]$timestamp <= t2)
    i.scan <- i.scan + 1

  ## get the tracks for these scans
  while (i.scan > 0) {
    sc <- all.scans[[i.scan]]
    if (sc$timestamp <= t1)
      break
    if (sc$timestamp <= t2)
      tid <- c(tid, sc$tracks)
    i.scan <- i.scan - 1
  }
  tid <- unique(sort(tid))
  ## for the memory-based list, we need to remove active tracks from the list
  tid <- tid[tid <= length(all.tracks)]
  ## filter these candidate tracks based on their first and last blip times

  return(tid[bool.lapply(tid,
                         function(x) {
                           p <- all.tracks[[x]]$points
                           (!is.null(p)) &&  ## the track is not empty
                           all.blips[p[1], COL.T] <= t2 && ## it begins no later than the end of the interval
                           all.blips[tail(p, 1), COL.T] >= t1 ## it ends no earlier than the start of the interval
                         })])
}

add.blip.to.track = function(i.blip, atid, expiry=0) {
  ## add the blip to the end of the given track object
###.if $DEBUG
  if (length(tracks[[atid]]$points) > 0)
    gui.print.cons("Adding " %:% i.blip %:% " to " %:% atid)
  else
    gui.print.cons("Error: called add.blip.to.track with blip " %:% i.blip %:% " and empty track " %:% atid) 
###.endif
  track <- tracks[[atid]]
  ## Note: "track" is an environment, so the following assignments
  ## are side effects of this function
  track$points[length(track$points) + 1] <- i.blip
  track$expiry <- expiry
  track$state <- TS.ACTIVE
  if (update.plot)
    gui.blip.added.to.track(track)
###.if $DEBUG
  gui.print.cons("Done")
###.endif
}

add.track.info = function(atid, gain, info) {
  
  track <- tracks[[atid]]
  ## Note: "track" is an environment, so the following assignments
  ## are side effects of this function
  track$gain <- c(track$gain, gain)
  track$info <- c(track$info, list(info))
}

start.new.track = function(i.blip, expiry=0) {
  ## start a new tracks with blip(s) and given properties
  ## return the track's index in list "tracks" (i.e. its atid)
###.if $DEBUG
  gui.print.cons("Starting " %:% (1+num.tracks) %:% " with " %:% paste(i.blip, collapse=","))
###.endif
  num.tracks <<- num.tracks + 1
  track <- strictenv(
                     state         = if (length(i.blip) == 1) TS.NASCENT else TS.ACTIVE,
                     index         = num.tracks,   ## absolute index of this track
                     points        = i.blip,       ## index(es) of blip(s) in this track
                     gain          = NA,           ## gain from adding point
                     info          = list(numeric(0)),   ## state info
                     expiry        = expiry,
                     LOCALIZE.FUNS = FALSE
                     )
  atid <- .Call("first_empty_slot", tracks)
  tracks <<- .Call("into_first_empty_slot", tracks, track)
  if (update.plot)
    gui.track.started(track)
  return (atid)
}

truncate.track.tail = function(atid, drop.blip.id) {

  ## drop from a track the tail beginning with the blip drop.blip.id
  ## If the specified track does not contain a blip with the specified id,
  ## nothing happens.

  ## record which tracks are being ended this scan
###.if $DEBUG
  gui.print.cons("Truncating from track atid=" %:% atid %:% " tail beginning at blip.id=" %:% drop.blip.id)
###.endif
  
  pt <- tracks[[atid]]$points
  i <- which(pt == drop.blip.id)
  if (length(i) == 0)
    ## blip is not in this track, so do nothing
    return()

  if (i == 1) {
    ## first point in track, so drop this track
    drop.tracks(atid=atid)
  } else {
    tracks[[atid]]$points <- tracks[[atid]]$points[seq(length=i-1)]
    tracks[[atid]]$info <- tracks[[atid]]$info[seq(length=i-1)]
    tracks[[atid]]$gain <- tracks[[atid]]$gain[seq(length=i-1)]
    if (update.plot)
      gui.truncate.track.tail(atid, i)
  }
###.if $DEBUG
  gui.print.cons("Done")
###.endif
}

end.tracks = function(atid=NULL, tid = NULL, drop.from.tail = 0) {

  ## mark tracks as complete.
  ## The tracks can be specified as a list of active track IDs (atid) or track IDs (tid)
  ## If atid is a logical vector, it is used to select the tracks from the active track list.
  ## drop.from.tail is the number of blips to remove from the end
  ## of each track

  if (is.logical(atid))
    atid <- which(atid)

  if (length(tid) == 0 && length(atid) == 0)
    return()

  ## record which tracks are being ended this scan
###.if $DEBUG
  gui.print.cons("Ending track(s) with atid=" %:% paste(atid, collapse=","))
###.endif
  if (is.null(tid))
    ## compute tid from atid
    tid <- tracks[atid]%$0%index
  if (is.null(atid)) {
    ## compute atid and tid from track
    atid <- match(tid, tracks %$0% index)
  }

  if (!is.null(selected.atid) && any(atid == selected.atid)) {
    selected.atid <<- NULL
    selected.point <<- NULL
  }
  for (i in atid) {
    pt <- tracks[[i]]$points[seq(length=max(0, length(tracks[[i]]$points) - drop.from.tail))]

    ## if we're not doing a preview, add this track to the list
    ## of completed ones if it is long enough

    short <- length(tracks[[i]]$points) < track.min.blips

    ## set the track state appropriately
    
    tracks[[i]]$state <<- state <- if (short) TS.DISCARDED else if (RSS$previewing) TS.COMPLETED.IN.PREVIEW else TS.COMPLETE
    if (state == TS.COMPLETE) {
      all.tracks[[tracks[[i]]$index]] <<- list(points=pt)
      if (track.hook.enabled)
        rss.call.hooks(RSS$TRACK_HOOK, all.blips[pt, ], tracks[[i]]$index)
    }
  }
  ## tell the gui to end the track, deleting any points required
  if (update.plot)
    gui.end.tracks(tid, drop.from.tail)

  if (RSS$previewing) {
    tracks.completed.in.preview <<- tracks[atid]
  }
  ## remove the tracks from the active tracks list
  tracks[atid] <<- list(NULL)
###.if $DEBUG
  gui.print.cons("Done")
###.endif
} 

end.expired.tracks = function(time.now) {
  ## end any track whose expiry time has been reached
  
  end.tracks(atid = time.now >= as.numeric(tracks %$0% expiry))
}

end.all.tracks = function() {
  ## end all tracks
  models[[current.model]]$end.all.tracks(TRACKER)
  atid <- .Call("which_slots_full", tracks)
  if (length(atid) > 0) {
    end.tracks(atid=atid)
    if (!is.null(all.blips) && inherits(all.blips, "bigframe")) {
      close(all.blips)
      close(all.tracks)
      close(all.scans)
      all.scans <<- NULL
      all.blips <<- NULL
      all.tracks <<- NULL
    }
    tracks <<- NULL
  }
  gui.drop.all.tracks(just.complete=FALSE)
  num.blips <<- num.tracks <<- num.scans <<- 0
}

drop.tracks = function(atid) {
  ## delete the track(s) with index atid
  ## from the active list, without adding them to
  ## all.tracks
  if (is.logical(atid))
    atid <- which(atid)
  if (length(atid) == 0)
    return()
  ## call gui handler first, since it needs track information
  if (update.plot)
    gui.drop.tracks(tid=tracks[atid]%$0%index)
  if (RSS$previewing)
    tracks[atid] %$$% state <<- TS.DISCARDED
  else
    tracks [atid] <<- list(NULL)
}

process.blips.from.one.scan = function() {
  ## add blips from this scan to saved collection
  ## then process them as a new batch of blips

  if (is.null(all.blips))
    return()
  
  if (num.new.blips <- RSS$num.blips)
    new.blips <- cbind(RSS$patches[RSS$blips,], scan=RSS$current.scan)
  else
    new.blips <- NULL
  ## add new blips to all.blips
  i.new <- as.integer(seq(from=as.integer(num.blips + 1), length=as.integer(num.new.blips)))
  if (num.new.blips > 0)
    all.blips[i.new,] <<- new.blips
###.if $DEBUG
  print ("about to call process.new.blips with " %:% num.new.blips %:% " and i.new = " %:% paste(i.new, collapse=","))
###.endif
  time.now <- rss.time.now()
  process.new.blips(new.blips, i.new, time.now)

  ## if not doing a preview, keep track of scans for which tracks
  ## have been built

  if (!RSS$previewing) {
    num.scans <<- num.scans + 1
    active.tracks <- tracks %$0% index
    active.tracks <- active.tracks[!is.na(active.tracks)]
    all.scans[[num.scans]] <<- list(timestamp=time.now, tracks=active.tracks, first.blip=num.blips + 1, num.blips=num.new.blips)
    num.blips <<- num.blips + num.new.blips
    ## remove canvas items for any tracks past their retention time
    if (update.plot)
      gui.drop.old.tracks(time.now)
  }
}

process.new.blips = function(blips, i.blips, time.now) {
  ## add blips in blips, whose absolute indexes are i.blips to tracks
  ## time.now is the current time, which must be at least max(blips$t[i.blips])

  ## tell the model about any new blips (even if there are none)
  
  models[[current.model]]$update(TRACKER, blips, i.blips, time.now, RSS$previewing)

}

restore.state = function() {
  ## given that the last scan processed was a preview
  ## restore all.blips, tracks, and all.tracks to their
  ## pre-preview states; do the same for the gui
  ## If RSS$previewing is TRUE, we make copies of the saved track objects
  ## so that the original saved ones are preserved for (eventual) non-preview
  ## processing.  Otherwise, we restore the original saved
  ## tracks themselves.

  if (RSS$previewing) {
    ## copy the original saved tracks
    tracks <<- vector("list", length(saved.state$tracks))
    for (i in seq(along=saved.state$tracks))
      if (!is.null(saved.state$tracks[[i]]))
        tracks[[i]] <<- c(strictenv(), saved.state$tracks[[i]])
      else
        tracks[i] <<- list(NULL)
  } else {
    ## use the original saved tracks
    tracks <<- saved.state$tracks
  }
  ## remove any added blips
  if (inherits(all.blips, "bigframe"))
    dim(all.blips) <- saved.state$num.blips ## just for reducing size on disk; num.blips is used
  ## to determine where in the frame new blips are added

  gui.restore.state()
}

save.state = function() {

  ## We're about to preview this scan.  If we have not yet
  ## previewed it, save the state of all.blips, tracks, and
  ## all.tracks and have the gui do the same
  
  ## record which track ids get completed in this scan,
  ## this is how we save state for all.tracks
  if (!RSS$have.previewed.scan) {
    ## This is the first preview, so save existing track state
    saved.state <<- list(num.blips=num.blips, tracks=vector("list", length(tracks)))
    ## copy the active tracks (which are strict envs and must be copied using c.strictenv)
    for (i in seq(along=tracks))
      if (!is.null(tracks[[i]]))
        saved.state$tracks[[i]] <<- c(strictenv(), tracks[[i]])
      else
        saved.state$tracks[i] <<- list(NULL)
  }
  ## get the GUI to do the same
  gui.save.state()
}

drop.saved.state = function() {
  ## erase any saved state
  saved.state <<- list( num.blips=0, tracks=NULL)
  gui.drop.saved.state()
}

hooks = list (

  DONE_SCAN = list (enabled = FALSE, read.only = TRUE,
    f = function(...) {
      ## register any changes to GUI params (e.g. canvas zoom, pan, rotation)
      gui.update.plot.parms()
      ## are we updating the plot?
      old.update.plot <- update.plot
      update.plot <<- RSS$have.gui && (GUI$plot.enabled || RSS$play.state < RSS$PS$PLAYING)
      if (update.plot && !old.update.plot)
        ## we've just turned plot update back on, so clean
        ## the plot of old tracks which might have missed being
        ## ended/dropped
        gui.drop.old.tracks(rss.time.now(), force=TRUE)
      ## is there a hook enabled for completed tracks?
      track.hook.enabled <<- rss.hook.is.active(RSS$TRACK_HOOK)
      if (last.scan.was.preview)
        restore.state()
      if (RSS$previewing) {
        if (!last.scan.was.preview)
          ## save copies of the current active tracks and their
          ## gui reflections
          save.state()
      } else {
        drop.saved.state()
      }
      
      ## end any expired tracks
      if (auto.expire.tracks)
        end.expired.tracks(rss.time.now())
      process.blips.from.one.scan()
      last.scan.was.preview <<- RSS$previewing
    }),

  PLOT_CURSOR_MOVED = list(enabled=FALSE, read.only = FALSE,
    f = function(plot.coords, spatial.coords, sample.coords, cell.coords) {
      track <- gui.current.track()
      rv <- NULL
      if (!is.null(track)) {
        rv <- gui.track.summary(track)
      }
      if (!is.null(selected.atid)) {
        if (selected.atid <= length(tracks)) {
          track <- tracks[[selected.atid]]
          if (!is.null(track)) {
            point <- if (!is.null(selected.point) && selected.point <= length(track$points)) selected.point else length(track$points)
            ## Adjust the time of the cursor point so that it appears to be coming from a different
            ## scan, namely the one holding the track point after the selected track point.
            ## If the selected track point is the last one in the track, then we use the time
            ## corresponding to one scan in the future.  That way, the scans shown to the user
            ## will match those actually calculated for the points in the track.

            ## number of scans difference between current scan and scan with selected track point
            d.scan <- as.numeric(round((spatial.coords$t - all.blips[track$points[point], COL.T]) / (RSS$scan.info$duration / 1000)))

            ## Calculate the gain, adjusting the time of the current point.
            
            gain <- models[[current.model]]$gain.from.track.to.point(
                          as.numeric(all.blips[track$points[point], c(COL.X, COL.Y, COL.Z, COL.T)]),
                          track$info[[point]],
                          c(spatial.coords$xyz, spatial.coords$t - RSS$scan.info$duration / 1000 * (d.scan - 1)),
                          d.scan == 1)
            gain.txt <- sprintf("Gain from tracks[[%d]]$points[%d] = %0.3f (log units)", selected.atid, point, gain.to.log.units(gain))
            if (length(rv) > 0)
              rv[1] <- paste(rv[1], gain.txt, sep="\n")
            else
              rv <- c(gain.txt, "")
          }
        }
      }
      return(rv)
    }),

  ONPLAY = list (enabled = FALSE, read.only = TRUE,
    f = function(...) {
      ## if the plugin is disabled, don't do anything
      if (!enabled)
        return()
      enable(TRUE)           
    }),

  ONPAUSE = list (enabled = FALSE, read.only = TRUE,
    f = function(...) {
      ## if the plugin is disabled, don't do anything
      if (!enabled)
        return()

      ## make sure all data is flushed to disk
      ## if the storage is file-based
      if (!is.null(all.blips) && inherits(all.blips, "bigframe")) {
        flush(all.blips)
        flush(all.scans)
        flush(all.tracks)
      }
    }),
  
  ONSTOP = list (enabled = FALSE, read.only = TRUE,
    f = function(...) {
      ## if the plugin is disabled, don't do anything
      if (!enabled)
        return()
      end.all.tracks()
      close.csv.file()
      drop.saved.state()
      last.scan.was.preview <<- TRUE
    }),

  END_SOURCE = list (enabled = TRUE, read.only = TRUE,
    f = function(...) {
      ## forget any saved state
      drop.saved.state()
      last.scan.was.preview <<- TRUE
      gui.end.source()
    }),

  TRACK = list (enabled = FALSE, read.only = TRUE,
    f = function(track, index, ...) {
      ## record this track to the CSV file
      ## This hook is only enabled when a valid CSV output file has
      ## been enabled.  Its handle is in csv.file
      cat(format.track.for.csv(track, index), file=csv.file)
      cat("\n", file=csv.file)
    })

  )  ## END OF HOOKS

## additional state variables for this plugin:

all.scans             = NULL
num.scans             = 0
all.blips             = NULL
num.blips             = 0
all.tracks            = NULL
num.tracks            = 0
tracks                = NULL
gui.tracks            = data.frame(tkid = integer(0), tid=integer(0), state=integer(0), last.time = double(0), np = integer(0))
saved.state           = NULL
gui.saved.state       = NULL
last.scan.was.preview = TRUE
track.hook.enabled    = FALSE
prev.xy               = NULL  # previous xy coordinates of a mouse Enter track event
                           # needed to work around a tk bug in which an infinite chain
                           # of Enter/Leave events is triggered by redrawing an Entered
                           ## track as thick with arrow
update.plot           = TRUE # is the plot updated?

tracks.completed.in.preview   = NULL
model.names                   = NULL        
model.filenames               = NULL
models                        = list()      ## the list of model objects
csv.file                      = NULL        ## the file connection for a .CSV output file
selected.atid                 = NULL        ## the atid of the most recently selected track 
selected.point                = NULL        ## the index (base 1) of the most recently selected track point
nonselected.tracks.hidden     = FALSE       ## are non-selected tracks hidden?      

## constants mapping column names to column indexes for the all.blips bigframe
## these must be identical to the indexes for the global RSS$patches

COL.X     = 1
COL.Y     = 2
COL.Z     = 3
COL.T     = 4
COL.NS    = 5
COL.AREA  = 6
COL.INT   = 7
COL.MAX   = 8
COL.ASPAN = 9
COL.RSPAN = 10
COL.PERIM = 11
COL.RANGE = 12
COL.SCAN  = 13

## constants for track state

TS.NASCENT              = 1
TS.ACTIVE               = 2
TS.COMPLETE             = 3
TS.COMPLETED.IN.PREVIEW = 4
TS.DISCARDED            = 5  ## only used for tracks discarded during a preview
TS.SELECTED             = 6  ## not used as an actual track state, but as an index into track.colour

## state names

state.names = c("Nascent", "Active", "Complete", "Completed (in preview)", "Discarded (in preview)", "Selected")
