##  svn $Id: gifmovie.plugin.R 629 2010-07-30 20:21:58Z john $
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

##      The GIFMOVIE radR plugin

about = function() {
  ## a function returning a description and the current state of the plugin.
  "gifmovie captures frames and creates an animated .GIF from them.\n\
Capture only happens when the plugin is enabled.  There are two capture modes:\
normally the plugin captures one frame per scan, but when you start continuous\
capture, the plugin captures frames at the fixed time interval specified in\
the controls, regardless of radR activity.\n\
You can also manually 'Capture the current frame' from the menu.
Each frame is saved in a folder as a .GIF file with a reduced palette\
or as a raw .RGB file.\
When you hit 'Stop' (in one-per-scan capture mode) or switch from continuous \
to one-per-scan capture mode, all frames captured are merged into a .GIF movie.\
You can also cancel continuous capture mode without making a movie.\
\
This plugin requires the free gifsicle program, with a radR patch\
for capturing an RGB window as a .GIF or .rgb file.\
\
gifsicle is by Eddie Kohler < ekohler AT gmail DOT com >.\
http://www.cs.ucla.edu/~kohler/\
\
For gifsicle source code, see http://www.lcdf.org/gifsicle/" %:%
  if (capture.mode == 2) sprintf("\n\nCapturing a frame every %.3f seconds.\n", capture.interval / 100)
}


load = function() {
  ## try to find the configured or system default versions of capture programs
  if (check.capture.progs) {
    if (!identical(system(capture.progs[1] %:% " --version", intern=TRUE)[1], "LCDF Gifsicle 1.52")) {
      if (!identical(system("gifsicle --version", intern=TRUE)[1], "LCDF Gifsicle 1.52")) {
        enable(FALSE)
        rss.gui("POPUP_DIALOG", "gifsicle is missing", "The gifmovie plugin requires a patched version of gifsicle-1.52\nwhich is provided with radR.  It should be in the plugins/gifmovie directory.\n\nThis plugin will be disabled now.", buttons=c("Ok"))
      } else {
        capture.progs[1] <<- "gifsicle"
      }
    }

    ## try to find the configured or system default version of win2rgb
    if (!identical(system(capture.progs[2], intern=TRUE)[1], "Usage: wintorgb WINID -o outfile.rgb")) {
      if (!identical(system("win2rgb", intern=TRUE)[1], "Usage: wintorgb WINID -o outfile.rgb")) {
        enable(FALSE)
        rss.gui("POPUP_DIALOG", "win2rgb is missing", "The gifmovie plugin requires win2rgb\nwhich is provided with radR.  It should be in the plugins/gifmovie directory.\n\nThis plugin will be disabled now.", buttons=c("Ok"))
      } else {
        capture.progs[2] <<- "win2rgb"
      }
    }
  }

}

unload = function(save.config) {
  ## do whatever is required to shut down this
  ## plugin and free resources
  ## Changes to it have no effect, and the return
  ## value of this function is ignored.
}

enable = function(enab) {
  ## enable or disable the plugin, according to the value of "enable"
  ## This function illustrates the default behaviour.  If your plugin
  ## does only this, you can omit the definition of "enable".

  enabled <<- enab
  rss.enable.hook("ONPLAY", name, enab)
  rss.enable.hook("ONSTOP", name, enab)
}

get.menus = function() {
  list(
       plugin = list(
         "Choose the .GIF movie filename ..." =
         list ("file",
               type = "save",
               title = "Choose the .GIF movie filename", 
               file.types = file.types.list,
               on.set = function(f) {
                 movie.filename <<- f
               },
               init.file = movie.filename
               ),
         "Choose output folder..." = list (
           "file",
           type = "open.dir",
           title = "Choose a folder for saving .GIF frames",
           init.file = function() GIFMOVIE$frame.dir,
           on.set = function(x) if (nchar(x) > 0) frame.dir <<- x
           ),
         "---",
         "When frames are captured:",
         c(list ("choose.one",
                 on.set = function(x) {
                   capture.mode <<- x
                   if (x == 1)
                     merge.frames()
                   else
                     grab.this.plot()
                 },
                 set.or.get = "set.frame.capture.menu.choice"
                 ),
           structure(capture.mode == 1:length(capture.modes), names=capture.modes)
           ),
         "Cancel continuous frame capture now" = function() {
           capture.mode <<- 1
           set.frame.capture.menu.choice(1)
           merge.frames(delete.only=TRUE)
         },
         "Capture the current scan now" = function() grab.this.plot(TRUE),
         "---",
         "What is captured:",
         c(list ("choose.one",
                 on.set = function(x) window.style <<- x),
           structure(window.style == 1:length(window.styles), names=window.styles)
           ),
         "---",
         "Format of the individual frame files:",
         c(list ("choose.one",
                 on.set = function(x) frame.file.format <<- x),
           structure(frame.file.format == 1:length(frame.file.formats), names=frame.file.formats)
           ),
         "---",
         c(list ("choose.any",
                 on.set = function(x, v) delete.frames.after.merging <<- v),
           structure(delete.frames.after.merging, names="Delete .GIF frames after merging into a movie")
           ),
         c(list ("choose.any",
                 on.set = function(x, v) loop.movie <<- v),
           structure(loop.movie, names="Set loop flag so movie repeats when played")
           ),
         "---",
         "Delete all frames without making movie" = function() {merge.frames(delete.only=TRUE);},
         "View movie now..." = function() {
             if (length(frames.captured) > 0)
               merge.frames()
             view.movie()
           },
         list (frame.delay = "gauge",
               label = "delay between frames in gif movie (1/100ths of a second)",
               range = c(0, 100000000),
               increment = 1,
               value = frame.delay,
               on.set = function(x) { frame.delay <<- x}
               ),
         list (capture.interval = "gauge",
               label = "interval between periodic captures (1/100ths of a second)",
               range = c(1, 100000000),
               increment = 1,
               value = capture.interval,
               on.set = function(x) { capture.interval <<- x}
               )
         )
       )
}

merge.frames = function(delete.only=FALSE, wait=TRUE) {
  ## call gifsicle to coalesce the frames
  if (length(frames.captured) && !rss.get.and.set("lock", TRUE, GIFMOVIE)) {
    while (capture.pending) {
      ## maybe a bit dangerous, but capture.pending can only be TRUE if tcl("after", ...) was successful
      ## and setting it to FALSE is the first thing grab.this.plot does.
      Sys.sleep(capture.interval / 100)
    }
    frames <- file.path(frame.dir, sprintf(frame.filename.template[frame.file.format], frames.captured))
    if (!delete.only) {
      if (wait)
        rss.gui(POPUP_MESSAGEBOX, "Merging images", "The gifmovie plugin is using gifsicle to merge " %:% length(frames) %:% " frames.\nThis may take some time...", id="merging gifs")
      system(paste(capture.progs[1], "-O -d", frame.delay, if (loop.movie) "-l", paste('"', frames, '"', sep="", collapse=" "), "-o", movie.filename), wait=wait)
      if (wait)
        rss.gui(DELETE_MESSAGEBOX, "merging gifs")
    }
    if (delete.frames.after.merging && (delete.only || (file.exists(movie.filename) && file.info(movie.filename)$size > 0)))
      unlink(frames)
    lock <<- FALSE
    frames.captured <<- c()
  }
}

get.dumpwin.id = function(mode) {
  switch(mode,
         tclchar("winfo", "id", ".plot"),  ## just the plot window contents
         tclchar("wm", "frame", ".plot"),  ## the plot window plus frame including titlebar
         "0x0")
}

get.movie.filename = function(f = movie.filename) {
  ## get the movie filename into a full path
  ## huge kludge to deal with stupid windows drive letters
  if (! length(grep("^([a-zA-Z]:)?/", f, perl=TRUE)))
    f <- file.path(getwd(), f)
  return(f)
}

grab.this.plot = function(force = FALSE) {
  if (!enabled && !force)
    return()
  capture.pending <<- FALSE
  if (!rss.get.and.set("lock", TRUE, GIFMOVIE)) {
    if (!file.exists(frame.dir))
      dir.create(frame.dir, recursive=TRUE)
    last.frame <<- last.frame + 1
    if (last.frame > skip.first.scans) {
      system(paste(capture.progs[frame.file.format], get.dumpwin.id(window.style), "-o", paste('"', file.path(frame.dir, sprintf(frame.filename.template[frame.file.format], last.frame)), '"', sep="")))
      frames.captured <<- c(frames.captured, last.frame)
    }
    if (last.frame == max.scans) {
      rss.disable.hook("DONE_SCAN", name)
      merge.frames()
    } else {
      if (capture.mode == 2) {
        tcl("after", as.integer(capture.interval * 10), grab.this.plot)
        capture.pending <<- TRUE
      }
    }
    lock <<- FALSE
  }
}

hooks = list(
  ## a list of hooks.
  ## Names in the list are hook types and must be in names(RSS$hooks)
  ## each item in the list is a list with these fields:
  ## $enabled:  the hook is enabled at plugin load time
  ## $read.only: the hook only reads radR data, but doesn't modify it
  ## $f: the actual hook function, which receives the parameters
  ##     appropriate to its type.  FIXME: document where these are defined
  
  ONPLAY = list( enabled = TRUE, read.only = TRUE,
    f = function() {
      if (enabled) {
        ## save the information for this scan
        rss.enable.hook("DONE_SCAN", name)
        last.frame <<- 1
      }
    }),
  
  ONSTOP = list( enabled = TRUE, read.only = TRUE,
    f = function() {
      if (enabled) {
        ## save the information for this scan
        rss.disable.hook("DONE_SCAN", name)
        if (capture.mode == 1)
          merge.frames()
      }
    }),

  DONE_SCAN = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      if (capture.mode == 1) grab.this.plot()
    })
  )


## state variable:

last.frame = 1

file.types.list = list(".gif" = "GIF files", ".*" = "All files")

## a lock to prevent interleaved calls to merge.frames or grab.this.plot
lock = FALSE

## a variable to indicate when a periodic capture event is pending
## so we can wait for the last one before creating a movie

capture.pending = FALSE

## a vector to house the frame numbers we've actually captured
frames.captured = c()
