##  svn $Id: gui.R 800 2011-06-18 15:03:25Z john $
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

## The radR GUI.

## FIXME: This is a HORRIBLE MESS!  We thrash between Tcl/Tk and R
## far too much.  We should be using double ported Tcl objects,
## and defining autonomous procedures in Tcl.  We should avoid calling
## tcl() because of its serious parameter conversion overhead.

## The base system calls this via the GUI$eventlist, defined at
## the bottom of this file.  Functions here may also need to call the
## base system (e.g. to set a GUI-adjusted parameter value), but that
## interface is not exported per se.

gui.plugin.loader.list <- function() {
  ## create the menu list for loading available plugins
  ## plugin.items is a list whose names are the plugin.labels
  ## and whose items are the (short) plugin names.
  plugin.items <- names(RSS$available.plugins)
  names(plugin.items) <- sapply(RSS$available.plugins, function(p)paste(p$name, p$plugin.label, sep=": "))
  return (list(c(list(
                 "do.one",
                 on.do = rss.load.plugin),
            plugin.items
            )
          ))
}

gui.create.plugin.menu <- function() {
  ## create the menu for dealing with plugins
  lst <- list("Plugins loaded:",
               "---",
               "Load a plugin" = c(list(
                 load="menu"),
                 gui.plugin.loader.list()
                 )
              )
  gui.menu.from.list(".cmd.plugins", title="Plugins", lst)
  TCL(".cmd.plugins entryconfigure 1 -background white -state disabled")
  TCL(".cmd.plugins configure -disabledforeground black")
}


gui.plugin.loaded <- function(plugin, manually=FALSE) {
  ## Create the plugin meta and plugin-specified menus.
  ## The meta menu, with entries
  ## for enabling, disabling, enabling-on R load, and About,
  ## goes under the plugins menu.

  ## The plugin-specified main menu goes wherever its $tcl.menu.path specifies.
  ## The sources and sinks menus, if any, are added to the "From:" and "To: 
  ## buttons on the player.

  ## get the list of menus for this plugin
  if ("get.menus" %in% plugin)
    menus <- plugin$get.menus()
  else
    menus <- NULL

  ## build the list defining the plugin's metamenu:
  always.on <- "always.on" %in% plugin && plugin$always.on
  always.enabled <- always.on || ("always.enabled" %in% plugin && plugin$always.enabled)
  lst <- c( list(),
           if (!always.enabled) {
             list(
                  list(
                       type="choose.any",
                       on.set = rss.make.closure(
                         function(which, enable) {
                           rss.enable.plugin(name, enable)
                         },
                         list(name = plugin$name)
                         ),
                       Enabled = plugin$enabled
                       )
                  )
             },
           if (!always.on) {
             list(
                  list(
                       type="choose.any",
                       on.set = rss.make.closure(
                         function(which, enable) {
                           if (enable && !name %in% RSS$plugins.to.load)
                             RSS$plugins.to.load <- c(RSS$plugins.to.load, name)
                           else if (!enable && name %in% RSS$plugins.to.load)
                             RSS$plugins.to.load <- RSS$plugins.to.load[RSS$plugins.to.load != name]
                         },
                         list(name = plugin$name)
                         ),
                       "Load this plugin when radR starts" = plugin$name %in% RSS$plugins.to.load
                       )
                  )
           },
           list(
                "About..." = function() {
                  me <- get(toupper(plugin$name), .GlobalEnv)
                  name <- ".about" %:% me$name
                  .Tcl("toplevel " %:% name %:% "; wm withdraw " %:% name)
                  tcl("wm", "iconbitmap", name, GUI$application.icon)
                  tcl("wm", "title", name, "About radR plugin " %:% me$name)
                  tcl("text", name %:% ".txt", font=GUI$console.font)
                  tcl(name %:% ".txt", "insert", "end", paste(me$about(), collapse="\n"))
                  tcl("button", name %:% ".btn", text="Ok", width=10,
                      command=function() {
                        tcl("destroy", name)
                      })
                  tcl("wm", "geometry", name, "+100+100")
                  tcl("pack", name %:% ".txt", fill="both", side="top")
                  tcl("pack", name %:% ".btn", side="bottom")
                  tcl("wm", "deiconify", name)
                  tcl("raise", name)
                  tcl("focus", name %:% ".btn")
                  tcl("focus", name)
                },
                "---"
                ),
           ## possible extra entries for the plugin menu
           menus$plugin,
           "---",
           if (!always.on) {
             list(
                  "Unload this plugin" = function() {
                    n <- plugin$name
                    rss.unload.plugin(n, save.config=FALSE)
                  })
           },
           "Reload this plugin" = function() {
             n <- plugin$name
             rss.unload.plugin(n, save.config=FALSE)
             rss.load.plugin(n)
           },
           "Save this plugin's configuration" = function() {
             conf.name <- file.path(dirname(plugin$plugin.file), plugin$name)
             rss.save.config(plugin, conf.name)
           }
           )

  meta.menu.name <- ".cmd.plugins." %:% plugin$name
  meta.menu <- gui.menu.from.list(meta.menu.name, lst, title=plugin$name, env=plugin)
  GUI$menu.title.to.tcl.path[[plugin$name]] <- meta.menu.name
  tcl(".cmd.plugins", "insert", Tclint(".cmd.plugins index end") - 1, "cascade", menu=meta.menu.name, label=plugin$name)

  ## get the list of menus
  for (m in names(menus)) {
    if (m != "plugin" && m %in% names(GUI$menu.tcl.name)) {
      plugin.menu.name <- gui.add.menu(GUI$menu.tcl.name[[m]], menus[[m]]$titles)
      gui.menu.from.list(plugin.menu.name, menus[[m]]$menu, title=tail(menus[[m]]$titles, 1), env=plugin)
    }
  }
  ## remove this "available" plugin from the menu
  ## (we add the wildcard to the name because its entire label is there, not just the short name)

  tcl(".cmd.plugins.load", "delete", plugin$name %:% ":*")

  ## show the menu, if configured to

  if (manually && GUI$show.plugin.menu.on.manual.load) {
    ## don't leave the menu at current mouse location;
    ## there may be enough of a delay in loading the plugin
    ## that this is counterintuitive.
    GUI$skip.relocate.tearoff <- TRUE
    tcl(meta.menu.name, "invoke", 0)
    rss.gui(UPDATE_GUI)
  }
}

gui.no.source <- function() {
  ## configure the UI to match the no-source situation
  for (btn in c(".play.play", ".play.stop", ".play.pause")) {
    tcl(btn, "configure", state="disabled")
    tcl(btn, "deselect")
  }
  TCL(".play.play1 configure -state disabled")
  TCL(".play.toend configure -state disabled")
  TCL(".play.tostart configure -state disabled")
  
  ## empty the playlist and re-insert into the play window
  TCL("destroy .play.playlist")
  gui.create.menu.button(".play.playlist", c(), null.label="No table of contents found.")
  TCL("place configure .play.playlist -x 0 -relwidth 0.5 -width -4 -rely 0.333")
  TCL(".play.sourcelabel configure -text {(select a source)} -anchor w")
  gui.skip.event(".play.slider", "<<PLAY_SLIDER_MOVED>>")
  gui.reset.slider(TRUE)
  TCL(".play.slider configure -state disabled")
  TCL(".play.source configure -state normal")
  TCL(".play.sink configure -state normal")
}

gui.no.sink <- function() {
  ## configure the UI to match the no-source situation
  TCL(".play.record deselect")
  TCL(".play.record configure -state disabled")
  TCL(".play.sinklabel configure -text {(select a destination)} -anchor w")
}

gui.plugin.unloaded <- function(plugin) {
  ## delete the plugin meta menu
  tcl("destroy", ".cmd.plugins." %:% plugin$name)
  ## delete the plugin meta menu entry from the plugins menu
  tcl(".cmd.plugins", "delete", plugin$name)
  ## delete the plugin-specified menus
  if ("get.menus" %in% plugin) {
    menus <- plugin$get.menus()
    for (m in names(menus))
      if (m %in% names(GUI$menu.tcl.name))
        gui.drop.menu (GUI$menu.tcl.name[[m]], menus[[m]]$titles)
  }
  ## delete named file port entries corresponding to this plugin
  ## Note: this depends on the entries being prefixed by the plugin name
  for (m in c(".play.source.menu", ".play.sink.menu"))
    for (i in rev(gui.menu.inds(m)))
      if (substr(tclchar(m, "entrycget", i, "-label"), 1, nchar(plugin$name))[1] == plugin$name)
        tcl(m, "delete", i)
    
  ## recreate the plugin-loader list
  tcl("destroy", ".cmd.plugins.load")
  gui.menu.from.list(".cmd.plugins.load", title="Load a plugin", gui.plugin.loader.list())

  ## if the current source was provided by this plugin, reset to no source
  if (identical(class(RSS$source)[1], plugin$name)) {
    gui.no.source()
  }
  if (identical(class(RSS$sink)[1], plugin$name)) {
  ## FIXME: do the same for the sinks
  }
}

gui.create.command.menu <- function() {
  ## create the command menu that is invoked by right-clicking in the plot window
  
  if ("1" == TCL("winfo exists .cmd"))
    return(TRUE)

  ## create the menu
  gui.menu.from.list(".cmd",
                     list(
                          " (close menu) " = function() {},
                          "Show R console" = function() {
                            TCL("wm state .cons normal
                            raise .cons
                            focus .cons.text
                            event generate .cons.text <Button-1> -x 10 -y 10
                            .cons.text mark set insert end
                            .cons.text yview end")
                          },
                          "Source an R script..." = function() {
                              file <- rss.gui("FILE_DIALOG",
                                              mode = "open.one",
                                              title = "Choose a file to source()",
                                              types = list(".R" = "R Script", ".*" = "All files"),
                                              init.file = GUI$default.scripts.folder)
                              if (length(file) > 0 && !file.info(file[1])$isdir) {
                                GUI$default.scripts.folder <- dirname(file)
                                tryCatch(source(file),
                                         error = function(e) {
                                           rss.gui("POPUP_MESSAGEBOX",
                                                   "Error sourcing R script",
                                                   sprintf("The file %s generated this error:\n%s\n", file, as.character(e)))
                                         })
                              }
                            },
                          "Quit radR" = function()RSS$time.to.go <- TRUE,
                          "---",
                          "View" = list (
                            "menu",
                            list ("do.one",
                                  on.do = function(x) {
                                    tcl("wm", "deiconify", x)
                                    tcl("raise", x)
                                  },
                                  "Blip processing" = ".blip",
                                  "Console" = ".cons",
                                  "Display options" = ".pctl",
                                  "Plot" =".plot",
                                  "Player" = ".play"
                                  )
                            ),
                          "Plugins" = ".cmd.plugins"
                          ),
                     title = "radR menu")
}

gui.set.plot.window <- function(hwnd) {
  # set the plotting window
  # tkwin is the tcl object representing a window
  .Call("set_plot_window", as.integer(hwnd), PACKAGE=GUI.PACKAGE)
}

gui.have.plot.window <- function() {
  .Call("have_plot_window", PACKAGE=GUI.PACKAGE)
}

gui.save.config <- function() {

  ## only called on exit
  ## save relevant properties of open windows to
  ## restore their state on next run of radR

  ## regular toplevel windows:
  ## record the geometry and state

  plot.win.title <- GUI$windows$.plot$title
  GUI$windows <- list()

  ## a list of all toplevel windows, with mapped windows
  ## in their z-order
  win.list <- unique(c(rev(tclchar("wm", "stackorder", ".")), tclchar("winfo", "children", ".")))

  z <- 1
  ## save current window manager details of these windows
  for (w in win.list) {
    ## if this is the toplevel window for a menu, ignore it
    if (tclchar("winfo", "class", w) == "Menu" && substring(w, 1, 8) != ".tearoff")
      next
    ## if this window is withdrawn, don't save state for it (?)
    state <- tclchar("wm", "state", w)
    if (state == "withdrawn")
      next
    ## special case the .plot window, which uses a dynamic
    ## title.  We want to resave the original title.
    if (w == ".plot")
      title <- plot.win.title
    else
      title <- paste(tclchar("wm", "title", w), collapse=" ")

    ## we save tearoff window names as something that lets us reconstruct them,
    ## rather than the unhelpful ".tearoffXX" names which don't tell us what
    ## the window actually is
    shortname <- if (substring(w, 1, 8) == ".tearoff") title else w

    ## add this open window to the list
    GUI$windows[[shortname]] <- list (
                                      title = title,
                                      geometry = tclchar("wm", "geometry", w),
                                      state = state,
                                      z = z
                                      )
    z <- z + 1
  }
  rss.save.config(GUI, name="gui/gui")
}

gui.confirm.quit <- function() {
  TCL('tk_messageBox -message "Save parameters and GUI configuration?" -title "Quit radR" -type yesnocancel')
}

gui.repaint.plot.window <- function(coords) {
  .Call("repaint_plot_window", coords, PACKAGE=GUI.PACKAGE)
}

gui.do.rotate <- function(dpulse) {
  rot <- gui.parm(north.angle)
  if (is.null(RSS$scan.info$pulses)) {
    dtheta <- GUI$default.rotation.step
  } else {
    ## round the new angle to the nearest pulse-wise angle so that the
    ## graphics cursor locator works correctly
    dtheta <- 360 / RSS$scan.info$pulses
  }
  rot <- (rot + dtheta * dpulse) %% 360
  gui.new.parm(north.angle, rot)
  gui.update.plot.window()
  rss.call.hooks(RSS$PLOT_ROTATED_HOOK, GUI$plot.origin, dtheta * dpulse)
}

gui.do.zoom <- function(f, relative=TRUE) {
  old.mpp <- gui.parm(mpp)
  if (relative) {
    if (f != 0) {
      mpp <- (old.mpp / GUI$zoom.factor ^(f/2))
    } else {
      mpp <- GUI$default.mpp
    }
  } else {
    mpp <- f
  }

  gui.new.parm(mpp, mpp)
  
  ## adjust the plot origin so that the same data point is in the
  ## centre of the window

  gui.set.plot.origin(gui.parm(plot.dim) / 2 + (gui.parm(plot.origin) -
                                          gui.parm(plot.dim) / 2) * old.mpp / mpp)
  rss.call.hooks(RSS$PLOT_ZOOMED_HOOK, mpp / old.mpp)
}

gui.set.plot.origin <- function(coords, absolute=TRUE) {
  if (absolute) {
    gui.new.parm(plot.origin, coords)
  } else {
    gui.new.parm(plot.origin, coords <- gui.parm(plot.origin) + coords)
  }
  gui.update.plot.window()
  rss.call.hooks(RSS$PLOT_PANNED_HOOK, coords)
}

gui.set.plot.dim <- function(dim = gui.parm(plot.dim)) {
  gui.new.parm(plot.dim, dim)
}

gui.put.cons <- function(txt, type="output", prompt=GUI$console.prompt.string, is.output=TRUE) {
  ## if is.output==TRUE: append txt to the console in the given colour, followed by prompt
  ## if is.output==FALSE: replace the current input line with the given prompt in the given colour, followed
  ##                      by txt in the default foreground colour, preserving the cursor position in the line

  t <- GUI$cons
  if (is.output) {
    start.pos <- tcl(t, "index", "end-1c")
    tcl(t, "insert", "end", as.character(txt))
    end.pos <- tcl(t, "index", "end-1c")
    tcl(t, "insert", "end", prompt)
    tcl(t, "tag", "add", "prompt", end.pos, paste(end.pos, "+", nchar(prompt), "c", sep=""))
    tcl(t, "mark", "set", "insert", "end-1c")
  } else {
    ## replace the current line with the prompt followed by the string
    ## preserving the cursor position if possible
    curs.pos <- tcl(t, "index", "insert")
    if (is.na(prev.prompt <- tclchar(t, "tag", "prevrange", "prompt", "insert")[2]))
      prev.prompt <- "1.0"
    if (is.na(next.prompt <- tclchar(t, "tag", "nextrange", "prompt", "insert")[1]))
      next.prompt <- "end-1c"

    tcl(t, "delete", prev.prompt, next.prompt)
    if (prev.prompt == "1.0") {
      tcl(t, "insert", "1.0", prompt)
      prev.prompt <- paste("1", nchar(prompt), sep=".")
      curs.pos <- paste(curs.pos, "+", nchar(prompt), "c", sep="")
    }
    start.pos <- prev.prompt
    tcl(t, "insert", "end", as.character(txt))
    end.pos <- tcl(t, "index", "end-1c")
    tcl(t, "mark", "set", "insert", start.pos)
  }
  tcl(t, "tag", "add", type, start.pos, end.pos)
  tcl(t, "see", "insert")
}

gui.print.cons <- function(x) {
  x <- withVisible(x)
  if (x$visible)
    gui.put.cons("\n" %:% paste(capture.output(print(x$value)), collapse="\n") %:% "\n")
  else
    gui.put.cons("\n")
}

gui.get.cons <- function(append.to.history=TRUE) {
  ## get the user's input line from the console
  ## Be smart: if the cursor position is on a later line than the last one where we
  ## pasted a prompt, use all the text from that last position to the
  if (is.na(prev.prompt <- tclchar(GUI$cons, "tag", "prevrange", "prompt", "insert")[2]))
    prev.prompt <- "1.0"
  if (is.na(next.prompt <- tclchar(GUI$cons, "tag", "nextrange", "prompt", "insert")[1]))
    next.prompt <- "end-1c"
  
  text <- tclvalue(tcl(GUI$cons, "get", prev.prompt, next.prompt))
                   
  if (append.to.history) 
    gui.append.to.history(text)
  return(text)
}

gui.eval.cons <- function(code) {
  ## evaluate code as an R expression, and send output to the console
  ## output from assignments is suppressed

  try.res <- rss.try({
    if (0 == length(grep("^[ \t]*$", code))) {
      options(width = as.numeric(TCL(".cons.text cget -width")))
      code <- gsub("^[ \t]*\\?(.*)$", "help(\"\\1\\\")", code)
      expr <- parse(text=code)
        gui.print.cons(eval(expr, .GlobalEnv))
    } else {
      gui.put.cons("\n")
    }
  })
  
  if (identical("try-error", class(try.res))) {
    err <- geterrmessage()
    if (nchar(err) > 0 && exists("gui.put.cons"))
      gui.put.cons("\n" %:% gsub("^[^:]*: ", "", err), type="error")
  }
}
          
gui.append.to.history <- function(txt) {
  ## add the line to the console history
  if (nchar(txt) > 0 && (length(GUI$console.history) == 0 || !identical(tail(GUI$console.history, 1), txt))) {
    GUI$console.history <- c(GUI$console.history, txt)
    ## keep the console history within limits
    if (GUI$console.history.max.length > 0 && length(GUI$console.history) > GUI$console.history.max.length)
      GUI$console.history <- tail(GUI$console.history, GUI$console.history.max.length)
  }
  ## always set the history index to be past the last entry
  GUI$console.history.index <- length(GUI$console.history) + 1
}

gui.move.in.history <- function(delta = -1) {
  ## change the history index by delta
  ## then display the history line corresponding
  ## to it as the last line of the console,
  ## preserving the cursor position
  ## But first, re-record the current line in its history
  ## position, in case the user has made changes and wants to return.

  txt <- gui.get.cons(FALSE)
  if (GUI$console.history.index <= length(GUI$console.history))
    GUI$console.history[GUI$console.history.index] <- txt
  
  i <- GUI$console.history.index + delta
  if (delta == 0 || i < 1 || i > length(GUI$console.history) + 1)
    return()

  if (GUI$console.history.index > length(GUI$console.history) && delta < 0) {
    ## save the current line
    GUI$console.current.line <- gui.get.cons(append.to.history=FALSE)
  } 
  gui.put.cons(if (i <= length(GUI$console.history)) GUI$console.history[i] else GUI$console.current.line, type="input", is.output=FALSE)
  GUI$console.history.index <- i
}

gui.console.at.last.prompt <- function() {
  ## return TRUE if the insertion point is just after the last prompt in the console.
  ## or if the console is empty.
  ## FALSE otherwise

  return (identical(tclchar(GUI$cons, "tag", "prevrange", "prompt", "insert")[2], tclchar(GUI$cons, "index", "insert"))
          && length(tclchar(GUI$cons, "tag", "nextrange", "prompt", "insert")) == 0)
}
  
gui.create.cons.window <- function() {

  ## FIXME: we change some key bindings for the "Text" bindtag, even though we really only want
  ## to do this for the console window.  Unfortunately, I haven't yet figured out how
  ## to add a "break" to an R function call in a binding script, which is what we'd
  ## to do to override the default "Text" key bindings with bindings on ".cons.text"
  
  if (!Tclbool("winfo exists .cons")) {
    TCL('toplevel .cons; wm withdraw .cons')
    tcl("text", GUI$cons, font=GUI$console.font, cursor="xterm", wrap="char", width=GUI$console.line.width)
    .Tcl(".cons.text configure " %:% GUI$console.style$input)
    ## configure styles
    for (s in names(GUI$console.style))
      .Tcl(paste(".cons.text tag configure", s, GUI$console.style[[s]]))
    tcl("scrollbar", ".cons.sb", command=".cons.text yview", orient="vertical")
    tcl(GUI$cons, "configure", yscrollcommand=".cons.sb set")
    tcl("place", GUI$cons, anchor="nw", relwidth=1, relheight=1, width=-20, relx=0)
    tcl("focus", GUI$cons)
    tcl("place", ".cons.sb", anchor="ne", width=20, relheight=1, relx=1)
    ## DANGER:  rebind the <KeyPress-Return> event in Text elements to a null event so that
    ## the return key is really "Submit input"
    tcl("bind", "Text", "<KeyPress-Return>", function(){})
    GUI$old.Text.KeyPressUp.binding <- tclvalue(tcl("bind", "Text", "<KeyPress-Up>"))
    GUI$old.Text.KeyPressDown.binding <- tclvalue(tcl("bind", "Text", "<KeyPress-Down>"))
    tcl("bind", "Text", "<KeyPress-Up>", function(W) {
      if (gui.console.at.last.prompt())
        gui.move.in.history(-1)
      else
        TCL(gsub("%W", W, GUI$old.Text.KeyPressUp.binding))
    })
        
    tcl("bind", "Text", "<KeyPress-Down>", function(W) {
      if (gui.console.at.last.prompt())
        gui.move.in.history(+1)
      else
        TCL(gsub("%W", W, GUI$old.Text.KeyPressDown.binding))
    })

    tcl("bind", GUI$cons, "<KeyRelease-Return>",
        function(s) {
          if (!gui.state.has.key(s, "Shift"))
            gui.eval.cons(gui.get.cons())
          else
            tcl(GUI$cons, "insert", "insert", "\n")
        })

    ## bind HOME to start of line (i.e. move back to last prompt or start of buffer)
    tcl("bind", "Text", "<KeyPress-Home>", function(W) {
      if (is.na(prev.prompt <- tclchar(W, "tag", "prevrange", "prompt", "insert")[2]))
        prev.prompt <- "1.0"
      tcl(W, "mark", "set", "insert", prev.prompt)
      tcl(W, "see", "insert")
    })
      

    ## bind END to start of line (i.e. move back to last prompt or start of buffer)
    tcl("bind", "Text", "<KeyPress-End>", function(W) {
      if (is.na(next.prompt <- tclchar(W, "tag", "nextrange", "prompt", "insert")[1]))
        next.prompt <- "end"
      tcl(W, "mark", "set", "insert", next.prompt %:% "-1c")
      tcl(W, "see", "insert")
    })

    ## bind Esc to restore current line to original state (if it came from history)
    tcl("bind", "Text", "<KeyPress-Escape>", function(W) {
      if (GUI$console.history.index <= length(GUI$console.history)) {
        gui.put.cons(GUI$console.history[GUI$console.history.index], type="input", is.output=FALSE)
        tcl(W, "see", "insert")
      }
    })
      
    gui.put.cons(RSS$splash.text, type="splash")
  }
  tcl("wm", "protocol", ".cons", "WM_DELETE_WINDOW",
      function(){
        TCL("wm withdraw .cons")
      })

}

## this function must follow definitions for any functions used
## as callbacks in tkbind() calls, e.g. "rss.zoom.in()"

gui.create.plot.window <- function() {
  if (!Tclbool("winfo exists .plot")) {
    TCL('toplevel .plot -background ""; wm withdraw .plot')
    TCL('frame .plot.frame -cursor hand2 -borderwidth 0 -relief flat -background ""')
    TCL('canvas .plot.frame.canvas -cursor hand2 -borderwidth 0 -relief flat -highlightthickness 0 -background "black" -closeenough 3')
    TCL('place configure .plot.frame -x 0 -y 0 -anchor nw -relwidth 1.0 -relheight 1.0')
    tcl("bind", ".plot.frame", "<Map>", gui.continue.create.plot.window)

    GUI$plot.dim <- gui.parse.geometry(GUI$windows$.plot$geometry)
    
    tcl("bind", ".plot.frame", "<Expose>",
           function(x, y, w, h) {
             if (!GUI$plot.is.tk.image) {
               gui.repaint.plot.window(as.integer(c(x, y, w, h)))
             }
           })

    tcl("bind", ".plot.frame", "<Configure>",
           function(w, h) {
             coords <- gui.window.dims(".plot.frame")
             if (all(coords > 0)) {
               if (GUI$plot.is.tk.image && any (coords != dim(RSS$pix.mat))) {
                 gui.make.tk.image.for.plot(coords)
                 gui.compass.zoomed()
               }
               ## resize, maintaining the current plot origin at the same offset
               ## from the centre of the window
               offset <- GUI$plot.origin - GUI$plot.dim / 2
               gui.set.plot.dim(coords)
               gui.set.plot.origin(coords / 2 + offset)
             }
           })

    tcl("wm", "protocol", ".plot", "WM_DELETE_WINDOW",
           function(){
             RSS$time.to.go <- TRUE
           })


  }
  ## create a label which will float near the pointer when
  ## GUI$pointer.info.mode is TRUE
  tcl("label", GUI$info, foreground="black", background="white", font=GUI$plot.info.font, justify="left")

  ## place the info label just off the screen
  tcl("place", GUI$info, x=-1, y=-1, anchor="se")

  ## determine our tcl interpreter (KLUDGE: this code relies on having a Tk window)
  GUI$tcl.interp <- .Call("get_tcl_interp", tclint("winfo", "id", ".plot"))

  ## make the range rings and compass items
  gui.create.range.rings()
  gui.create.compass()
  
}


gui.continue.create.plot.window <- function() {
  tcl("bind", ".plot.frame", "<Map>", "")
  gui.set.plot.window(TCL("winfo id .plot.frame"))
  gui.set.plot.is.tk.image(GUI$plot.is.tk.image, force=TRUE)
}
  
gui.set.plot.window.as.group.leader <- function() {
  ## set the plot window to be the group leader:
  ## iconifying it and showing it will do the same
  ## to all other windows
  tcl("bind", ".plot", "<Unmap>",
      function() {
        ## record the z order of open windows
        if (is.null(GUI$windows.in.zorder))
          GUI$windows.in.zorder <- tclchar("wm", "stackorder", ".")
        ## disable the Map handler to prevent
        ## strange loops
        gui.disable.event(".plot", "<Map>")
        ## iconify all radR top-level windows
        for (w in tclchar("wm", "stackorder", "."))
          try(tcl("wm", "withdraw", w), silent=TRUE)
        gui.enable.event(".plot", "<Map>")
      })

  tcl("bind", ".plot", "<Map>",
      function(){
        ## restore the windows which were previously open
        ## first, disable the Unmap handler to prevent
        ## strange loops
        gui.disable.event(".plot", "<Unmap>")
        for (w in GUI$windows.in.zorder)
          try(TCL("wm deiconify $w \n raise $w "), silent=TRUE)
        GUI$windows.in.zorder <- NULL
        gui.enable.event(".plot", "<Unmap>")
      })
}

gui.root.to.win.coords <- function(coords, win=GUI$plot) {
  ## convert root-window-based coordinates to coordinates relative to the
  ## given window; the latter defaults to the plot window
  return (coords - c(tclint("winfo", "rootx", win), tclint("winfo", "rooty", win)))
}

gui.show.pointer.info <- function(plot.coords = GUI$last.pointer.coords)
{
  ## display the pasted odd-numbered elements of text near the plot window cursor,
  ## and copy the even-numbered elements of text to the clipboard
  if (is.null(plot.coords) || (GUI$info.window.follows.mouse && !GUI$mouse.in.plot)) {
    ## if the mouse is outside the plot window or has unknown coordinates
    ## hide the info window
    tcl("place", GUI$info, x=-1, y=-1, anchor="se")
  } else {
    ## let every hook function generate some info for this cursor position
    ## Each hook function should return a two-element character vector
    ## The first element is text (with embedded "\n") to be displayed at
    ## the cursor location.  The second element is text to be copied to
    ## the clipboard.  Elements that are empty strings are ignored.

    spatial.coords <- GUI$tx.plot.to.spatial(plot.coords)
    if (RSS$have.valid$bitmap && RSS$have.valid$scan.data) {
      sample.coords <- GUI$tx.plot.to.matrix(plot.coords)
      cell.coords <- 1 + floor((sample.coords - 1) / RSS$cell.dims)
    } else {
      sample.coords <- NULL
      cell.coords <- NULL
    }
    text <- rss.call.hooks(RSS$PLOT_CURSOR_MOVED_HOOK, plot.coords, spatial.coords, sample.coords, cell.coords)

    ## if there is no text to display, hide the info window
    if (length(text) == 0) {
      tcl("place", GUI$info, x=-1, y=-1, anchor="se")
    } else if (!is.character(text)) {
      tcl(GUI$info, "configure", text="INTERNAL ERROR: a PLOT_CURSOR_MOVED\nhook function is returning a non-character")
    } else if (all(nchar(text) == 0)) {
      tcl("place", GUI$info, x=-1, y=-1, anchor="se")
    } else {      
      ## use non-emtpy strings in the odd/even slots of text as the plot-window popup / clipboard contents
      ## but if the clipboard entry is empty, copy the popup window contents to the clipboard too
      view.text <- text[c(TRUE, FALSE)]
      clip.text <- text[c(FALSE, TRUE)]
##      clip.text[nchar(clip.text) == 0] <- view.text[nchar(clip.text) == 0]
      view.text <- view.text[nchar(view.text) > 0]
      clip.text <- clip.text[nchar(clip.text) > 0]
                        
      tcl(GUI$info, "configure", text=paste(view.text, collapse="\n"))
      if (length(clip.text) > 0)
        gui.set.clipboard(clip.text)
      if (GUI$info.window.follows.mouse) {
        frame.dims <- gui.window.dims(".plot.frame")
        label.dims <- gui.window.dims(GUI$info)
        GUI$last.pointer.coords <- plot.coords
        win.coords <- plot.coords + ifelse(plot.coords + c(10, 10) + label.dims < frame.dims, c(10, 10), -(10 + label.dims))
        GUI$info.window.coords <- win.coords
        tcl("place", GUI$info, x=win.coords[1], y=win.coords[2], anchor="nw")
      } else {
        ## check whether the window has actually moved (4 and 8 are locations of the x and y integers in the
        ## vector returned by "place info"
        if (any(as.integer(tclchar("place", "info", GUI$info)[c(4,8)]) != GUI$info.window.coords))
          tcl("place", GUI$info, x=GUI$info.window.coords[1], y=GUI$info.window.coords[2], anchor="nw")
      }
    }
  }
}

gui.set.clipboard <- function(x) {
  tcl("clipboard", "clear")
  tcl("clipboard", "append", paste(x, collapse=GUI$clipboard.element.separator) %:% GUI$clipboard.element.terminator)
}
  
gui.select.file.for.port <- function(port, init.dir) {
  ## FIXME
  ## pop up a file selection dialog for a port having is.file == TRUE
  ## - init.file is the initial location at which the dialog points
  ## If a file is selected, the following parameters are used:
  ## - menu is the name of the menu to which to append this file name
  ## - menu.button is the name of the menubutton whose text will be changed
  ##   to the name of this file, when selected
  ## The newly-appended entry is invoked

  is.source <- port$is.source
  if (!is.list(port$file.ext)) {
    default.ext <- port$file.ext
    file.types <- '{"' %:% class(port)[1] %:% '" {.' %:% type.ext %:% '}} {"All files" {.*}}'
  } else {
    default.ext <- names(port$file.ext)[1]
    file.types <- paste('{"', port$file.ext, '" {.', names(port$file.ext), '}}', sep="", collapse=" ") %:% ' {"All files" {.*}}'
  }
    ## Note: we must paste space-separated pieces returned by tcl(tk_getOpenFile)
  filename <- paste(tclchar(ifelse(is.source, "tk_getOpenFile", "tk_getSaveFile"),
                            filetypes = file.types,
                            title="Choose a file for " %:% ifelse(is.source, "playback", "recording"), defaultextension="." %:% default.ext,
                            initialdir=init.dir), collapse=" ")
  tcl("focus", ".play")
  if (length(filename) > 0 && nchar(filename[1]) > 0) {
    rss.set.port(port, filename=filename)
    return (TRUE)
  }
  return(FALSE)
}

gui.port.label <- function(port) {
  ## return the label to be used in source/sink menus and labels for
  ## a port.  Should only be called if port$is.file is TRUE
  
  class(port)[1] %:% ": " %:% config(port)$filename
}

gui.activate.named.file.port <- function() {
  ## If the user has clicked on a source/sink menu button
  ## representing a port with a particular filename, and that
  ## button was not already selected, then configure that port
  ## to use that filename.
  ## Note: this is a closure whose environment must
  ## be set to include bindings for "port" and "filename"
  ## (see its use in gui.set.port)

  if (port$is.source) {
    menu <- ".play.source.menu"
  } else {
    menu <- ".play.sink.menu"
  }    
  if (tclvalue("lastgroup" %:% menu) != tclvalue("group" %:% menu)) {
    tcl("set", "lastgroup" %:% menu, tclvalue("group" %:% menu))
    rss.set.port(port, filename=filename)
  }
}

gui.set.run <- function(i, ...) {
  ## set the current run to i
  RSS$current.run <- i
  
  ## resetting the slider to the start
  ## also causes a seek to there

  if (isTRUE(RSS$source$is.seekable)) {
    tcl(".play.slider", "configure", to=tc$num.scans[i])
  } else if (isTRUE(RSS$source$can.specify.start.time)) {
    config(RSS$source, start.time = as.numeric(tc$start.time[i]))
  }
  gui.reset.slider(new.source = TRUE)
}      

gui.set.port <- function(port)
{
  ## the given port has been selected (possibly after reconfigurating it with a new filename)
  ## Update the GUI to reflect this.

  if (port$is.source) {
    menu <- ".play.source.menu"
    label.widget <- ".play.sourcelabel"
  } else {
    menu <- ".play.sink.menu"
    label.widget <- ".play.sinklabel"
  }
  if (port$is.file) {
    label <- gui.port.label(port)
    if (port$is.source) {
      filename <- config(port)$filename
      GUI$default.dir <- dirname(filename[1])
      tcl("set", "lastgroup" %:% menu, "-1")
      ## create a sink/source menu entry for this port/filename combination
      if (gui.index.of.menu.label(menu, label) < 0) {
        rss.try(tcl(menu, "delete", label))
        tcl(menu, "add", "radiobutton", label=label, variable="group" %:% menu, value = label)
      }
      ## temporarily disable the command for this button
      tcl(menu, "entryconfigure", label, command=function(){})
      ## invoke the button to have it checked and any other source button unchecked
      tcl(menu, "invoke", label)
      ## restore / init this button's command
      tcl(menu, "entryconfigure", label, command=rss.make.closure(gui.activate.named.file.port, list(port=port, filename=filename)))
    } else {
      tcl("set", "group" %:% menu, -1)
    }
    tcl(label.widget, "configure", text=label)
  } else {
    tcl(label.widget, "configure", text=port$name)
  }    
    
  if (port$is.source) {
    can.play <- TRUE
    show.toc <- port$has.toc
    if (port$has.toc) {
      ## try get a table of contents
      tc <- get.contents(port)
      if (is.null(tc) || length(tc$num.scans) == 0) {
        ## no toc found, so disable play
        can.play <- FALSE
        show.toc <- FALSE
        ## we still must update the playlist to show this
      }
      ## toc found, so replace the playlist menu with an entry
      ## appropriate to the contents of this port
      
      TCL("destroy .play.playlist")
      
      ## A function to seek to the first scan of a given run
      ## and set the slider appropriately.  This function
      ## will be called each time an item in the playlist
      ## menu is selected
      
      if (!show.toc) {
        gui.create.menu.button(".play.playlist", c(), null.label="No table of contents found.")
      } else {
        gui.create.menu.button(".play.playlist",
                               paste(seq(along=tc$num.scans), ".  ",
                                     format.time.interval(tc$start.time,tc$end.time),
                                     sprintf("; Scans: %6d", tc$num.scans), sep=""),
                               set.fun = rss.make.closure(gui.set.run, list(tc=tc)))
        tcl(".play.playlist.menu", "configure", font=GUI$console.font)
        tcl(".play.playlist", "configure", anchor="w")
      }
    }
    tcl("raise", if (port$is.seekable) ".play.slider" else ".play.progbar")

    if (!port$has.toc) {
      TCL("set .play.playlist.lab {Source has no table of contents}
           .play.playlist configure -state disabled")
    } else {
      TCL(" place configure .play.playlist -x 0 -relwidth 1.0 -rely 0.333")
      tcl(".play.playlist", "configure", state=ifelse(show.toc, "normal", "disabled"))
    }
    gui.reset.slider(TRUE)
    if (can.play && port$is.seekable) {
      tcl(".play.slider", "configure", state="normal")
    } else {
      ## disable the slider
      tcl(".play.slider", "configure", state="disabled")
    }
    TCL(".play.play deselect
       .play.pause deselect
       .play.pause configure -state disabled
       .play.stop deselect
       .play.stop configure -state disabled")

    tcl(".play.play", "configure", state=ifelse(can.play, "normal", "disabled"))
    tcl(".play.play1", "configure", state=ifelse(can.play, "normal", "disabled"))
    tcl(".play.tostart", "configure", state=ifelse(can.play && port$is.seekable, "normal", "disabled"))
    tcl(".play.toend", "configure", state=ifelse(can.play && port$is.seekable, "normal", "disabled"))
    gui.update.plot.window()
  } else {
    RSS$recording <- TRUE
    TCL(".play.record configure -state normal")
    if (GUI$press.record.after.choosing.sink)
       TCL(".play.record select")
  }
}

gui.set.no.port <- function(which="source") {
  ## unselect the source or sink port, shutting it down if
  ## it is not NULL
  if (which == "source") {
    menu <- ".play.source.menu"
    label.widget <- ".play.sourcelabel"
    label.text <- "(select a source)"
    gui.no.source()
  } else {
    menu <- ".play.sink.menu"
    label.widget <- ".play.sinklabel"
    label.text <- "(select a destination)"
    RSS$recording <- FALSE
    gui.no.sink()
  }
  tcl(label.widget, "configure", text=label.text)
  ## make sure the appropriate "No port" menu entry has been selected,
  ## in case this function was triggered by the user.

  if (tclvalue("group" %:% menu) != "(none)")
    tcl(menu, "invoke", "0")

  gui.update.plot.window()
}

gui.reset.slider <- function(new.source=FALSE) {
  ## Set the slider to the first position
  ## and load the first image from the current run.
  ## If the slider is already at the first position,
  ## we must force a call to the new position handler
  ## because tcl will not.  This is required in case
  ## we have changed to a different run (i.e. a different
  ## item from the playlist has been selected)
  
###.if $DEBUG
  print("Resetting slider.")
###.endif  

  if (isTRUE(RSS$source$is.seekable) || new.source) {
    force <- 1 == Tclint(".play.slider get")
    TCL(".play.slider set 1")
    if (force)
      gui.new.slider.pos()
  }
}

gui.new.slider.pos <- function() {

  ## Indicate (via a lock variable) to the radR event loop that the
  ## slider has been moved to a new location This is either the result
  ## of a user event, or of the event loop's call to SCAN_ADVANCE

  RSS$new.scan.index <- Tclint(".play.slider get")
###.if $DEBUG
  print("New scan index is " %:% RSS$new.scan.index)
###.endif  

}

gui.set.plot.cursor <- function(cursor.name) {
  tcl(".plot.frame", "configure", cursor=cursor.name)
  tcl(".plot.frame.canvas", "configure", cursor=cursor.name)
}

gui.create.play.window <- function () {
  if (!Tclbool("winfo exists .play")) {
    TCL("image create photo ::img::play   -file gui/play.gif
         image create photo ::img::play1  -file gui/play1.gif
         image create photo ::img::pause  -file gui/pause.gif
         image create photo ::img::stop   -file gui/stop.gif
         image create photo ::img::record -file gui/record.gif
         image create photo ::img::tostart -file gui/tostart.gif
         image create photo ::img::toend -file gui/toend.gif
         toplevel .play
         wm withdraw .play")
    TCL("checkbutton .play.play   -indicatoron 0 -image ::img::play
         button .play.play1  -image ::img::play1
         checkbutton .play.pause  -indicatoron 0 -image ::img::pause
         checkbutton .play.stop   -indicatoron 0 -image ::img::stop
         button .play.tostart -image ::img::tostart
         button .play.toend   -image ::img::toend
         checkbutton .play.record -indicatoron 0 -image ::img::record -variable play.record -selectcolor $GUI$record.button.active.colour
         menubutton .play.source -indicatoron 0 -menu .play.source.menu -relief raised -bd 2 -highlightthickness 2  -direction flush -text From:
         menu .play.source.menu -title Source -tearoff 0
         label .play.sourcelabel -text {} -anchor w
         menubutton .play.sink -indicatoron 0 -menu .play.sink.menu -relief raised -bd 2 -highlightthickness 2  -direction flush -text To:
         menu .play.sink.menu -title Destination -tearoff 0
         label .play.sinklabel -text {} -anchor w")
    gui.create.menu.button(".play.playlist", c(), null.label="No table of contents found.")

    label <- "(none)"
    tcl(".play.source.menu", "add", "radiobutton", label=label, variable="group.play.source.menu", value = "(none)")
    tcl(".play.source.menu", "invoke", label)
    tcl(".play.source.menu", "entryconfigure", label, command=function()rss.set.no.port("source"))

    label <- "(none)"
    tcl(".play.sink.menu", "add", "radiobutton", label=label, variable="group.play.sink.menu", value = "(none)")
    tcl(".play.sink.menu", "invoke", label)
    tcl(".play.sink.menu", "entryconfigure", label, command=function() rss.set.no.port("sink"))

    TCL("label .play.progbar -relief sunken -borderwidth 2 -anchor w
         scale .play.slider -orient h -showvalue 0 -from 1 -to 1 -resolution 1 -bigincrement 5

         place configure .play.source -x 0 -y 0
         place configure .play.sourcelabel -x 50 -y 5 -relwidth 0.5 -width -50
         place configure .play.sink -y 0 -relx 0.5
         place configure .play.sinklabel -relx 0.5 -x 50 -y 5 -relwidth .5 -width -50
         place configure .play.playlist -x   0 -relwidth 1.0 -rely 0.333 
         place configure .play.play     -x   2 -rely 1 -anchor sw -y -2
         place configure .play.play1    -x  27 -rely 1 -anchor sw -y -2
         place configure .play.pause    -x  52 -rely 1 -anchor sw -y -2
         place configure .play.stop     -x  77 -rely 1 -anchor sw -y -2
         place configure .play.tostart   -x 102 -rely 1 -anchor sw -y -2
         place configure .play.toend     -x 127 -rely 1 -anchor sw -y -2
         place configure .play.record   -x 152 -rely 1 -anchor sw -y -2
         place configure .play.progbar  -x 1 -y -3 -relx 1 -rely 1 -relwidth 1 -width -177 -anchor se
         place configure .play.slider   -relx 1 -rely 1 -relwidth 1 -width -177 -anchor se

         .play.play   configure -state disabled
         .play.play1  configure -state disabled
         .play.pause  configure -state disabled
         .play.record configure -state disabled
         .play.tostart configure -state disabled
         .play.toend   configure -state disabled
         .play.stop   configure -state disabled")

    tcl(".play.progbar", "configure", font=GUI$console.font)
    tcl("focus", ".play.slider")
  }
  tcl("wm", "protocol", ".play", "WM_DELETE_WINDOW",
         function(){
           TCL("wm withdraw .play")
         })
}

gui.create.blip.window <- function () {
  if (!Tclbool("winfo exists .blip")) {
    TCL("toplevel .blip; wm withdraw .blip")

    g.bl <- tcl("checkbutton", ".blip.blipping",
                anchor="w",
                variable="blip.blipping",
                text="find blips",
                command=function() {
                  val <- tclbool("expr", "${blip.blipping}")
                  rss.defer.assign(blip.finding, val, RSS)
                  if (!val)
                    rss.defer.call(rss.stop.blipping())
                  gui.enable.tree(".blip.stats", val)
                  if (RSS$play.state < RSS$PS$PLAYING)
                    rss.process.scan(put.scan = FALSE, calculate.scores = val, find.patches = val, process.patches = val,
                                         convert.scan = TRUE, is.preview = TRUE)

                  ## fix the enabled/disabled status of the stats_k parm
                  if (val && identical("0", as.character(tclvalue("blip.updatestats")))) {
                    TCL(".blip.stats.stats_k.spin configure -state disabled
                         .blip.stats.stats_k.lab  configure -state disabled")
                  }
                })

    g.fn <- gui.create.gauge(".blip", "noise cutoff", c(0, 65535),
                           GUI$noise.cutoff.spinner.increment,
                           RSS$noise.cutoff,
                           function(x){
                             rss.defer.assign(noise.cutoff, x, RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(filter.noise = TRUE, put.scan = FALSE, calculate.scores = FALSE,
                                             convert.scan = TRUE, is.preview = TRUE)
                           })
    
    g.bsf <- tcl("labelframe", ".blip.stats", labelwidget=".blip.blipping", padx=6)

    g.ls <- gui.create.gauge(".blip.stats", "learning scans", c(0, 100),
                             GUI$learning.scans.spinner.increment,
                             RSS$default.scans.to.learn,
                             function(x) {
                               RSS$default.scans.to.learn <- x
                             })

    g.rsl <- tcl("button", ".blip.stats.relearn", text="Restart learning now",
                 command = function(...) {
                   RSS$restart.learning <- TRUE
                 })

    g.us <- tcl("checkbutton", ".blip.stats.updatestats",
                anchor="w",
                variable="blip.updatestats",
                text="update stats every scan",
                command=function() {
                  val <- tclboolvar("blip.updatestats")
                  rss.defer.assign(update.stats.while.blipping, val, RSS)
                  tcl(".blip.stats.stats_k.spin", "configure",
                      state=ifelse(val, "normal", "disabled"))
                  tcl(".blip.stats.stats_k.lab", "configure",
                      state=ifelse(val, "normal", "disabled"))
                  tcl(".blip.stats.excludeblips", "configure",
                      state=ifelse(val, "normal", "disabled"))
                })


    g.exblp <- tcl("checkbutton", ".blip.stats.excludeblips",
                anchor="w",
                variable="blip.excludeblips",
                text="exclude blips from stats update",
                command=function() {
                  val <- tclboolvar("blip.excludeblips")
                  rss.defer.assign(blip.exclude.blips.from.stats.update, val, RSS)
                })

    g.k <- gui.create.gauge(".blip.stats", "old stats weighting", c(0.0, 1.0),
                            GUI$stats.k.spinner.increment,
                            RSS$stats.k,
                            function(x) {
                              rss.defer.assign(stats.k, x, RSS)
                            },
                            short.name = "stats_k")


    g.hs <- gui.create.gauge(".blip.stats", "hot score threshold high", c(-128, 128),
                           GUI$hotscore.spinner.increment,
                           RSS$blip.score.threshold[1],
                           function(x){
                             rss.defer.assign(blip.score.threshold, c(x,RSS$blip.score.threshold[2]), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE, calculate.scores = FALSE,
                                             convert.scan = TRUE, is.preview = TRUE)
                           })
    
    g.cs <- gui.create.gauge(".blip.stats", "hot score threshold low", c(-128, 128),
                           GUI$coldscore.spinner.increment,
                           RSS$blip.score.threshold[2],
                           function(x){
                             rss.defer.assign(blip.score.threshold, c(RSS$blip.score.threshold[1],x), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE, calculate.scores = FALSE,
                                             convert.scan = TRUE, is.preview = TRUE)
                           })

    g.cw <- gui.create.gauge(".blip.stats", "samples per cell", c(1, 128),
                           1,
                           RSS$cell.dims[1],
                           function(x){
                             rss.defer.assign(cell.dims, c(x, RSS$cell.dims[2]), RSS)
                             rss.restart.learning()
                           })
    
    g.ch <- gui.create.gauge(".blip.stats", "pulses per cell", c(1, 128),
                           1,
                           RSS$cell.dims[2],
                           function(x){
                             rss.defer.assign(cell.dims, c(RSS$cell.dims[1], x), RSS)
                             rss.restart.learning()
                           })

    g.di <- tcl("checkbutton", ".blip.stats.diag",
                anchor="w",
                variable="blip.diag",
                text="blips extend diagonally",
                command = function() {
                  rss.defer.assign(blip.use.diags, tclboolvar("blip.diag"), RSS)
                  if (RSS$play.state < RSS$PS$PLAYING)
                    rss.process.scan(put.scan = FALSE, calculate.scores = FALSE, convert.scan = TRUE, is.preview = TRUE)
                })          

    g.aw <- tcl("checkbutton", ".blip.aweight",
                anchor="w",
                variable="blip.aweight",
                text="blip centroids by area, not intensity",
                command = function() {
                  rss.defer.assign(blip.area.weighting, tclboolvar("blip.aweight"), RSS)
                  if (RSS$play.state < RSS$PS$PLAYING)
                    rss.process.scan(put.scan = FALSE, calculate.scores = FALSE, convert.scan = TRUE, is.preview = TRUE)
                })          


    g.bpfilt <- tcl("checkbutton", ".blip.filtering",
                anchor="w",
                variable="blip.filtering",
                text="filter blips",
                command=function() {
                  val <- tclboolvar("blip.filtering")
                  rss.defer.assign(blip.filtering, val, RSS)
                  gui.enable.tree(".blip.patches", val)
                  if (RSS$play.state < RSS$PS$PLAYING)
                    rss.process.scan(put.scan = FALSE, calculate.scores = FALSE,
                                         classify.samples = FALSE, convert.scan = TRUE, is.preview = TRUE)
                })
    
    g.bpf <- tcl("labelframe", ".blip.patches", labelwidget=".blip.filtering", padx=6)

    g.nslo <- gui.create.gauge(".blip.patches", "min blip samples", c(2, 50000),
                           GUI$minsamples.spinner.increment,
                           RSS$blip.samples.minmax[1],
                           function(x){
                             rss.defer.assign(blip.samples.minmax, c(x,RSS$blip.samples.minmax[2]), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.nshi <- gui.create.gauge(".blip.patches", "max blip samples", c(-1, 50000),
                           GUI$maxsamples.spinner.increment,
                           RSS$blip.samples.minmax[2],
                           function(x){
                             rss.defer.assign(blip.samples.minmax, c(RSS$blip.samples.minmax[1],x), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.alo <- gui.create.gauge(".blip.patches", "min blip area (m^2)", c(0, 50000),
                           GUI$minblip.spinner.increment,
                           RSS$blip.area.minmax[1],
                           function(x){
                             rss.defer.assign(blip.area.minmax, c(x,RSS$blip.area.minmax[2]), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.ahi <- gui.create.gauge(".blip.patches", "max blip area (m^2)", c(-1, 50000),
                           GUI$maxblip.spinner.increment,
                           RSS$blip.area.minmax[2],
                           function(x){
                             rss.defer.assign(blip.area.minmax, c(RSS$blip.area.minmax[1],x), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })
    
    g.anglo <- gui.create.gauge(".blip.patches", "min angular span", c(0, 1024),
                           GUI$minangles.spinner.increment,
                           RSS$blip.angular.minmax[1],
                           function(x){
                             rss.defer.assign(blip.angular.minmax, c(x,RSS$blip.angular.minmax[2]), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.anghi <- gui.create.gauge(".blip.patches", "max angular span", c(-1, 1024),
                           GUI$maxangles.spinner.increment,
                           RSS$blip.angular.minmax[2],
                           function(x){
                             rss.defer.assign(blip.angular.minmax, c(RSS$blip.angular.minmax[1],x), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.radlo <- gui.create.gauge(".blip.patches", "min radial span", c(0, 1024),
                           GUI$minangles.spinner.increment,
                           RSS$blip.radial.minmax[1],
                           function(x){
                             rss.defer.assign(blip.radial.minmax, c(x,RSS$blip.radial.minmax[2]), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.radhi <- gui.create.gauge(".blip.patches", "max radial span", c(-1, 1024),
                           GUI$maxangles.spinner.increment,
                           RSS$blip.radial.minmax[2],
                           function(x){
                             rss.defer.assign(blip.radial.minmax, c(RSS$blip.radial.minmax[1],x), RSS)
                             if (RSS$play.state < RSS$PS$PLAYING)
                               rss.process.scan(put.scan = FALSE,
                                                    calculate.scores = FALSE, classify.samples = FALSE,
                                                    convert.scan = TRUE, is.preview = TRUE)
                           })

    g.useexpr <-
      tcl("checkbutton", ".blip.patches.useexpr",
          anchor="w",
          variable="use.blip.filter.expr",
          text="also filter by logical expression:",
          command=function() {
            val <- tclboolvar("use.blip.filter.expr")
            rss.defer.assign(use.blip.filter.expr, val, RSS)
            if (RSS$play.state < RSS$PS$PLAYING) {
              if (val)
                RSS$scan.info$blip.filter.expr <- RSS$blip.filter.expr
              else
                RSS$scan.info$blip.filter.expr <- NULL
              rss.process.scan(put.scan = FALSE, calculate.scores = FALSE,
                               classify.samples = FALSE, convert.scan = TRUE, is.preview = TRUE)
            }
          })

    g.filtexpr <-
      gui.create.string(parent = ".blip.patches",
                        label = "R",
                        short.name = "blipfilterexpr",
                        width = 30,
                        height = 30,
                        val = as.character(RSS$blip.filter.expr),
                        val.check = function(x) tryCatch({parse(text=x); TRUE}, error=function(e)FALSE),
                        set.fun = function(x) {
                          rss.defer.assign(blip.filter.expr, parse(text=x), RSS)
                          if (RSS$play.state < RSS$PS$PLAYING) {
                            RSS$scan.info$blip.filter.expr <- parse(text=x)
                            rss.process.scan(put.scan = FALSE, calculate.scores = FALSE,
                                             classify.samples = FALSE, convert.scan = TRUE, is.preview = TRUE)
                          }
                        })

    if (RSS$blip.use.diags)
      tcl(g.di, "select")

    ## now that the subwindows all exist, see whether to enable/disable them
    if (RSS$blip.finding)
      tcl(g.bl, "select")

    if (RSS$blip.filtering)
      tcl(g.bpfilt, "select")
    
    ## set the initial value for blip.updatestats
    if (RSS$update.stats.while.blipping)
      tcl(g.us, "invoke")

    ## set the initial value for blip.stats.excludeblips
    if (RSS$blip.exclude.blips.from.stats.update)
      tcl(g.exblp, "invoke")

    if (RSS$use.blip.filter.expr)
      tcl(g.useexpr, "select")
    
    tcl("pack", g.fn, g.bl, g.ls, g.rsl, g.us, g.exblp, g.k, g.hs, g.cs, g.cw, g.ch, g.nslo, g.nshi, g.alo, g.ahi,
        g.anglo, g.anghi, g.radlo, g.radhi, g.di, g.bsf, g.aw, g.bpf, g.useexpr, g.filtexpr, side="top", fill="x")
  }
  tcl("wm", "protocol", ".blip", "WM_DELETE_WINDOW",
         function(){
           TCL("wm withdraw .blip")
         })
}

gui.bkgd.bmdata <- "#define blip_width 16
                    #define blip_height 16
                    static unsigned char blip_bits[] = {
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };"

gui.bkgd.bmmask <- "#define rrbm_width 16
                    #define rrbm_height 16
                    static unsigned char rrbm_bits[] = {
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };"

gui.compass.bmdata <- "#define compass_width 16
                       #define compass_height 16
                       static unsigned char compass_bits[] = {
                       0x00, 0x00, 0xc0, 0x03, 0x40, 0x00, 0x81, 0x08, 0x00, 0x05, 0x00, 0x22,
                       0x08, 0x51, 0x84, 0x88, 0x02, 0x10, 0x40, 0x22, 0x20, 0x24, 0x10, 0x38,
                       0x08, 0x02, 0x04, 0x01, 0x82, 0x00, 0x01, 0x10 };"
  
gui.rangering.bmdata <- "#define rrbm_width 16
                         #define rrbm_height 16
                         static unsigned char rrbm_bits[] = {
                         0x40, 0x40, 0x80, 0x80, 0x01, 0x00, 0x02, 0x00, 0x04, 0x04, 0x00, 0x08,
                         0x00, 0x10, 0x20, 0x00, 0x40, 0x00, 0x80, 0x80, 0x01, 0x00, 0x00, 0x00,
                         0x00, 0x04, 0x08, 0x08, 0x10, 0x10, 0x20, 0x00 };"

gui.create.palette.widget <- function(class.name, row, label, gamma.incr) {
  class <- RSS$class.named[[class.name]]
  sel <- ".pctl." %:% class.name %:% "chooser"
  enab <- ".pctl." %:% class.name %:% "enabled"
  grad <- ".pctl." %:% class.name %:% "gradient"
  var <-   "pctl." %:% class.name %:% "enab"
  img <- "::img::" %:% class.name %:% "gradient"

  tcl("checkbutton", enab, variable = var, text=label,
      command=function() {
        RSS$show.class[class] <- tclboolvar(var)
        gui.update.plot.window(TRUE)
      })
  tcl("set", var, RSS$show.class[[class]])

  gam <- gui.create.gauge(".pctl",
                          label = NULL,
                          short.name = class.name %:% "gam",
                          range=c(gamma.incr, 10 - gamma.incr),
                          increment = gamma.incr,
                          width = 5,
                          val = RSS$class.gamma[[class]],
                          set.fun = function(x) {
                            RSS$class.gamma[[class]] <- as.numeric(x)
                            rss.realize.class.palette(class)
                            gui.class.palette.changed(class)
                          })
  
  tcl("canvas", grad, width=128, height=16, borderwidth=0, relief="flat")
  tcl("image", "create", "photo", img, width=128, height=16)
  tcl(grad, "create", "image", 0, 0, image=img, anchor="nw")
  gui.create.menu.button(sel, names(RSS$palettes), paste(names(RSS$palettes), ": ", sapply(RSS$palettes, function(x)x$desc), sep=""),
                         set.fun=function(i, val) {
                           RSS$class.palette[[class]] <- names(RSS$palettes)[i]
                           rss.realize.class.palette(class)
                           gui.class.palette.changed(class)
                         },
                         selected = which(RSS$class.palette[[class]] == names(RSS$palettes)))
  tcl("grid", "configure", enab, row = row, column = 0, sticky="w" )
  tcl("grid", "configure", sel, row = row, column = 3)
  tcl("grid", "configure", grad, row = row, column = 2 )
  tcl("grid", "configure", gam,  row = row, column = 1 )
  tcl("bind", grad, "<Button-1>",
      function() {
        pal.name <- RSS$class.palette[[class.name]]
        pal<- RSS$palettes[[pal.name]]
        EPAL$edit(pal.name, class.name,
                  if (rss.plot.data.source.signed()) {
                    c(-2^(RSS$pixel.data.bits-1), 2^(RSS$pixel.data.bits-1)-1)
                  } else {
                    c(0, 2^RSS$pixel.data.bits - 1)
                  }
                  )
      })
}

gui.create.pctl.window <- function() {
  ## create the plotting control window
  if (!Tclbool("winfo exists .pctl")) {
    TCL("
         toplevel   .pctl
         wm withdraw .pctl
         label .pctl.classlab          -text Class
         label .pctl.palettelab        -text Palette
         label .pctl.spectrumlab       -text {Click to edit}
         label .pctl.gammalab          -text Gamma
         label .pctl.rangeringlab      -text Rings
         label .pctl.rangeringspacelab -width 20
         label .pctl.compassradiuslab  -width 20
         label .pctl.bkgdlab           -text {Background layer} -anchor w
         grid configure .pctl.classlab    -row 0 -column 0
         grid configure .pctl.palettelab  -row 0 -column 3
         grid configure .pctl.spectrumlab -row 0 -column 2
         grid configure .pctl.gammalab    -row 0 -column 1")
    
    i <- 1
    for (type in c("Blip", "Hot", "Cold", "Other", "Excluded")) {
      gui.create.palette.widget(tolower(type), i, type, GUI$class.palette.gamma.increment[[i]])
      i <- i+1
    }
    tcl("checkbutton", ".pctl.rangeringenabled",
        variable = "pctl.rangeringenabled",
        text = "Range rings",
        command = function() {
          val <- tclboolvar("pctl.rangeringenabled")
          tcl(".pctl.rangeringcolour", "configure", state=ifelse(val, "normal", "disabled"))
          tcl(".pctl.rangeringslider.spin", "configure", state=ifelse(val, "normal", "disabled"))
          gui.enable.range.rings(val)
          gui.update.plot.window(TRUE)
        })
    if (GUI$range.rings.enabled)
      tcl(".pctl.rangeringenabled", "select")
    
    tcl("image", "create", "bitmap", "::img::pctlrangeringbm",
        data=gui.rangering.bmdata, maskdata=gui.bkgd.bmmask,
        foreground = GUI$range.ring.colour, background = "black")

    tcl("button", ".pctl.rangeringcolour", image="::img::pctlrangeringbm",
        command = function() {
          focus.win <- tcl("focus")
          col <- tclchar("tk_chooseColor", initialcolor = GUI$range.ring.colour,
                                  title="Choose the colour for the range rings")
          tcl("focus", focus.win)
          if (length(col) > 0) {
            GUI$range.ring.colour <- col
            tcl("::img::pctlrangeringbm", "configure", foreground = col)
            tcl(GUI$plot, "itemconfigure", "ring", outline=col)
            if (!GUI$plot.is.tk.image)
              gui.update.plot.window(TRUE)
          }
        })

    gui.create.gauge(".pctl", "metres", c(GUI$range.ring.scale.increment, GUI$range.ring.scale.max),
                     GUI$range.ring.scale.increment,
                     GUI$range.ring.spacing,
                     function(x) {
                       x <- as.numeric(x)
                       tcl(".plot.frame.canvas", "scale", "ring", GUI$plot.origin[1], GUI$plot.origin[2], x / GUI$range.ring.spacing, x / GUI$range.ring.spacing)
                       GUI$range.ring.spacing <- x
                       if (!GUI$plot.is.tk.image)
                         gui.update.plot.window(TRUE)
                     },
                     short.name = "rangeringslider",
                     width=6)

    tcl("image", "create", "bitmap", "::img::pctlbkgdcolour",
        data=gui.bkgd.bmdata, maskdata=gui.bkgd.bmmask,
        foreground = RSS$pix.mat.background, background = "black")

    tcl("button", ".pctl.bkgdcolour", image="::img::pctlbkgdcolour",
        command = function() {
          focus.win <- tcl("focus")
          col <- tclchar("tk_chooseColor", initialcolor = RSS$pix.mat.background,
                                  title="Choose a colour for the background")
          tcl("focus", focus.win)
          if (length(col) > 0) {
            RSS$pix.mat.background <- col
            tcl("::img::pctlbkgdcolour", "configure", foreground = col)
            gui.update.plot.window(TRUE)
          }
        })

    tcl("image", "create", "bitmap", "::img::pctlcompcolour",
        data=gui.compass.bmdata, maskdata=gui.bkgd.bmmask,
        foreground = GUI$compass.colour, background = "black")

    tcl("button", ".pctl.compcolour", image="::img::pctlcompcolour",
        command = function() {
          focus.win <- tcl("focus")
          col <- tclchar("tk_chooseColor", initialcolor = GUI$compass.colour,
                                  title="Choose a colour for the compass")
          tcl("focus", focus.win)
          if (length(col) > 0) {
            GUI$compass.colour <- col
            tcl("::img::pctlcompcolour", "configure", foreground = col)
            tcl(GUI$plot, "itemconfigure", "compass", fill = col)
            if (!GUI$plot.is.tk.image)
              gui.update.plot.window(TRUE)
          }
        })

    tcl("checkbutton", ".pctl.showplot",
        variable = "pctl.showplot", text="Update plot while playing",
        command = function() {
          if (GUI$plot.enabled <- tclboolvar("pctl.showplot"))
            gui.update.plot.window(TRUE)
        })

    tcl("checkbutton", ".pctl.tkplot",
        variable = "pctl.tkplot", text="Use slow tk plotting; needed for displaying tracks and zones",
        command = function() {
          gui.set.plot.is.tk.image(tclboolvar("pctl.tkplot"))
        })

    tcl("checkbutton", ".pctl.compass",
        variable = "pctl.compass", text="Compass",
        command = function() {
          val <- tclboolvar("pctl.compass")
          tcl(".pctl.compcolour", "configure", state=ifelse(val, "normal", "disabled"))
          gui.enable.compass(val)
          gui.update.plot.window(TRUE)
        })

    gui.create.gauge(".pctl", "pixels", c(GUI$compass.scale.increment, GUI$compass.scale.max),
                     GUI$compass.scale.increment,
                     GUI$compass.radius,
                     function(x) {
                       x <- as.numeric(x)
                       tcl(".plot.frame.canvas", "scale", "compass", GUI$plot.origin[1], GUI$plot.origin[2], x / GUI$compass.radius, x / GUI$compass.radius)
                       GUI$compass.radius <- x
                       if (!GUI$plot.is.tk.image)
                         gui.update.plot.window(TRUE)
                       else
                         gui.compass.zoomed()
                     },
                     setter.name = "gui.set.compass.radius",
                     short.name = "compassslider",
                     width = 5)

    tcl("label", ".pctl.modelabel", text="Plot data: ")
    
    gui.create.menu.button(".pctl.mode", names(RSS$plot.data.sources),
                           set.fun = function(m, ...) {
                             src <- RSS$plot.data.sources[[m]]
                             if (!is.null(GUI$default.cold.palette.for.source[[src]])) {
                               GUI$saved.cold.palette.name <- RSS$class.palette$cold
                               gui.set.class.palette("cold", GUI$default.cold.palette.for.source[[src]])
                             } else {
                               if (!is.null(GUI$saved.cold.palette.name)) {
                                 gui.set.class.palette("cold", GUI$saved.cold.palette.name)
                               }
                             }
                             rss.set.plot.data.source(src)
                             gui.update.plot.window(TRUE)
                           })

    tcl(".pctl.mode.menu", "invoke", match(RSS$plot.data.source, RSS$plot.data.sources) - 1)
        
    if (GUI$plot.enabled)
      tcl(".pctl.showplot", "select")

    if (GUI$plot.is.tk.image)
      tcl(".pctl.tkplot", "select")
    
    if (GUI$compass.enabled)
      tcl(".pctl.compass", "select")
    
    tcl("wm", "iconbitmap", ".pctl", GUI$application.icon)
    .Tcl(paste(
         "grid configure .pctl.bkgdlab           -row", i + 1, "-column 0 -columnspan 2 -sticky w;",
         "grid configure .pctl.bkgdcolour        -row", i + 1, "-column 2 -sticky ew;",
         "grid configure .pctl.compcolour        -row", i + 2, "-column 2 -sticky ew;",
         "grid configure .pctl.compass           -row", i + 2, "-column 0 -sticky w;",
         "grid configure .pctl.compassslider     -row", i + 2, "-column 1 -sticky ew;",
         "grid configure .pctl.compassradiuslab  -row", i + 2, "-column 3 -columnspan 2 -sticky w;",
         "grid configure .pctl.rangeringenabled  -row", i + 3, "-column 0 -sticky w;",
         "grid configure .pctl.rangeringcolour   -row", i + 3, "-column 2 -sticky ew;",
         "grid configure .pctl.rangeringslider   -row", i + 3, "-column 1 -sticky ew;",
         "grid configure .pctl.rangeringspacelab -row", i + 3, "-column 3 -columnspan 2 -sticky w;",
         "grid configure .pctl.showplot          -row", i + 4, "-column 0 -columnspan 3 -sticky w;",
         "grid configure .pctl.modelabel         -row", i + 4, "-column 2 -sticky e;",
         "grid configure .pctl.mode              -row", i + 4, "-column 3 -sticky w;",
         "grid configure .pctl.tkplot            -row", i + 5, "-column 0 -columnspan 3 -sticky w;"
    ))
    
  }
  tcl("wm", "protocol", ".pctl", "WM_DELETE_WINDOW",
      function(){
        TCL("wm withdraw .pctl")
      })
}

gui.set.class.display <- function(class.name, enab=TRUE) {
  if (enab != RSS$show.class[[class.name]])
    tcl(".pctl." %:% class.name %:% "enabled", "invoke")
}
  
gui.set.class.palette <- function(class.name, palette.name, gamma = RSS$class.gamma[[class.name]]) {
  tcl(".pctl." %:% class.name %:% "chooser.menu", "invoke", palette.name %:% "*")
  tcl(gam <- ".pctl." %:% class.name %:% "gam.spin", "set", gamma - tclreal(gam, "cget", "-increment"))
  tcl(gam, "invoke", "buttonup")
}

gui.register.new.palette <- function(palette.name) {
  for (class.name in names(RSS$class.palette))
    gui.add.item.to.menu.button(".pctl." %:% class.name %:% "chooser", palette.name, palette.name %:% ": " %:% RSS$palettes[[palette.name]]$desc)
}

gui.register.palette.desc.change <- function(palette.name) {
  index <- which(names(RSS$palettes) == palette.name)
  for (class.name in names(RSS$class.palette))
    gui.change.item.in.menu.button(".pctl." %:% class.name %:% "chooser", index, palette.name, palette.name %:% ": " %:% RSS$palettes[[palette.name]]$desc)
}

gui.set.plot.window.title <- function() {
  if (is.null(RSS$source)) {
    wt <- "(no scan)"
  } else {
    ts <- structure(RSS$scan.info$timestamp, class="POSIXct")

    if (RSS$blip.finding) {
      left <- RSS$scans.to.learn - RSS$num.scans.learned
      if (left > 0 && RSS$num.scans.learned == 0) {
        extra <- sprintf(" To learn: %d", left)
      } else if (left > 0 && RSS$scans.to.learn > 0) {
        extra <- sprintf(" Learned: %d scans; %d remain(s)", RSS$num.scans.learned, left)
      } else if (! RSS$have.valid$classification) {
        extra <- sprintf(" Learning of %d scans complete.", RSS$num.scans.learned)
      } else {
        extra <- sprintf(" %5d blips; %7d hot (%.5f%%)", RSS$num.blips, RSS$num.hot.samples, 100 * RSS$num.hot.samples / prod(dim(RSS$scan.mat)))
      }
    } else {
      extra <- ""
    }
    wt <- paste(if (RSS$previewing) "PREVIEW: " else if (RSS$have.processed.scan) "POSTVIEW: ", format(ts, format=GUI$plot.title.date.format, tz=RSS$timezone, usetz=TRUE),
                extra,
                paste(sprintf("  %.2f", gui.plot.window.geometry()),
                          collapse=" x "),
                " km",
                sep="")
  }
  tcl("wm", "title", ".plot", paste(GUI$windows$.plot$title, "   ", wt, sep=""))
}

gui.xlat.tcl.colour <- function(colour) {
  ## in case the colour doesn't match the "#rrggbb" format,
  ## look it up in the system's rgb colour database
  if (substring(colour, 1, 1) != "#")
    colour <- "#" %:% paste(sprintf("%02x", floor(tclint("winfo", "rgb", ".", colour) / 256)), collapse="")
  return(colour)
}
  
gui.class.palette.changed <- function(class) {
  ## update the gui palette controls to mark a change in the palette
  ## for the given sample class.  
  ## This happens when a user does one of these things:
  ##  - edits the palette currently selected for class
  ##  - selects a different palette for class
  ##  - changes the gamma value for class
  ## Currently, this function simply repaints the palette
  ## bar and updates the plot window.
  ## It is assumed that RSS$palette.mat has already been changed
  ## by calling rss.realize.palette(class)

  img <- "::img::" %:% names(RSS$class.named)[[class]] %:% "gradient"
  if (length(tclchar("info", "command", img)) == 0)
    ## there is no gradient image for this class
    return ()
  width <- tclint(img, "cget", "-width")
  height <- tclint(img, "cget", "-height")
  max.colour <- 2 ^ RSS$pixel.data.bits - 1
  if (!rss.plot.data.source.signed()) {
    inds <- round(1 + (0:(width-1)) * (max.colour) / (width-1))
  } else {
    inds <- round(1 + c(width%/%2 + (0:(width%/%2-1)), 0:(width%/%2-1)) * (max.colour) / (width-1))
  }
  data <- paste("{{", paste(rss.rgbmat.to.tclcolour(rss.rgbmat.blend(rss.rgbint.to.rgbmat(RSS$palette.mat[inds, class]), rss.tclcolour.to.rgbmat(gui.xlat.tcl.colour(tclchar(".pctl", "cget", "-background")))), GUI$plot.is.tk.image), collapse=" "), " }} -to", 0, 0, width - 1, height - 1)
  TCL(img %:% " put " %:% data )
  gui.update.plot.window(TRUE)
}

gui.create.range.rings <- function() {
  num.range.rings <- GUI$num.range.rings
  origin <- GUI$plot.origin
  spacing <- GUI$range.ring.spacing / GUI$mpp
  tcl(GUI$plot, "delete", "ring")
  for (i in 1:num.range.rings) {
    delta <- i * spacing
    ## do not use a dash= option here, because tk sets the line width to a positive value, which is
    ## extremely slow on my X server.  In any case, there is an X11 bug in XDrawArc (see xbug.c) which
    ## causes corrupt drawing of large-radius rings.
    
    tcl(GUI$plot, "create", "oval", origin[1] - delta, origin[2] - delta, origin[1] + delta, origin[2] + delta, outline=GUI$range.ring.colour, tags=c("ring", "pan", "zoom", "antenna", "zoom_arc"), width=1, fill="", state=if (GUI$range.rings.enabled) "normal" else "hidden")
  }
}

gui.plot.range.rings <- function() {
  if (GUI$range.rings.enabled) {
    if (GUI$plot.is.tk.image) {
      # nothing to do
    } else {
      .Call("plot_range_rings", GUI$num.range.rings, GUI$plot.origin,
            GUI$range.ring.spacing / GUI$mpp, rss.tclcolour.to.rgbmat(GUI$range.ring.colour))
    }
  }
}

gui.enable.range.rings <- function(enab=TRUE) {
  GUI$range.rings.enabled <- enab
  if (GUI$plot.is.tk.image) {
    tcl(GUI$plot, "itemconfigure", "ring", state=if (enab) "normal" else "hidden")
    if (enab)
      tcl("canvas_showhide_big_arc", GUI$plot, "zoom_arc")
  }
}

gui.visible.compass.angle <- function() {
  ## return the smallest compass angle divider which is visible
  ## i.e., a tail of the vector GUI$compass.angular.spacing 
  radius <- GUI$compass.radius
  ## FIXME: this lab.size should be the maximum dimension of the string "000" in pixels
  lab.size <- 30
  which <- radius * GUI$compass.angular.spacing * pi / 180 > 1.5 * lab.size
  if (any(which))
    return(which(which)[1])
  else
    return(length(GUI$compass.angular.spacing))
}

gui.which.compass.angle <- function(A) {
  ## given the angle A, in degrees,
  ## return the largest component of GUI$compass.angular.spacing
  ## of which A is an even multiple
  return (tail(which(A %% GUI$compass.angular.spacing == 0), 1))
}

gui.plot.compass <- function() {
  if (GUI$compass.enabled && !GUI$plot.is.tk.image)
      .Call("plot_compass", GUI$plot.origin, GUI$compass.tick.length, GUI$north.angle, GUI$compass.radius, rss.tclcolour.to.rgbmat(GUI$compass.colour))
}

gui.enable.compass <- function(enab=TRUE) {
  GUI$compass.enabled <- enab
  if (GUI$plot.is.tk.image) {
    gui.compass.zoomed()
  }
}

gui.visible.compass.lab.tags <- function() {
  paste(paste("complab", gui.visible.compass.angle(), sep=""), collapse="||")
}

gui.invisible.compass.lab.tags <- function() {
  paste(paste("compass&&!complab", gui.visible.compass.angle(), sep=""), collapse="&&")
}

gui.compass.zoomed <- function() {
  ## show level of labels/ticks appropriate for current zoom
  ## and enabled/disabled state
  if (GUI$compass.enabled) {
    tcl(GUI$plot, "itemconfigure", gui.visible.compass.lab.tags(), state="normal")
    tcl(GUI$plot, "itemconfigure", gui.invisible.compass.lab.tags(), state="hidden")
    tcl(GUI$plot, "itemconfigure", "comptick", state="normal")
  } else {
    tcl(GUI$plot, "itemconfigure", "compass", state="hidden")
  }
}

gui.compass.translated <- function(coords) {
  if (any(coords != 0.0))
    tcl(GUI$plot, "move", "compass", coords[1], coords[2])
}

gui.create.compass <- function() {
  
  rps <- rss.planar.rps()
  radius <- GUI$compass.radius
  if (length(radius) == 0 || !is.finite(radius))
    radius <- 250
  if (radius > 0) {
    ## remember the plot geometry at the time the compass was created
  
    GUI$prev.plot.geom <- GUI[c("mpp", "plot.origin", "north.angle")]

    tcl(GUI$plot, "delete", "compass")
    ang.deg <- 0:359
    ## actual location of this angle label, given current plot rotation
    ang.rad <- pi / 2 - (ang.deg + GUI$north.angle) * pi / 180
    tick.len <- rep(GUI$compass.tick.length, length(ang.deg))
    tick.len[seq(1, 360, 5)] <-  tick.len[seq(1, 360, 5)] + GUI$compass.tick.length
    tick.len[seq(1, 360, 10)] <-  tick.len[seq(1, 360, 10)] + GUI$compass.tick.length
    x <- GUI$plot.origin[1] + cos(ang.rad) * (radius + tick.len)
    y <- GUI$plot.origin[2] - sin(ang.rad) * (radius + tick.len)
    x.in <- GUI$plot.origin[1] + cos (ang.rad) * radius
    y.in <- GUI$plot.origin[2] - sin (ang.rad) * radius
    x.lab <- GUI$plot.origin[1] + cos (ang.rad) * (radius + GUI$compass.tick.length * 2 + 15) ## FIXME: 15 should be 1/2 max label dimension
    y.lab <- GUI$plot.origin[2] - sin (ang.rad) * (radius + GUI$compass.tick.length * 2 + 15)
    for (i in seq(along=ang.rad)) {
      tcl(GUI$plot, "create", "text", x.lab[i], y.lab[i], justify="center", text=ang.deg[i], tags=c("compass", "pan",
    "rotate", paste("complab", which(ang.deg[i] %% GUI$compass.angular.spacing == 0), sep="")), fill=GUI$compass.colour,
    state="hidden", font=GUI$compass.label.font)
    }
    compass.tick.coords <- c(t(cbind(x.in, y.in, x, y, x.in, y.in)), x.in[1], y.in[1])
    tclint(GUI$plot, "create", "line", compass.tick.coords, width=1, fill=GUI$compass.colour, tags=c("compass", "pan",
    "rotate", "comptick"), state="hidden")
  }
}

gui.make.tk.image.for.plot <- function(coords) {
  tcl(GUI$plot, "delete", "plot")
  tcl("destroy", GUI$plot.image.name)
  tcl("image", "create", "photo", GUI$plot.image.name, width=coords[1], height=coords[2])
  tcl(GUI$plot, "create", "image", 0, 0, tags="plot", anchor="nw", image=GUI$plot.image.name)
  tcl(GUI$plot, "lower", "plot")
  .Call("radR_attach_image_to_extmat", GUI$plot.image.name, GUI$tcl.interp, RSS$pix.mat)
  .Call("radR_image_extmat_changed", RSS$pix.mat)
}

gui.set.plot.is.tk.image <- function(is.tk = TRUE, force=FALSE) {
  ## set the flags determining whether plotting is done directly
  ## into a platform-specific-GUI-controlled matrix, or via
  ## a tk photo image
  rps <- rss.planar.rps()
  if (is.tk && (force || ! GUI$plot.is.tk.image)) {
    TCL('place configure .plot.frame.canvas -x 0 -y 0 -anchor nw -relwidth 1.0 -relheight 1.0')
    RSS$pix.mat <- extmat(type="int", name="radR matrix of 32-bit raw ABGR values for the plot window", dim=c(0, 0))
    d <- c(tclint("winfo", "width", ".plot.frame"), tclint("winfo", "height", ".plot.frame"))
    gui.make.tk.image.for.plot(d)
    GUI$plot.is.tk.image <- TRUE
    RSS$plot.is.tk.image <- TRUE
    gui.enable.compass(GUI$compass.enabled)
    gui.enable.range.rings(GUI$range.rings.enabled)
    if (length(GUI$prev.plot.geom) > 0) {
      gui.canvas.panned(GUI$plot.origin - GUI$prev.plot.geom$plot.origin)
      gui.canvas.zoomed(GUI$prev.plot.geom$mpp / GUI$mpp, GUI$plot.origin)
      gui.compass.zoomed()
      gui.canvas.rotated(GUI$north.angle - GUI$prev.plot.geom$north.angle, GUI$plot.origin)
      tcl("canvas_showhide_big_arc", GUI$plot, "zoom_arc")
    }
    tcl(".pctl.tkplot", "select")
    tcl("update")
    rss.call.hooks(RSS$TK_PLOT_MODE_HOOK, TRUE)
  } else if ((!is.tk) && (force || GUI$plot.is.tk.image)) {
    tcl(GUI$plot, "delete", "plot")
    TCL('place configure .plot.frame.canvas -x -1 -y -1 -anchor nw -width 0 -height 0 -relwidth 0 -relheight 0')
    tcl("destroy", GUI$plot.image.name)
    RSS$pix.mat <- .Call("prepare_for_plot", PACKAGE = GUI.PACKAGE)
    GUI$plot.is.tk.image <- FALSE
    RSS$plot.is.tk.image <- FALSE
    if (length(GUI$prev.plot.geom) > 0)
      GUI$prev.plot.geom <- GUI[c("mpp", "plot.origin", "north.angle")]
    tcl(".pctl.tkplot", "deselect")
    rss.call.hooks(RSS$TK_PLOT_MODE_HOOK, FALSE)
  } else {
    ## don't do anything if no change is required and force is false
    return()
  }
  ## re-realize the palettes, since pixel order is different for tk images than
  ## raw radar
  for (class in seq(along=RSS$CLASS.VAL)) {
    rss.realize.class.palette(class)
    gui.class.palette.changed(class)
  }
  gui.update.plot.window()
}
    
gui.update.plot.window <- function(reconv=TRUE) {
  ## update the offscreen plot bitmap
  ## This is used to refresh
  ## the window when parts are exposed, or to force
  ## plotting of new data.
  ## if reconv is true, we do a scan-conversion 

  if (! .Call("have_plot_window", PACKAGE = GUI.PACKAGE))
    return()

  if (GUI$plot.enabled || RSS$play.state < RSS$PS$PLAYING) {

    gui.update.plot.parms()
    GUI$scan.converter <- rss.convert.scan(reconv,
                                           scan.converter = GUI$scan.converter,
                                           geom = if (is.null(GUI$image.geom)) c(0, 0, dim(RSS$pix.mat)) else GUI$image.geom)
    if (GUI$plot.is.tk.image)
      .Call("radR_image_extmat_changed", RSS$pix.mat)
  
    if (RSS$have.valid$bitmap) {
      gui.plot.range.rings()
      gui.plot.compass()
    } else {
      ## if no valid bitmap exists, fill the window with black
      if (!GUI$plot.is.tk.image)
        .Call("show_no_plot", PACKAGE = GUI.PACKAGE)
    }
    ## repaint the window to the device
    if (!GUI$plot.is.tk.image)
      .Call("repaint_plot_window", NULL, PACKAGE = GUI.PACKAGE)
    else
      .Tcl("update idletasks")
  }
  gui.set.plot.window.title()
  if (GUI$plot.enabled)
    gui.show.pointer.info()
}



gui.create.port.file.selector <- function(port) {
  ## create a closure whose job is to select a file for a
  ## file-type port, and add it to the appropriate menu

  return(function() gui.select.file.for.port(port, GUI$default.dir))
}
    
gui.enable.controls <- function (which, enable=TRUE) {
  switch(which,
         blip.finding = {
           gui.enable.tree(".blip.stats", enable)
           gui.enable.tree(".blip.blipping", enable)
         },
         blip.filtering = {
           gui.enable.tree(".blip.patches", enable)
           gui.enable.tree(".blip.filtering", enable)
         })
}

gui.finalize <- function() {
  ## free gui resources, including unloading the
  ## device-dependent module

  ## remove all the gui. functions

  rm(list=ls(pattern="^gui\\.", .GlobalEnv), pos=.GlobalEnv)

  ## close tcl windows
  
  for (w in TCL("winfo children ."))
    try(tcl("destroy", w), silent=TRUE)

  ## unload the GUI shared library
  try(rss.dyn.unload(GUI.PACKAGE), silent=TRUE)

##   cat("About to unload tcltk\n")
##   ## unload the tcltk package
##   try(detach("package:tcltk"), silent=TRUE)

##   ## uninstall the tcltk event handler so that
##   ## we can uninstall the radR event handler
##   ## to which it chains
  
##   switch(.Platform$OS.type,
##          unix = .C("delTcl"),
##          windows = .C("tcltk_end")
##          )
  
##   cat("Unloaded tcltk\n")

##   ## define a diagnostic tcl function to alert to post-unload use
##   tcl <<- function(...) {
##     stop("Attempt to call tcl after unloading gui")
##   }
}



GUI.init <- function() {
  ## load the platform-dependent GUI
  ## this is given by "windows" or "unix" followed by "gui"

  GUI.PACKAGE <<- .Platform$OS.type %:% "gui"
  rss.dyn.load(GUI.PACKAGE, in.dir.of="gui/gui.conf.R")

  ## read the tkImgPNG library (which allows .png images to be loaded into Tk)
  rss.dyn.load("main/R_tkImgPNG")

  ## load the gradient palette editor
  source("gui/epal.R")

  ## set up some tcl functions
  rss.source.tcl("gui/guicanvas.tcl")
  rss.source.tcl("gui/datepick.tcl")
    
  GUI <<- strictenv (
                     ## set up GUI variables
                     ## many of these are set later, and we are just
                     ## creating slots in the strictenv symbol table for them

                     cons                          = ".cons.text",
                     console.current.line          = NULL,
                     console.history.index         = NULL,
                     drag.origin                   = NULL,
                     event.list                    = gui.get.event.list(),
                     events                        = NULL,
                     image.geom                    = NULL,
                     info                          = ".plot.frame.info",
                     last.motion.state             = NULL,
                     last.pointer.coords           = NULL,
                     key.hit                       = NULL,
                     marked.point                  = NULL,
                     located.line.endpoint         = NULL,
                     menu.title.to.tcl.part        = list(),
                     
                     menu.title.to.tcl.path        = list(
                       "radR menu"                 = ".cmd",
                       "Plugins"                   = ".cmd.plugins",
                       "Load a plugin"             = ".cmd.plugins.load",
                       "Sources"                   = ".play.source.menu",
                       "Sinks"                     = ".play.sink.menu"),
                     
                     menu.tcl.name                 = list(
                       main                        = ".cmd",
                       sources                     = ".play.source.menu",
                       sinks                       = ".play.sink.menu"),
                     
                     mouse.in.plot                 = FALSE,
                     new.parm.values               = list (),
                     new.parm.values               = NULL,
                     num.cons.tags                 = NULL,
                     num.graphs                    = 0,
                     num.msgboxes                  = 0,
                     old.Text.KeyPressDown.binding = NULL,
                     old.Text.KeyPressUp.binding   = NULL,
                     playlist.date.fmt             = NULL,
                     play.num.scans                = 0,
                     play.start.time               = NULL,
                     plot                          = ".plot.frame.canvas", ## tk name of plot canvas
                     tx.plot.to.spatial            = NULL,  ## coordinate transforms
                     tx.plot.to.matrix             = NULL,
                     tx.xy.to.plot                 = NULL,
                     tx.xyz.to.plot                = NULL,
                     plot.dim                      = NULL,
                     plot.image.name               = "::img::plot",  ## name of image for tk plot
                     prev.plot.geom                = list(),  ## for saving plot geometry when switching between tk and native modes
                     saved.cold.palette.name       = NULL,
                     scan.converter                = NULL,
                     skip.relocate.tearoff         = NULL,
                     tcl.interp                    = NULL, ## pointer to the tcl interpreter
                     windows.in.zorder             = NULL,

                     PARENT = .GlobalEnv
                     )
  
  ## set default coordinate transforms
  gui.set.coord.tx()

  ## append configuration information
  
  rss.load.config("gui/gui", GUI)

  ## set the console history index to the first available slot
  
  GUI$console.history.index <- length(GUI$console.history) + 1

  ## inform tcl of defaults

  tcl("option", "add", "*Button.font",             GUI$default.font)
  tcl("option", "add", "*Label.font",              GUI$default.font)
  tcl("option", "add", "*Checkbutton.font",        GUI$default.font)
  tcl("option", "add", "*Radiobutton.font",        GUI$default.font)
  tcl("option", "add", "*Menubutton.font",         GUI$default.font)
  tcl("option", "add", "*Menu.font",               GUI$default.font)
  tcl("option", "add", "*Spinbox.font",            GUI$default.font)
  tcl("option", "add", "*Spinbox.background",      GUI$entry.field.background.colour)
  tcl("option", "add", "*Text.background",         GUI$entry.field.background.colour)
  tcl("option", "add", "*Menu.background",         GUI$menu.background)
  tcl("option", "add", "*Menu.disabledForeground", GUI$menu.heading.foreground)

  ## return TRUE to indicate a GUI has been loaded
  return(TRUE)
}

gui.create.windows <- function() {
  gui.create.plot.window()
  gui.repaint.plot.window(NULL)
  gui.create.cons.window()
  gui.create.blip.window()
  gui.create.pctl.window()
  gui.create.play.window()
  EPAL$create.window()
  ## gui.create.command.menu must be called after all toplevel windows have been created,
  ## since it generates a window list
  gui.create.command.menu()
  gui.setup.bindings()

  ## initialize the TkImgPNG library (which must be done after create.plot.window sets the tcl interpreter)
  .Call("init_tk_img_png", GUI$tcl.interp)
}

gui.restore.window.settings <- function() {
  ## assuming all toplevel windows have been created,
  ## restore their geometries, states, titles, and
  ## z-orders

  n <- names(GUI$windows)[order(sapply(GUI$windows, function(x)x$z), decreasing=TRUE)]
  for (wn in n) {
    w <- GUI$windows[[wn]]
    if (!tclbool("winfo", "exists", wn)) {
      wn <- GUI$menu.title.to.tcl.path[[wn]]
      if (is.null(wn)) {
        ## this menu no longer exists, at least by this name
        next
      }
      ## found the menu, invoke it to tear it off
      wn <- tclchar(wn, "invoke", 0)
      tcl("wm", "withdraw", wn)
    }
    tcl("wm", "geometry", wn, w$geometry)
    tcl("wm", "iconbitmap", wn, GUI$application.icon)
    ## skip the special case of the .plot window,
    ## which uses a dynamic title
    if (wn != ".plot")
      tcl("wm", "title", wn, w$title)
    if (w$state == "normal") {
      tcl("wm", "state",  wn, "normal")
      tcl("raise", wn)
    }
  }
  gui.set.plot.window.as.group.leader()
}
    
gui.mousewheel.redirector <- function(W, X, Y, D) {
  ## redirect a mousewheel event to the window
  ## actually under the cursor
  ## event.type, which must be bound in the environment
  ## is the name of the virtual event to be directed
  ## to the window
  CW <- tclchar("winfo", "containing", X, Y)
  if (length(CW) > 0)
    tcl("event", "generate", CW, event.type, x=D)
}

gui.mousewheel.translator <- function(W, x) {
  ## convert a virtual mousewheel event into an up or down
  switch(tclchar("winfo", "class", W),
         "Scale" = tcl("tk::ScaleIncrement", W, ifelse(as.integer(x) > 0, "up", "down"), "little", "noRepeat"),
         "Spinbox" = tcl(W, "invoke", ifelse(as.integer(x) > 0, "buttonup", "buttondown"))
         )
}

gui.parm.to.option = list (

  ## a complete list of the Tk event parameter substitutions and the
  ## corresponding option name for the "event" command.  This is to
  ## allow us to "forward" parameter substitutions from a real event
  ## to a virtual one.
  
  "%a" = "-above %a",
  "%B" = "-borderwidth %B",
  "%b" = "-button %b",
  "%c" = "-count %c",
  "%D" = "-delta %D",
  "%d" = "-detail %d",
  "%E" = "-sendevent %E",
  "%f" = "-focus %f",
  "%h" = "-height %h",
  "%k" = "-keycode %k",
  "%K" = "-keysym %K",
  "%m" = "-mode %m",
  "%o" = "-override %o",
  "%p" = "-place %p",
  "%R" = "-root %R",
  "%#" = "-serial %#",
  "%s" = "-state %s",
  "%S" = "-subwindow %S",
  "%t" = "-time %t",
  "%w" = "-width %w",
  "%X" = "-rootx %X",
  "%x" = "-x %x",
  "%Y" = "-rooty %Y",
  "%y" = "-y %y",
  "%%" = "%"
  )
  
gui.sub.event.parms <- function(x) {
  ## for each "%"-parameter in the character vector x, perform
  ## substitution so that the resulting string can be used as the
  ## options fragment of a call to "event generate"
  ## We try to be correct about converting "%%" into "%"
  m <- gregexpr("%[a-zA-Z#%]", x)[[1]]
  len <- attr(m, "match.length")
  if (m[1] == -1)
    return(x)
  j <- 0
  y <- ""
  for (i in seq(along=m)) {
    y <- paste(y, substring(x, j, m[i] - 1), gui.parm.to.option[[substring(x, m[i], m[i] + len[i] - 1)]], sep="")
    j <- m[i] + len[i]
  }
  return(paste(y, substring(x, j, nchar(x)), sep=""))
}

gui.setup.bindings <- function() {
  ## create various general event bindings
  
  if (GUI$redirect.mousewheel.events.to.children) {
    ## rebind the appropriate toplevel event so that a
    ## MouseWheel event (possibly with shift or ctrl)
    ## is directed to the window below the mouse cursor

    for (w in c("Toplevel", "Spinbox", "Scale", "Text", "Menu", "Canvas", "Checkbutton", "Button")) {
      tcl("bind", w, "<MouseWheel>",
          rss.make.closure(gui.mousewheel.redirector, list(event.type="<<MouseWheel>>")))
      tcl("bind", w, "<Shift-MouseWheel>",
          rss.make.closure(gui.mousewheel.redirector, list(event.type="<<Shift-MouseWheel>>")))
      tcl("bind", w, "<Control-MouseWheel>",
          rss.make.closure(gui.mousewheel.redirector, list(event.type="<<Control-MouseWheel>>")))
      tcl("bind", w, "<Control-Shift-MouseWheel>",
          rss.make.closure(gui.mousewheel.redirector, list(event.type="<<Control-Shift-MouseWheel>>")))
    }
    ## translate virtual mousewheel events for the Scale and Spinbox
    ## widgets into value changes

    for (w in c("Scale", "Spinbox"))
      tcl("bind", w, "<<MouseWheel>>", gui.mousewheel.translator)

    ## In text widgets, paste should remove CR/LF (since our console is dumb about that)
    ## We do this 
##     tcl("bind", "Text", "<<Paste>>", function(W) {
##       .Tcl("set tmpclip [clipboard get]")
##       txt <- tclvalue("tmpclip")
##       .Tcl("unset tmpclip")
##       txt <- gsub("(?s)\\{[ \\t]*\\r?\\n", "{", txt, perl=TRUE)
##       txt <- gsub("(?s)\\r?\\n", ";", txt, perl=TRUE)
##       tcl("clipboard", "clear")
##       tcl("clipboard", "append", txt)
##       .Tcl("tk_textPaste " %:% W)
##     })

  }

  ## load customizable event bindings 

  bindings <- rss.load.config("gui/gui.bindings")
  ## loop over all virtual events (to which something might be bound)
  
  for (virt in ls(bindings)) {
    ## loop over each window binding for this event

    evt <- bindings[[virt]]
    for (i in seq(along=evt)) {
      seqs <- evt[[i]]
      win <- names(evt)[i]
      ## loop over each bound sequence
      for (j in seq(along=seqs)) {
        s <- seqs[j]

        ## s is either an event descriptor, or the word "command", or a list;
        ## the virtual event is either bound to s, or the command for window w
        ## is made to invoke the virtual event.  If s has an "%x" substring,
        ## the virtual event is generated explicitly with an x parameter
        ## set to the value of the 2nd parameter (as a substituted tcl script).
        ## If s is a list, then win must be the name of a canvas widget,
        ## and the bindings are applied to items on the canvas given by the tags
        ## of s.

        event.parms <- "%X %Y %x %y %s" 
        if (is.list(seqs)) {
          tcl(win, "bind", names(seqs)[j], s[[1]], paste("event generate %W <<", virt, ">> ", gui.sub.event.parms(event.parms), sep=""))
        } else if (substring(s, 1, 7) == "command") {
          tcl(win, "configure", command=paste("event generate ", win, " <<", virt, ">> ", gui.sub.event.parms(substring(s, 8)), sep=""))
        } else if (win == "all") {
          tcl("event", "add", "<<" %:% virt %:% ">>", s)
        } else {
          parts <- strsplit(s, "[ \t]+", perl=TRUE)[[1]]
          tcl("bind", win, parts[[1]], paste("event generate %W <<", virt, ">> ", gui.sub.event.parms(paste(event.parms, paste(parts[-1], collapse=" "))), sep=""))
        }
      }
      
      ## Create a function that dispatches through GUI$event.list[[n]] at run time
      ## the trick is to present the call to bind() with a function having the
      ## same formals as GUI$event.list[[n]] so that the correct tcl '%' substitutions
      ## are carried forward.  Moreover, we bind this function to an appropriate symbol
      ## in the global environment so that it can be manipulated (e.g. dispatched to
      ## by code that wants to install its own handler)
      
      f <- rss.make.closure(function() {
        args<-lapply(names(formals(GUI$event.list[[virt]])), function(..x..)get(..x..))
        names(args)<-names(formals(GUI$event.list[[virt]]))
        do.call(GUI$event.list[[virt]], args)
      }, list(virt=virt))
      formals(f) <- c(formals(GUI$event.list[[virt]]), `...`=NULL)
      tcl("bind", win, "<<" %:% virt %:% ">>", f)
      assign(paste("bind", win, "<<" %:% virt %:% ">>"), f, .GlobalEnv)
    }
  }
}

gui.file.dialog <- function(mode, title, types, init.file, on.done)
{
  ## pop up a file-selection dialog, using these parameters:
  ##
  ## mode:  a character vector indicating what kind of file selection dialog, one of:
  ##
  ##    open.one  - open a single existing file
  ##    open.many - open multiple existing files
  ##    save      - open a file for saving
  ##    open.dir  - select a directory
  ##
  ## title: the title to display in the file dialog; some macros substitution is
  ##        done on this.  See rss.file.info.macro
  ##
  ## types: a tagged character vector or list of file types
  ##
  ## init.file: a character scalar (or function returning such) giving
  ##            the full path to the initial default file
  ##
  ## on.done: a function to call when a filename(s) (or directory) has
  ##         been chosen.  Takes one argument, a character vector of
  ##         paths.  This argument has length 0 iff the user hit
  ##         "Cancel".
  ##
  ## Example:
  ##
  ##     gui.file.dialog(
  ##          mode = "open.one",
  ##          title = "Open an image file",
  ##          types = list(".gif" = "GIF images", ".*" = "All files"),
  ##          init.file = "c:/My Documents/mypic.gif",
  ##          on.done = function(f) print("You chose " %:% paste(f, collapse=",")) 
  ##          )

  ## fill in default values; for convenience, we treat NULLs the same as missings
  if (missing(mode) || is.null(mode)) mode <- "open.one"
  if (!missing(init.file) && is.function(init.file))
    init.file <- init.file()
  if (missing(init.file) || is.null(init.file)) init.file <- GUI$default.dir
  if (missing(title) || is.null(title))
    title <- "Choose a file for reading"
  else
    title <- rss.file.info.macro(title, init.file)
  if (missing(types) || is.null(types)) types <- list(".*" = "All Files")
  if (missing(on.done)) on.done <- NULL
  types <- paste('{"', types, '" {', names(types), '}}', sep="", collapse=" ")

  ## save the current focus
  focus.win <- tcl("focus")
  
  if (mode != "open.dir") {
    ## if initial file is a directory, append "." to it to prevent
    ## the following dialogues from opening in the parent directory.
    if (file.exists(init.file) && file.info(init.file)$isdir)
      init.file <- file.path(init.file, "*")
    files <- do.call(tclchar,
                     c(list(if (mode == "save") "tk_getSaveFile" else "tk_getOpenFile",
                            title = title,
                            parent = GUI$plot,
                            initialfile = basename(init.file),
                            initialdir = dirname(init.file),
                            filetypes = types),
                       if (mode != "save") list(multiple = mode == "open.many") else NULL))

  } else {
    files <- tclchar("tk_chooseDirectory",
                     title = title,
                     initialdir = init.file)
  }
  ## restore the focus to where it was before
  tcl("focus", focus.win)

  if (mode != "open.many")
    ## We must paste space-separated pieces
    ## except when mode is open.many
    files <- paste(files, collapse=" ")

  if (length(files) > 0)
    GUI$default.dir <- if (mode != "open.dir") dirname(files[1]) else files[1]

  if (!is.null(on.done))
    on.done(files)

  return(files)
}

gui.locate.point <- function (title=NULL, msg=NULL, timeout=30, keys=c("Escape", "space")) {
  ## Popup a message and wait for the user to hit one of the specified
  ## keys in the plot window.
  ## Return a list with these items:
  ##   $coords: the plot window coordinates
  ##   $key: the name of the key the user hit
  ##
  ## If no key is hit within the specified timeout,
  ## return NULL.
  
  plot <- ".plot"

  tcl("after", timeout * 1000,
      function() {
        if (is.null(GUI$key.hit))
          GUI$key.hit <- NA
      })

  ## save bindings for the window
  save.bindings <- list()

  for (k in keys) {
    key.evt <- "<" %:% k %:% ">"
    save.bindings$k <- tcl("bind", plot, key.evt)
    tcl("bind", plot, key.evt,
        rss.make.closure(function() {
          GUI$marked.point <- tclint("winfo", "pointerxy", plot) - c(tclint("winfo", "rootx", plot), tclint("winfo", "rooty", plot))
          GUI$key.hit <- k
        }, list (k=k, plot=plot))
    )
  }
  GUI$key.hit <- NULL
  tcl("raise", plot)
  tcl("focus", "-force", plot)
  if (!is.null(title) && !is.null(msg))
    msg.id <- gui.popup.message(title=title, msg=msg, time.to.live=timeout)
  else
    msg.id <- NULL
  
  while(is.null(GUI$key.hit)) {
    tcl("update")
  }
  if (!is.null(msg.id))
    gui.delete.message(msg.id)

  ## restore any key bindings for the window
  for (k in keys) {
    key.evt <- "<" %:% k %:% ">"
    tcl("bind", plot, key.evt, save.bindings$k)
  }
  if (is.na(GUI$key.hit)) {
    rv <- NULL
  } else {
    rv <-list(coords=GUI$marked.point, key=GUI$key.hit)
  }
  GUI$marked.point <- NULL
  GUI$key.hit <- NULL
  return(rv)
}

gui.locate.line <- function (title, msg, timeout=120) {
  ## Create a line item in the plot window, and let the user select its endpoints.
  ## The user switches between endpoints by hitting Space, selects the current line
  ## with Enter, or cancels with Escape.
  ## Returns the screen coordinates of the line endpoints, or NULL if the user cancels.

  is.tk <- GUI$plot.is.tk.image
  if (!is.tk) {
    gui.set.plot.is.tk.image(TRUE)
    rss.gui(UPDATE_GUI)
  }

  lol <- "locateline"

  GUI$located.line.endpoint <- 0
  
  tcl(GUI$plot, "create", "line", rep(gui.plot.window.centre(), 2), tags=c(lol, "pan", "zoom", "rotate"), fill=GUI$locate.line.colour)

  msg.id <- gui.popup.message(title=title, msg=msg %:% "\nKeys:  Tab=switch between endpoints;  Space=finish;  Escape=cancel", time.to.live=timeout)

  rss.add.hook("PLOT_CURSOR_MOVED", lol,
               function(coords,...) {
                 old <- tclint(GUI$plot, "coords", lol)
                 old[GUI$located.line.endpoint + (1:2)] <- coords
                 tcl(GUI$plot, "coords", lol, old)
                 return(NULL)
               })
               
  repeat {
    pt <- gui.locate.point(timeout=timeout, keys=c("Tab", "Escape", "space"))
    if (pt$key != "Tab")
      break
    ## swap the endpoints
    GUI$located.line.endpoint <- 2 - GUI$located.line.endpoint
  }

  if (pt$key == "Escape") {
    rv <- NULL
  } else {
    rv <- tclint(GUI$plot, "coords", lol)
  }
  rss.drop.hook("PLOT_CURSOR_MOVED", lol)
  GUI$located.line.endpoint <- NULL
  tcl(GUI$plot, "delete", lol)
  if (!is.tk)
    gui.set.plot.is.tk.image(FALSE)
  gui.delete.message(msg.id)
  return(rv)
}

gui.after.stop <- function() {
  ## once play has truly stopped; ie. scan processing is finished,
  ## update the GUI to reflect this
  TCL(".play.play deselect
       .play.play configure -state normal
       .play.tostart configure -state normal
       .play.toend configure -state normal
       .play.play1 configure -state normal
       .play.pause deselect
       .play.pause configure -state disabled
       .play.stop deselect
       .play.stop configure -state disabled
       .play.playlist configure -state normal
       .play.source configure -state normal
       .play.sink configure -state normal")
  if (!is.null(RSS$sink))
    TCL(".play.record configure -state normal")
  GUI$play.start.time <- NULL
  if (RSS$recording && GUI$release.record.after.stopping) {
    TCL(".play.record deselect")
    RSS$recording <- FALSE
  }
}

gui.enter.key.press <- function(W, x, y, X, Y, s) {
  tcl("event", "generate", W, "<KeyPress>", keysym="Return", x=x, y=y, rootx=X, rooty=Y, state=s)
}

gui.enter.key.release <- function(W, x, y, X, Y, s) {
  tcl("event", "generate", W, "<KeyRelease>", keysym="Return", x=x, y=y, rootx=X, rooty=Y, state=s)
}

################################################################################
##
## This function contains the API for the GUI, namely
## a list of all GUI events that can be invoked by the
## base radR system or generated by the user via keystrokes or mouse events.
##
################################################################################

gui.get.event.list <- function() {
  strictenv(
            SCAN_ADVANCE =
            
            function(...) {
              ## Advance the slider up one, if it is visible.
              
              if (isTRUE(RSS$source$is.seekable)) {
                .Tcl(".play.slider set [expr 1 + [.play.slider get]]")
              } else {
                ## No slider, so update the progress display.
                if (!is.null(GUI$play.start.time)) {
                  GUI$play.num.scans <- 1 + GUI$play.num.scans
                  d<-round(difftime(Sys.time(), GUI$play.start.time), digits=2)
                  tcl(".play.progbar", "configure", text=sprintf("Scans: %6d  Elapsed time: %s", GUI$play.num.scans, paste(d, attr(d, "units"))))
                }
              }
            },
                        
            SET_PROGRESS_MAX =
            function(x)
            {
              TCL(".play.slider configure -to $x")
            },
            
            RESET_PROGRESS           = gui.reset.slider,
            
            CREATE_WINDOWS           = gui.create.windows,

            RESTORE_WINDOW_SETTINGS  = gui.restore.window.settings,

            UPDATE_PLOT_PARMS        = gui.update.plot.parms,

            UPDATE_PLOT_WINDOW       = gui.update.plot.window,

            UPDATE_PLOT_WINDOW_TITLE = gui.set.plot.window.title,

            SAVE_CONFIG              = gui.save.config,

            CONFIRM_QUIT             = gui.confirm.quit,
            
            HAVE_PLUGINS             = gui.create.plugin.menu,
            
            PLUGIN_LOADED            = gui.plugin.loaded,

            PLUGIN_UNLOADED          = gui.plugin.unloaded,

            SHOW_ERROR               = gui.error,

            SHOW_BUSY                = gui.show.busy,

            ENABLE_CONTROLS          = gui.enable.controls,

            POPUP_DIALOG             = gui.popup.dialog,

            PORT_SET                 = gui.set.port,

            SET_NO_PORT              = gui.set.no.port,

            SET_PLOT_CURSOR          = gui.set.plot.cursor,

            PALETTE_CHANGED          = gui.class.palette.changed,

            POPUP_RADR_MENU = function(X, Y) {
              tcl(".cmd", "post", X, Y)
            },

            ZOOM_IN_PLOT_STEP = function() {
              gui.do.zoom(1)
            },

            ZOOM_OUT_PLOT_STEP = function() {
              gui.do.zoom(-1)
            },

            ZOOM_BY_STEPS = function(x) {
              gui.do.zoom (as.integer(x) / GUI$mousewheel.step )  ## now matches google maps
            },

            ZOOM_TO_LEVEL = function(x) {
              gui.do.zoom(as.real(x), FALSE)
            },

            ZOOM_TO_PATCH = gui.zoom.to.patch,
            
            PLOT_TO_DEFAULT_VIEW = function() {
              old.mpp <- gui.parm(mpp)
              gui.new.parm(mpp, GUI$default.mpp)
              old.angle <- gui.parm(north.angle)
              gui.new.parm(north.angle, GUI$default.north.angle)
              gui.new.parm(compass.radius, GUI$compass.default.radius)
              gui.set.compass.radius(GUI$compass.default.radius)
              gui.set.plot.origin(gui.plot.window.centre(), TRUE)
            },

            ROTATE_PLOT_SMALL_STEP = function(x) {
              gui.do.rotate (- as.integer(x) / GUI$mousewheel.step)
            },

            ROTATE_PLOT_BIG_STEP = function(x) {
              gui.do.rotate (- as.integer(x) / GUI$mousewheel.step * GUI$plot.big.rotation.increment)
            },

            GENERATE_MOUSEWHEEL_UP = function(W, X, Y) {
              tcl("event", "generate", W, "<MouseWheel>", delta = GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_MOUSEWHEEL_DOWN = function(W, X, Y) {
              tcl("event", "generate", W, "<MouseWheel>", delta = - GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_CONTROL_MOUSEWHEEL_UP= function(W, X, Y) {
              tcl("event", "generate", W, "<Control-MouseWheel>", delta = GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_CONTROL_MOUSEWHEEL_DOWN= function(W, X, Y) {
              tcl("event", "generate", W, "<Control-MouseWheel>", delta = - GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_SHIFT_MOUSEWHEEL_UP= function(W, X, Y) {
              tcl("event", "generate", W, "<Shift-MouseWheel>", delta = GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_SHIFT_MOUSEWHEEL_DOWN= function(W, X, Y) {
              tcl("event", "generate", W, "<Shift-MouseWheel>", delta = - GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_CONTROL_SHIFT_MOUSEWHEEL_UP= function(W, X, Y) {
              tcl("event", "generate", W, "<Control-Shift-MouseWheel>", delta = GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            GENERATE_CONTROL_SHIFT_MOUSEWHEEL_DOWN= function(W, X, Y) {
              tcl("event", "generate", W, "<Control-Shift-MouseWheel>", delta = - GUI$mousewheel.step, rootx=X, rooty=Y)
            },

            SET_PLOT_ORIGIN = function(x, y) {
              gui.set.plot.origin(gui.plot.window.centre() - as.real(c(x,y)), FALSE)
            },

            CENTRE_PLOT_AT_RADAR = function() {
              gui.set.plot.origin(gui.plot.window.centre(), TRUE)
            },

            START_PLOT_DRAG = function(x, y){
              GUI$drag.origin <- as.integer(c(x, y))
            },
           
            END_PLOT_DRAG = function(x, y) {
              new.coords <- as.integer(c(x, y))
              if (!is.null(GUI$drag.origin) && !all(GUI$drag.origin == new.coords) ) {
                gui.set.plot.origin(new.coords - GUI$drag.origin, FALSE)
                GUI$drag.origin <- NULL
              }
            },

            START_INFO_WIN_DRAG = function(X, Y) {
                GUI$drag.origin <- as.integer(c(X, Y))
            },

            CONTINUE_INFO_WIN_DRAG = function(X, Y) {
              new.coords <- as.integer(c(X, Y))
              if (!is.null(GUI$drag.origin) && !all(GUI$drag.origin == new.coords) ) {
                GUI$info.window.coords <- GUI$info.window.coords + (new.coords - GUI$drag.origin)
                GUI$drag.origin <- new.coords
                tcl("place", GUI$info, x=GUI$info.window.coords[1], y=GUI$info.window.coords[2], anchor="nw")
                gui.show.pointer.info(gui.root.to.win.coords(as.integer(c(X, Y))))
              }
            },
              
            END_INFO_WIN_DRAG = function(X, Y) {
              new.coords <- as.integer(c(X, Y))
              if (!is.null(GUI$drag.origin) && !all(GUI$drag.origin == new.coords) ) {
                GUI$info.window.coords <- GUI$info.window.coords + (new.coords - GUI$drag.origin)
                gui.show.pointer.info()
                GUI$drag.origin <- NULL
              }
            },

            DRAG_RANGE_RINGS = function(x, y) {
              radius <- GUI$tx.plot.to.spatial(as.integer(c(x, y)))$rb[1]
              delta <- tclreal(".pctl.rangeringslider.spin", "cget", "-increment")
              tcl(".pctl.rangeringslider.spin", "set", max(delta, round(radius / (max(1, min(GUI$num.range.rings, round(radius / GUI$range.ring.spacing)))) + delta)))
              tcl(".pctl.rangeringslider.spin", "invoke", "buttondown")
              tcl("canvas_showhide_big_arc", GUI$plot, "zoom_arc")
              GUI$drag.origin <- NULL
            },

            DRAG_COMPASS_RING = function(x, y) {
              radius <- sqrt(sum((as.integer(c(x, y)) - GUI$plot.origin)^2))
              delta <- tclreal(".pctl.compassslider.spin", "cget", "-increment")
              tcl(".pctl.compassslider.spin", "set", round(radius) + delta)
              tcl(".pctl.compassslider.spin", "invoke", "buttondown")
              GUI$drag.origin <- NULL
            },

            SHOW_POINTER_INFO = function(X, Y, s) {
              GUI$last.motion.state <- as.integer(s)
              gui.show.pointer.info(GUI$last.pointer.coords <- gui.root.to.win.coords(as.integer(c(X, Y))))
            },

            ENABLE_POINTER_INFO = function(X, Y){
              if (GUI$info.window.follows.mouse) {
                GUI$mouse.in.plot <- TRUE
                gui.show.pointer.info(gui.root.to.win.coords(as.integer(c(X, Y))))
              }
            },

            DISABLE_POINTER_INFO = function() {
              if (GUI$info.window.follows.mouse) {
                GUI$mouse.in.plot <- FALSE
                gui.show.pointer.info(NULL)
              }
            },

            SET_POINTER_INFO = function(s) {
              tcl(GUI$info, "configure", text=s)
            },
            
            START_PLAY = function(..., user.generated = FALSE) {
              TCL(" .play.play configure -state disabled
                       .play.play1 configure -state disabled
                       .play.tostart configure -state disabled
                       .play.toend configure -state disabled
                       .play.pause deselect
                       .play.pause configure -state normal
                       .play.stop configure -state normal
                       .play.playlist configure -state disabled
                       .play.source configure -state disabled
                       .play.record configure -state disabled
                       .play.sink configure -state disabled")
              if (is.null(GUI$play.start.time)) {
                GUI$play.start.time <- Sys.time()
                GUI$play.num.scans <- 0
              }
              RSS$new.play.state <- RSS$PS$PLAYING
            },

            PLAY_ONE_SCAN = function() {
              TCL(" .play.pause select
                       .play.stop configure -state normal
                       .play.playlist configure -state disabled
      		       .play.source configure -state disabled
                       .play.tostart configure -state disabled
                       .play.toend configure -state disabled
                       .play.sink configure -state disabled
                       .play.pause configure -state disabled")
              if (is.null(GUI$play.start.time)) {
                GUI$play.start.time <- Sys.time()
                GUI$play.num.scans <- 0
              }
              RSS$new.play.state <- RSS$PS$PLAY.ONE.SCAN
            },

            PAUSE_PLAY = function(..., user.generated=TRUE) {
              if (!user.generated)
                TCL(".play.pause select")
              TCL(".play.play deselect
                   .play.play configure -state normal
                   .play.play1 configure -state normal
                   .play.pause configure -state disabled")
              if (!is.null(RSS$sink))
                TCL(".play.record configure -state normal")
              RSS$new.play.state <- RSS$PS$PAUSED
            },

            STOP_PLAY = function(..., user.generated=TRUE) {
              if (!user.generated)
                TCL(".play.stop select")
              if (RSS$play.state >= RSS$PS$PLAYING)
                rss.defer.call(gui.after.stop())
              else
                gui.after.stop()
              RSS$new.play.state <- RSS$PS$STOPPED
            },

            TO_START = function() {
              TCL(".play.slider set 0")
            },

            TO_END = function() {
              TCL(".play.slider set [.play.slider cget -to]")
            },

            TOGGLE_RECORDING = function() {
              RSS$recording <- ! RSS$recording
            },

            PLAY_SLIDER_MOVED = function() {
              gui.new.slider.pos()
            },

            SEEK_SCAN_FWD_REV = function(x) {
              x <- - as.integer(x) / GUI$mousewheel.step
              .Tcl(".play.slider set [expr [.play.slider get] +" %:% x %:%"]")
            },
            
            SCROLL_TEXT_WINDOW = function(W, x) {
              tcl(W, "yview", "scroll", - round(as.integer(x) / GUI$mousewheel.step * 5), "units")
            },

            MARK_PLOT_POINT = function(x, y) {
              rss.call.hooks(RSS$MARK_PLOT_POINT_HOOK, GUI$tx.plot.to.spatial(as.integer(c(x, y))))
            },
              
            ## a do-nothing event (for effectively deleting tcl default bindings)
            IGNORE = function(...) {},

            SET_CLASS_DISPLAY = gui.set.class.display,

            SET_CLASS_PALETTE = gui.set.class.palette,

            FILE_DIALOG = gui.file.dialog,

            UPDATE_GUI = function(...){ tcl("update"); for (i in 1:100) eval(expression(1+1))},

            CONSOLE_MESSAGE = gui.put.cons,

            CONSOLE_PRINT = gui.print.cons,

            POPUP_MESSAGEBOX = gui.popup.message,

            DELETE_MESSAGEBOX = gui.delete.message,

            SHOW_GRAPH = gui.show.graph,

            DELETE_GRAPH = gui.delete.graph,

            LOCATE_POINT = gui.locate.point,

            LOCATE_LINE = gui.locate.line,

            ENTER_KEY_PRESS = gui.enter.key.press,

            ENTER_KEY_RELEASE = gui.enter.key.release,

            POPUP_NEW_MENU = gui.popup.new.menu,

            DESTROY_MENU = gui.destroy.menu,
            
            PARENT = .GlobalEnv ## not an event
            )
}

###debug(continue.gui.create.plot.window)
###debug(gui.set.plot.is.tk.image)
###debug(gui.set.plot.window)
