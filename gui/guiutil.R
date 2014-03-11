##  svn $Id: guiutil.R 677 2010-11-04 16:07:27Z john $
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

## Utility functions for the radR GUI in Tcl/Tk

## FIXME:  a continuation of the HORRIBLE MESS described in gui.R

## conversions shortcuts for Tcl commands and objects into
## numeric and logical values; these go via as.character()

as.logical.tclObj <- function(x)    as.logical(as.integer(as.character(x)))
as.numeric.tclObj <- function(x)    as.numeric(as.character(x))
as.integer.tclObj <- function(x)    as.integer(as.character(x))
tclint            <- function(...)  as.integer(tcl(...))
tclchar           <- function(...)  as.character(tcl(...))
tcl1char          <- function(...)  paste(as.character(tcl(...)), collapse=" ")
tclreal           <- function(...)  as.real(tcl(...))
tclbool           <- function(...)  as.logical(tcl(...))
Tclint            <- function(s)    as.integer(.Tcl(s))
Tclchar           <- function(s)    as.character(.Tcl(s))
Tclreal           <- function(s)    as.real(.Tcl(s))
Tclbool           <- function(s)    as.logical(.Tcl(s))
tclintvar         <- function(v)    as.integer(tclvalue(v))
tclboolvar        <- function(v)    as.logical(as.integer(tclvalue(v)))
tclrealvar        <- function(v)    as.real(tclvalue(v))
tclcharvar        <- function(v)    tclvalue(v)

## a function to call tcl after doing substitution
## for expressions beginning with "$".
## Every such expression is evaluated in the caller's frame.
## in order, before the resulting long string is sent to .Tcl
## The tcl return value is converted to a character
## vector.

TCL <- function(x) {
  ## quick escape if no dollar signs
  if (length(grep("$", x, fixed=TRUE)) == 0)
    return (as.character(.Tcl(x)))
  
  wd <- strsplit(x, "([ \t])+|(?=\n)", perl=TRUE)[[1]]
  for (i in grep("^\\$", wd)) {
    wd[i] <- eval(parse(text=substring(wd[i], 2)), parent.frame())
  }
  as.character(.Tcl(paste(wd, collapse=" ")))
}

gui.handle.event <- function(event, ...) {
  if (GUI$enabled) {
    if (event %in% GUI$event.list)
      GUI$event.list[[event]](...)
    else
      warning("Unknown GUI event " %:% event)
  }
}

gui.mn.bind <- function(window, evt, fun, m=1, n=0) {
  ## bind a function to an event such that it is only
  ## invoked on the m-th, m+n-th, m+2n-th and so on
  ## events.  If n is zero, the function is only
  ## called once:  on the m-th event.

  tmp.fun <- function() {
    m <<- m - 1
    if (m == 0) {
      ## event count has been reached; if this is
      ## not a recurring event, unbind the function
      ## otherwise reset the counter
      if (n == 0)
        tcl("bind", window, evt, "")
      else
        m <<- n
      do.call(fun, lapply(as.list(sys.call())[-1], function(x)eval(x,parent.frame())))
    }
  }
  formals(tmp.fun) <- formals(fun)
  tcl("bind", window, evt, tmp.fun)
}

gui.skip.event <- function(window, evt, n=1) {
  ## force tcl to skip handling of the next n EVT events in
  ## window WINDOW; n must be zero or greater, but if zero,
  ## no events are skipped

  if (n > 0) {
    ## save the handler, if any
    saved.handler <- tcl("bind", window, evt)
    f <- function() {
      ## drop the count
      n <<- n - 1
      if (n <= 0)
        ## restore the handler
        tcl("bind", window, evt, saved.handler)
    }
    tcl("bind", window, evt, f)
  }
}
  
gui.disable.event <- function(window, evt) {
  ## force tcl to disable handling the given event
  ## for the given window/tag
  ## the existing handler is saved and can be restored by
  ## calling gui.enable.event
  ## Multiple calls to gui.disable.event() have a cumulative effect:
  ## an equal number of calls to gui.enable.event() must be made
  ## in order to restore the original event handler.

  ## save the handler, if any
  saved.handler <- tcl("bind", window, evt)
  f <- function(W) {
    ## if the handler has been called explictly and
    ## provided with the argument "__restore__" instead
    ## of a window name, then
    ## restore the original handler
    if (W == "__restore__")
      tcl("bind", window, evt, saved.handler)
    ## otherwise, do nothing
  }
  tcl("bind", window, evt, rss.make.closure(f, list(window=window, evt=evt, saved.handler=saved.handler)))
}

gui.enable.event <- function(window, evt) {
  ## explicitly call the handler installed by gui.disable.event
  ## with the argument W="__restore__", to signal that
  ## it should reinstall the original handler
  cmd <-tclchar("bind", window, evt)
  do.call("tcl", as.list(c(cmd[-length(cmd)], "__restore__")))
}

gui.new.tearoff <- function(...) {
  args <- as.character(list(...))
  win <- args[[2]]
  tcl("wm", "iconbitmap", win, GUI$application.icon)
  if (length(paste(tclchar(win, "entrycget", 1, "-label"), collapse=" ") %~% "\\((close menu\\))"))
    tcl(win, "delete", 1)
  ## force the new toplevel menu window into a position so that its title bar is under the pointer
  ## unless told to skip this (e.g. when loading a plugin, where appearance of the menu might be delayed)
  if ( is.null(GUI$skip.relocate.tearoff)) {
    tcl("wm", "geometry", win, paste("+", GUI$tearoff.menu.window.offset + tclint("winfo", "pointerxy", "."), sep="", collapse=""))
  } else {
    GUI$skip.relocate.tearoff <- NULL
    gui.centre.window(win)
  }
}
      
gui.menu.from.list <- function(name, x, title="", tearoffcommand=function(...)gui.new.tearoff(...), env=.GlobalEnv) {
  ## create a menu from x according to the rules
  ## described in generic.plugin.R
  ## name is the tcl window path name, and should begin
  ## with "."
  ## FIXME: the rules should be here, and referred
  ## to by the generic plugin!
  ## Returns name on success, NULL on failure.

  tcl("destroy", name)
  tcl("menu", name, title=title, tearoffcommand=tearoffcommand)
  if (is.null(GUI$menu.title.to.tcl.part[[title]]))
    GUI$menu.title.to.tcl.part[[title]] <- tail(strsplit(name, ".", fixed=TRUE)[[1]], 1)
  GUI$menu.title.to.tcl.path[[title]] <- name
  names <- names(x)
  num.vars <- 1
  num.sub.menus <- 1
  for (i in seq(along=x)) {
    switch(EXPR=class(x[[i]]),
           ##==============================================================================
           ## function - just a command entry
           ##==============================================================================

           "function" = {
             ## add a command entry
             tcl(name, "add", "command", label = names[[i]], command = x[[i]])
           },
           ##==============================================================================
           ## character - a separator or null command a.k.a. heading
           ##==============================================================================

           "character" = {
             ## add a separator, label (i.e. inactive command), or cascade
             ## to a named menu 
             if (identical(x[[i]], "---"))
               tcl(name, "add", "separator")
             else if (identical(substring(x[[i]], 1, 1), "."))
               tcl(name, "add", "cascade", menu=x[[i]], label=names[[i]])
             else if (names[i] == "options") {
               ## handle menu options, a character vector of options
               ## So far, the only one is "no-tearoff"
               for (o in x[[i]]) {
                 if (o == "no-tearoff")
                   tcl(name, "configure", tearoff=0)
               }
             } else {
               ## this is a heading.  Disable it and make it black on white
               tcl(name, "add", "command", label=x[[i]])
               tcl(name, "entryconfigure", "last", state="disabled")
               tcl(name, "entryconfigure", "last", background=GUI$menu.heading.background)
             }
           },
           ##==============================================================================
           ## list - a complex entry
           ##==============================================================================

           "list" = {
             switch(EXPR=x[[i]][[1]],
                    
                    ##==============================================================================
                    ## list : choose.one - a group of radio buttons
                    ##==============================================================================
                    
                    "choose.one" = {
                      ## add a group of radio buttons
                      y <- x[[i]][-1]
                      ## remove the option items
                      on.set <- y$on.set
                      set.or.get <- y$set.or.get
                      varname <- y$group
                      y$on.set <- NULL
                      y$set.or.get <- NULL
                      y$group <- NULL
                      
                      ## create the getter/setter function
                      if (is.null(varname)) {
                        varname <- "value" %:% name %:% "." %:% num.vars
                        num.vars <- num.vars + 1
                      }
                      getset <- rss.make.closure(
                                                 function(n) {
                                                   if (missing(n))
                                                     return(tclintvar(varname))
                                                   else
                                                     tcl("set", varname, n)
                                                 },
                                                 list(varname = varname)
                                                 )
                      if (!is.null(set.or.get))
                        assign(set.or.get, getset, env)
                      
                      ## create menu entries for each item
                      for (j in seq(along=y)) {
                        tcl(name, "add", "radiobutton", variable=varname, value = j,
                            label = ifelse(is.logical(y[[j]]), names(y)[[j]], y[[j]]),
                            command = rss.make.closure(
                              function() {
                                if (!is.null(on.set))
                                  on.set(set.or.get())
                              },
                              list(set.or.get = getset,
                                   on.set = on.set)
                              )
                            )
                        if (is.logical(y[[j]]) && y[[j]])
                          getset(j)
                      }
                    },

                    ##==============================================================================
                    ## list : choose.any - a group of independent options
                    ##==============================================================================

                    "choose.any" = {
                      ## add a group of check buttons
                      ## independent of each other, but get/settable using
                      ## the same R closure
                      y <- x[[i]][-1]
                      ## remove the option items
                      on.set <- y$on.set
                      set.or.get <- y$set.or.get
                      y$on.set <- NULL
                      y$set.or.get <- NULL
                      ## create the getter/setter function
                      varname <- "value" %:% name %:% "." %:% num.vars
                      num.vars <- num.vars + 1
                      getset <- rss.make.closure(
                                                 function(n, state) {
                                                   if (missing(state))
                                                     return(tclboolvar(varname %:% "." %:% n ))
                                                   else
                                                     tcl("set", varname %:% "." %:% n, as.numeric(state))
                                                 },
                                                 list(varname=varname)
                                                 )
                      if (!is.null(set.or.get))
                        assign(set.or.get, getset, env)
                      ## create menu entries for each item
                      for (j in seq(along=y)) {
                        tcl(name, "add", "checkbutton", variable=varname %:% "." %:% j,
                            label = ifelse(is.logical(y[[j]]), names(y)[[j]], y[[j]]),
                            command = rss.make.closure(
                              function() {
                                if (!is.null(on.set))
                                  on.set(which, set.or.get(which))
                              },
                              list(set.or.get=getset,
                                   on.set=on.set,
                                   which = j)
                              )
                            )
                        if (is.logical(y[[j]]))
                          getset(j, y[[j]])
                      }
                    },
                    ##==============================================================================
                    ## list : menu - a cascading submenu
                    ##==============================================================================

                    "menu" = {
                      if (is.null(names(x[[i]])[1])) {
                        sub.name = name %:% "." %:% num.sub.menus
                        num.sub.menus <- num.sub.menus + 1
                      } else {
                        sub.name = name %:% "." %:% names(x[[i]])[1]
                      }
                      tcl(name, "add", "cascade", menu = sub.name, label = names[[i]])
                      gui.menu.from.list(sub.name, x[[i]][-1], names[[i]])
                    },
                    
                    ##==============================================================================
                    ## list : dyn.menu - a cascading submenu with content generated at post time
                    ##==============================================================================

                    "dyn.menu" = {
                      ## add an entry that pops up a dynamically generated menu
                      ## The other item in the list after "dyn.menu" is a function which
                      ## returns a character vector suitable for calling gui.menu.from.list on it.
                      ##
                      if (is.null(names(x[[i]])[1])) {
                        sub.name = name %:% "." %:% num.sub.menus
                        num.sub.menus <- num.sub.menus + 1
                      } else {
                        sub.name = name %:% "." %:% names(x[[i]])[1]
                      }
                      tcl(name, "add", "command", label = names[[i]], command=rss.make.closure(function() {
                        tcl("destroy", sub.name)
                        gui.menu.from.list(sub.name, f(), n)
                        tcl(sub.name, "post", tcl("winfo", "pointerx", "."), tcl("winfo", "pointery", "."))
                       }, list(f=x[[i]][[2]], n=names[[i]],sub.name=sub.name)))
                    },
                    
                    ##==============================================================================
                    ## list : file - a file selection command
                    ##==============================================================================

                    "file" = {
                      ## add an entry that calls up a file selection dialog
                      ## The list items are:
                      ## type:  a character vector indicating what kind of file selection dialog, one of:
                      ##
                      ##    open.one  - open a single existing file
                      ##    open.many - open multiple existing files
                      ##    save      - open a file for saving
                      ##    open.dir  - select a directory
                      ##
                      ## title: the title to display in the file dialog
                      ##
                      ## file.types: a tagged character vector or list of file types
                      ##
                      ## init.file: a function returning the full path to the initial
                      ##            default file or directory
                      ##
                      ## on.set: a function to call when a filename(s) (or directory) has
                      ##         been selected.  Takes one argument, a character vector of
                      ##         filenames; e.g.
                      ##
                      ## e.g.:
                      ##
                      ##     list("file",
                      ##          title="Open an image file",
                      ##          type="open.one",
                      ##          file.types = list(".gif" = "GIF images", ".*" = "All files"),
                      ##          on.set = function(f) gui.print.cons("You chose " %:% paste(f, collapse=","))
                      ##          )
                      ##
 
                      y <- x[[i]][-1]
                      f <- rss.make.closure(
                                            function() {
                                              ## this is the function that will be called by invoking
                                              ## the list : file menu entry
                                              gui.file.dialog(y$type, y$title, y$file.types, y$init.file, y$on.set)
                                            }, list(y=y))
                      
                      tcl(name, "add", "command", label=names[[i]], command=f)               
                    },
                    ##==============================================================================
                    ## list : do.one - a set of related commands
                    ##==============================================================================
                    
                    "do.one" = {
                      ## add a group of command entries
                      ## which call the same function with a different
                      ## index
                      ## The list items are:
                      ## on.do: the function called when an item is selected
                      ## the rest of the list:  if this has names, these are the menu labels
                      ##                        and the item values are passed to on.do()
                      ##                        Otherwise, the item values are labels, and the
                      ##                        numbers 1, 2, ... are passed to on.do()

                      y <- x[[i]][-1]
                      on.do <- y$on.do
                      y$on.do <- NULL
                      ## create menu entries for each item
                      
                      if (is.null(names(y)) || all(nchar(names(y)) == 0)) {
                        labs <- y
                        vals <- seq(along=y)
                      } else {
                        labs <- names(y)
                        vals <- y
                      }
                      for (j in seq(along=vals)) {
                        tcl(name, "add", "command", label = labs[[j]],
                            command = rss.make.closure(
                              function() {
                                if (!is.null(on.do))
                                  on.do(arg)
                              },
                              list(arg=vals[[j]], on.do=on.do)
                              )
                            )
                      }
                    },
                    ##==============================================================================
                    ## list : gauge - a numeric value that can be set
                    ##==============================================================================
                    
                    "gauge" = {
                      ## add a gauge, which is a command entry that pops
                      ## up a toplevel window with a labelled gauge for
                      ## choosing a value.  If a menu has more than one gauge,
                      ## all of them will appear in the same toplevel window.
                      ## The window title will be the menu title followed by " controls"
                      ## The window's tcl name will be the last component of the menu's name
                      ## followed by "_controls"
                      ## "MENU" is the tcl name of the menu with "."s removed.
                      ##
                      ## The required list items are:
                      ## label: for the gauge in the popup window; the menu item label
                      ##        is paste("Set ", label, "...")
                      ## range: minimum and maximum values for the gauge
                      ## increment: amount by which up/down buttons change the value
                      ## format:  if not NULL, specify a floating point format like "%3.3f"
                      ## value: initial value
                      ## on.set: function of a single variable called with the new gauge
                      ##         value whenever it changes
                      ## set.or.get: the name of a function to be created that accepts
                      ##         0 or 1 arguments; with 0, it reports the current value of
                      ##         the gauge.  With 1, it sets the value of the gauge (without
                      ##         calling on.set).

                      y <- x[[i]][-1]
                      win <- "." %:% tail(strsplit(name, ".", fixed=TRUE)[[1]],1) %:% "_controls"
                      if (!tclbool("winfo", "exists", win)) {
                        .Tcl("toplevel " %:% win %:% "; wm withdraw " %:% win)
                        tcl("wm", "iconbitmap", win, GUI$application.icon)
                        tcl("wm", "title", win, paste(tclchar(name, "cget", "-title"), collapse=" ") %:% " controls")
                        tcl("wm", "protocol", win, "WM_DELETE_WINDOW",
                            function(){
                              tcl("wm", "withdraw", win)
                            })
                        ## add a trace to ensure the controls window is deleted when the menu
                        ## is destroyed (e.g. at plugin unload)
                        tcl("trace", "add", "command", name, "delete", function(...) tcl("destroy", win))
                        tcl(name, "add", "command", label="Controls...",
                            command=rss.make.closure(
                              function() {
                                tcl("wm", "deiconify", win)
                                tcl("wm", "geometry", win, paste("+", GUI$tearoff.menu.window.offset + tclint("winfo", "pointerxy", "."), sep="", collapse=""))
                                tcl("raise", win)
                              },
                              list(win=win))
                            )
                      }
                      gauge <- gui.create.gauge(parent=win, label=y$label, range=y$range, increment=y$increment, val=y$value,
                                                y$on.set, setter.name=y$set.or.get, setter.env=env, format=y$format)
                      tcl("pack", gauge, side="top", anchor="e")
                    },
                    ##==============================================================================
                    ## list : string - a string value that can be set
                    ##==============================================================================
                    
                    "string" = {
                      ## add a string entry box, which is added to the same toplevel window as
                      ## gauges are.  The entire contents of the entry box are treated as a single
                      ## line.
                      ## The window title will be the menu title followed by " controls"
                      ## The window's tcl name will be the last component of the menu's name
                      ## followed by "_controls"
                      ## "MENU" is the tcl name of the menu with "."s removed.
                      ##
                      ## The required list items are:
                      ## label: for the gauge in the popup window; the menu item label
                      ##        is paste("Set ", label, "...")
                      ## width: number of spaces to leave for the window
                      ## value: initial value
                      ## val.check: function of a single variable for checking the user's input
                      ##         the returned value should be NULL if there is an error, otherwise
                      ##         it becomes the value of the gauge which is sent to on.set
                      ##         and returned by set.or.get
                      ## on.set: function of a single variable called with the new gauge
                      ##         value whenever it changes
                      ## set.or.get: the name of a function to be created that accepts
                      ##         0 or 1 arguments; with 0, it reports the current value of
                      ##         the gauge.  With 1, it sets the value of the gauge (without
                      ##         calling on.set).

                      y <- x[[i]][-1]
                      win <- "." %:% tail(strsplit(name, ".", fixed=TRUE)[[1]],1) %:% "_controls"
                      if (!tclbool("winfo", "exists", win)) {
                        .Tcl("toplevel " %:% win %:% "; wm withdraw " %:% win)
                        tcl("wm", "iconbitmap", win, GUI$application.icon)
                        tcl("wm", "title", win, paste(tclchar(name, "cget", "-title"), collapse=" ") %:% " controls")
                        tcl("wm", "protocol", win, "WM_DELETE_WINDOW",
                            function(){
                              tcl("wm", "withdraw", win)
                            })
                        ## add a trace to ensure the controls window is deleted when the menu
                        ## is destroyed (e.g. at plugin unload)
                        tcl("trace", "add", "command", name, "delete", function(...) tcl("destroy", win))
                        tcl(name, "add", "command", label="Controls...",
                            command=rss.make.closure(function() {tcl("wm", "deiconify", win); tcl("raise", win)},
                              list(win=win))
                            )
                      }
                      string <- gui.create.string(parent=win,
                                                  label=y$label,
                                                  width=if (is.null(y$width)) 20 else y$width,
                                                  height=if (is.null(y$height)) 20 else y$height,
                                                  val=y$value,
                                                  val.check = y$val.check,
                                                  y$on.set,
                                                  setter.name=y$set.or.get,
                                                  setter.env=env)
                      tcl("pack", string, side="top", anchor="e")
                    },
                    ##==============================================================================
                    ## list : datetime - a datetime selection widget
                    ##==============================================================================
                    
                    "datetime" = {
                      ## add a datetime entry box, which is added to the same toplevel window as
                      ## gauges are.  The contents of the entry box are treated as a formatted date.
                      ## The window title will be the menu title followed by " controls"
                      ## The window's tcl name will be the last component of the menu's name
                      ## followed by "_controls"
                      ## "MENU" is the tcl name of the menu with "."s removed.
                      ##
                      ## The required list items are:
                      ## label: for the gauge in the popup window; the menu item label
                      ##        is paste("Set ", label, "...")
                      ## value: initial value
                      ## val.check: function of a single variable for checking the user's input
                      ##         the returned value should be NULL if there is an error, otherwise
                      ##         it becomes the value of the gauge which is sent to on.set
                      ##         and returned by set.or.get
                      ## on.set: function of a single variable called with the new gauge
                      ##         value whenever it changes
                      ## set.or.get: the name of a function to be created that accepts
                      ##         0 or 1 arguments; with 0, it reports the current value of
                      ##         the gauge.  With 1, it sets the value of the gauge (without
                      ##         calling on.set).

                      y <- x[[i]][-1]
                      win <- "." %:% tail(strsplit(name, ".", fixed=TRUE)[[1]],1) %:% "_controls"
                      if (!tclbool("winfo", "exists", win)) {
                        .Tcl("toplevel " %:% win %:% "; wm withdraw " %:% win)
                        tcl("wm", "iconbitmap", win, GUI$application.icon)
                        tcl("wm", "title", win, paste(tclchar(name, "cget", "-title"), collapse=" ") %:% " controls")
                        tcl("wm", "protocol", win, "WM_DELETE_WINDOW",
                            function(){
                              tcl("wm", "withdraw", win)
                            })
                        ## add a trace to ensure the controls window is deleted when the menu
                        ## is destroyed (e.g. at plugin unload)
                        tcl("trace", "add", "command", name, "delete", function(...) tcl("destroy", win))
                        tcl(name, "add", "command", label="Controls...",
                            command=rss.make.closure(function() {tcl("wm", "deiconify", win); tcl("raise", win)},
                              list(win=win))
                            )
                      }
                      datetime <- gui.create.datetime(parent=win,
                                                  label=y$label,
                                                  val=y$value,
                                                  val.check = y$val.check,
                                                  y$on.set,
                                                  setter.name=y$set.or.get,
                                                  setter.env=env)
                      tcl("pack", datetime, side="top", anchor="e")
                    }

                    )
           })
  }
  return (name)
}

gui.popup.new.menu <- function(title, lst) {
  ## popup a menu as a dialogue box, returning
  ## the id of its toplevel window
  ## get a unique menu name
  
  i <- 0
  repeat {
    mnu.name <- ".tmpmenu" %:% i
    if (! as.integer(tcl("winfo", "exists", mnu.name)))
      break
    i <- i+1
  }

  ## create the menu 
  mnu <- gui.menu.from.list(mnu.name, lst, title)

  ## tear it off as a floating dialog
  tcl(mnu.name, "invoke", 0)
  
  return(mnu.name)
}

gui.destroy.menu <- function(id) {
  ## destroy a popped up menu (or any window, really)
  tcl("destroy", id)
}

## return a formatted version of the start time and end time
## followed by a human-readable duration
## t1 and t2 are vectors of starting and ending times

format.time.interval <- function(t1, t2) {
  len<-lapply(seq(along=t1),
           function(i){
             d<-round(difftime(t2[i], t1[i]), digits=2)
             paste(d, attr(d, "units"))
           })
  paste(paste(format(t1, format=GUI$playlist.date.format, tz=RSS$timezone, usetz=TRUE), format(t2, format=GUI$playlist.date.format, tz=RSS$timezone, usetz=TRUE), sep="  --  "), sprintf(" Length: %10s", len))
}

gui.error <- function(msg) {
  ## display an error message, either in a dialog box,
  ## or on the console.
  if (GUI$errors.logged.to.console) {
    gui.put.cons("\n" %:% msg %:% "\n", colour=GUI$console.error.colour)
  } else {
    tcl("tk_messageBox", default="ok", message=msg, title="radR error", type="ok")
  }
}   

gui.show.busy <- function(busy = TRUE) {
  ## set the display cursor to indicate we are busy
  ## processing a scan (or not, if busy==FALSE)
  ## tcl(".plot.frame", "configure", cursor=ifelse(busy, "watch", "hand2"))
  ##
  ## DO NOTHING - this is just annoying
}

gui.tx.plot.to.matrix <- function(coords) {
  ## convert xy coordinates in the plot window
  ## to row/column coordinates in the raw data
  ## i.e. to pulse number / sample number
  ## in origin 1
  ## coords: an n x 2 matrix (or a vector of length 2)
  ## returns: an n x 2 matrix (or a vector of length 2)

  if (is.matrix(coords))
    offsets <- t(t(coords) - GUI$plot.origin)
  else
    offsets <- t(coords - GUI$plot.origin)
  sample <- 1 + floor(sqrt(apply(offsets^2, 1, sum)) / gui.pps() - RSS$scan.info$first.sample.dist / RSS$scan.info$sample.dist)
  pulse <- 1 + round((RSS$scan.info$orientation * (atan2(offsets[, 1], - offsets[, 2]) / (2 * pi) - (GUI$north.angle + RSS$scan.info$bearing + RSS$scan.info$bearing.offset) / 360)) * RSS$scan.info$pulses) %% RSS$scan.info$pulses
  rv <- cbind(ifelse(sample >= 1 & sample <= RSS$scan.info$samples.per.pulse, sample, NA), ifelse(pulse >= 1 & pulse <= RSS$scan.info$pulses, pulse, NA))
  return (if (is.matrix(coords)) rv else c(rv))
}

gui.tx.plot.to.spatial <- function(coords) {
  ## given plot coordinates coords (pixels in the x and y directions)
  ## return a two element list:
  ## rv$rb: a ground range, axial range, bearing vector (metres, metres, degrees)
  ## rv$xyz: cartesian coordinates, in metres
  ## rv$t:   time, the time corresponding to the location's pulse
  ##         if there is no sample data, the time returned is NA
  ## range is planar; range per sample is measured along the antenna axis,
  ## so we must adjust range according to the antenna angle
  offsets <- coords - GUI$plot.origin
  x <- offsets[c(TRUE, FALSE)] * GUI$mpp  ## in planar metres
  y <- - offsets[c(FALSE, TRUE)] * GUI$mpp
  ground.range <- sqrt(x^2+y^2)
  ang <- rss.antenna.angle()
  z <- ground.range * tan(ang) + rss.origin.elev()
  axial.range <- ground.range / cos(ang)
  bearing <- (atan2(offsets[c(TRUE, FALSE)], -offsets[c(FALSE, TRUE)]) * 180 / pi - GUI$north.angle) %% 360
  theta <- (90 - bearing) * pi / 180
  if (RSS$have.valid$scan.data) {
    pulse <- 1 + round((RSS$scan.info$orientation * (atan2(offsets[c(TRUE, FALSE)], - offsets[c(FALSE, TRUE)]) / (2 * pi) - (GUI$north.angle + RSS$scan.info$bearing + RSS$scan.info$bearing.offset) / 360)) * RSS$scan.info$pulses) %% RSS$scan.info$pulses
    dtime <- (pulse - 1) * (RSS$scan.info$duration / (1000 * RSS$scan.info$pulses))
    time <- as.numeric(RSS$scan.info$timestamp) + dtime
  } else {
    time <- NA
  }
  return(list(rb=c(ground.range, axial.range, bearing), xyz=c(ground.range * cos(theta), ground.range * sin(theta), z), t=time))
}

gui.tx.xyz.to.plot <- function(xyz) {

  ## xyz: an n x 3 matrix of spatial x, y, z coordinates (in metres
  ## East, North, Up, relative to the radar)
  ##
  ## Value: an n x 2 matrix of plot coordinates
  ##
  ## Note: xyz is used to compute a true, 3d range.  This is
  ## treated as an axial range, because the radar would do so.
  ## No attempt is made to determine whether a target at xyz
  ## would actually be visible in the radar.
  
  theta <- atan2(xyz[,2], xyz[,1]) - GUI$north.angle * pi / 180
  planar.range <- sqrt(xyz[,1]^2 + xyz[,2]^2 + (xyz[,3] - rss.origin.elev())^2) / rss.rps() * gui.pps()
  return(cbind(GUI$plot.origin[1] + planar.range * cos(theta), GUI$plot.origin[2] - planar.range * sin(theta)))
}

gui.tx.xy.to.plot <- function(xy) {

  ## xy: an n x 2 matrix of planar spatial x, y coordinates (in metres
  ## East, North, relative to the radar)
  ## This is a simpler form of gui.xyz.to.plot.coords that uses only
  ## the planar (x,y) coordinates,
  ##
  ## Value: an n x 2 matrix of plot coordinates
  ##
  ## Note: xy is used to compute a planar range.  This is
  ## the same as axial range if RSS$antenna$angle == 0.
  
  theta <- atan2(xy[,2], xy[,1]) - GUI$north.angle * pi / 180
  planar.range <- sqrt(xy[,1]^2 + xy[,2]^2) / GUI$mpp
  return(cbind(GUI$plot.origin[1] + planar.range * cos(theta), GUI$plot.origin[2] - planar.range * sin(theta)))
}

gui.tx.matrix.to.spatial <- function(coords) {

  ## coords: an n x 2 matrix of data coordinates (sample, pulse)
  ##         (need not be integers)
  ##
  ## Value: an n x 3 matrix of spatial coordinates given the
  ## current transformation of radar data.
  ## e.g. c(0, 0) -> (0, 0, 0)

  r <- coords[,1] * RSS$scan.info$sample.dist + RSS$scan.info$first.sample.dist
  th <- pi/2 - 2 * pi * (RSS$scan.info$orientation * coords[,2] / RSS$scan.info$pulses + (RSS$scan.info$bearing + RSS$scan.info$bearing.offset) / 360)
  phi <- rss.antenna.angle()
  return (cbind(r * cos(phi) * cos(th), r * cos(phi) * sin(th), r * sin(phi) + rss.origin.elev()))
}
  
gui.pps <- function() {
  ## return the number of pixels per sample
  rss.planar.rps() / GUI$mpp
}

## set a new value for a parameter
## this will not take effect until gui.update.plot.parms is called

gui.new.parm <- function(name, val) {
  GUI$new.parm.values[[as.character(substitute(name))]] <- val
}

## Get the value for a parameter this takes into account any changes
## made but not yet locked in by gui.update.plot.parms It is intended
## to allow certain functions, such as zoom and pan, to accumulate a
## net change in parms between calls to gui.update.plot.parms

gui.parm <- function(name) {
  n <- as.character(substitute(name))
  if (!is.null(GUI$new.parm.values[[n]]))
    GUI$new.parm.values[[n]]
  else
    GUI[[n]]
}

## any changes to plotting parameters are made temporarily into
## GUI$new.parm.values, which is copied to GUI only between scan
## displays, so that every scan display uses a single set of
## parameters

## For changes to the parameters plot.origin, north.angle, and mpp, we
## must inform the tk canvas that it has been translated, rotated,
## and/or zoomed. For changes to the compass radius, we do a fake
## zoom that only affects canvas items with the tag "compass".

gui.update.plot.parms <- function() {
  np <- GUI$new.parm.values
  for (parm in names(np)) {
    if (GUI$plot.is.tk.image) {
      switch(parm,
             "plot.origin" =
             gui.canvas.panned(np$plot.origin - GUI$plot.origin),
             "north.angle" =
             gui.canvas.rotated(np$north.angle -
                                GUI$north.angle, GUI$plot.origin),
             "mpp" = gui.canvas.zoomed(GUI$mpp / np$mpp, GUI$plot.origin),
              "compass.radius" = gui.canvas.zoomed(np$compass.radius / GUI$compass.radius, GUI$plot.origin, "compass", NULL, NULL),
             NULL)
    }      
    GUI[[parm]] <- np[[parm]]
  }
  GUI$new.parm.values <- list()
}

gui.window.dims <- function(win = ".plot") {
  gui.parse.geometry(tclchar("winfo", "geometry", win))
}

gui.screen.dims <- function() {
  return(c(Tclint("winfo screenwidth ."), Tclint("winfo screenheight .")))
}

gui.centre.window <- function(name) {
  tcl("wm", "geometry", name, "+" %:% paste(floor((gui.screen.dims() - gui.window.dims(name)) / 2), collapse="+"))
}

gui.parse.geometry <- function(geometry)  {
    as.numeric(strsplit(geometry, "[+x-]")[[1]][1:2])
}

gui.plot.window.geometry <- function() {
  # return the width and height of the plot window in km
  # according to current plotting parameters
  gui.window.dims() * GUI$mpp / 1000
}

gui.plot.window.centre <- function() {
  # return the centre of the plot window, in
  # plot window coordinates (i.e. pixels, not geographic coordinates)
  gui.window.dims() / 2
}

gui.create.menu.button <- function(name, values, full.values = values, heading=NULL, tearoff=FALSE, set.fun=NULL, selected=1, null.label="") {
  
  ## create a TK menu button named "name" from the list of values, and an R closure named "name.value"
  ## which, when invoked, either returns or sets the index of the currently selected value.  The index is a
  ## number in the set seq(along=values).  If set.fun is specified, then it is called with the index of
  ## the selected item each time an item is selected.
  ## When the button is depressed, it displays the list from full.values, but when at rest,
  ## it displays the (presumably shorter) name from values.

  ## The menubutton options are those used by the tk_optionMenu function of optMenu.tcl v1.4 in Tk 8.4

  var.name <- name %:% ".value"
  lab.name <- name %:% ".lab"
  menu.name <- name %:% ".menu"
  width <- if (length(values) > 0) max(nchar(values)) else 25
    
  tcl("menubutton", name, textvariable=lab.name, indicatoron=1, menu=menu.name, relief="raised", bd=2, highlightthickness=2, anchor="c", direction="right", width=width)
  tcl("menu", menu.name, tearoff=as.integer(tearoff))

  ## remove the builtin Menubutton bindtag from this button, since
  ## we are customizing its behaviour
  .Tcl(paste("bindtags", name, "{", name, gui.tcl.parent(name), "all }"))
  tcl("bind", name, "<Button-1>", function(X, Y) tcl(menu.name, "post", X, Y))
  
  if (length(values) == 0) {
    tclvalue(lab.name) <- null.label
    tcl(name, "configure", state="disabled")
  } else {

    if (!is.null(heading)) {
      tcl(menu.name, "add", "command", label=heading)
      tcl(menu.name, "add", "separator")
      ## disable the heading, but make it look normal
      tcl(menu.name, "entryconfigure", 0, state="disabled")
      tcl(menu.name, "configure", disabledforeground="black")
    }

    ## the command for each item will call the R closure named "name.value"
    ## once, with no args, to get the just-selected index, and once
    ## with that index
    
    f <- rss.make.closure(function() do.call(var.name, list(do.call(var.name, list()))), list(var.name=var.name))
    for (i in seq(along=values))
      tcl(menu.name, "add", "radiobutton", label=full.values[i],
          variable=var.name, value=i, command=f)
    
    assign(var.name,
           rss.make.closure(
                            function(i) {
                              if (missing(i)) {
                                as.numeric(tclvalue(var.name))
                              } else {
                                tclvalue(var.name) <- i
                                tclvalue(lab.name) <- values[i]
                                if (!is.null(set.fun))
                                  set.fun(i, values[i])
                              }
                            },
                            list(var.name=var.name, lab.name=lab.name, values=values, set.fun=set.fun)),
           .GlobalEnv)
    do.call(var.name, list(selected))
  }
  return(name)
}

gui.add.item.to.menu.button <- function(mb, short.label, long.label) {
  ## add an item to menubutton "mb" (which must have
  ## been created by gui.create.menu.button)
  ## short.label is what is shown on the button for the new
  ## entry, long.label is what is shown in the popup menu
  setter <- get(var.name <- mb %:% ".value", .GlobalEnv)
  menu <- mb %:% ".menu"
  len <- length(environment(setter)$values <- c(environment(setter)$values, short.label))
  tcl(menu, "add", "radiobutton", var=var.name, value=len, label=long.label, command=
      rss.make.closure(function() do.call(setter, list(do.call(setter, list()))), list(setter = setter)))
}

gui.change.item.in.menu.button <- function(mb, index, short.label, long.label) {
  ## change entry in menubutton mb given by index to have
  ## the given short and long labels
  
  setter <- get(var.name <- mb %:% ".value", .GlobalEnv)
  menu <- mb %:% ".menu"
  environment(setter)$values[index] <- short.label
  tcl(menu, "delete", index - 1)
  tcl(menu, "insert", index - 1, "radiobutton", var=var.name, value=index, label=long.label, command=
      rss.make.closure(function() do.call(setter, list(do.call(setter, list()))), list(setter = setter)))
}

gui.create.checklist <- function(name, values, heading = NULL, tearoff = FALSE, set.fun = NULL) {
  ## Create a TK menu named "name" from the list of values,
  ## and an R closure named "name.value" which, when invoked,
  ## will return or set the logical value associated with
  ## a given index.  Each time an item is toggled, the function
  ## set.fun is called with its index and its new value (TRUE or FALSE)
  
  var.name <- name %:% ".value"
  TCL("menu $name -tearoff $as.integer(tearoff)")

  if (!is.null(heading)) {
    tcl(name, "add", "command", label=heading)
    tcl(name, "add", "separator")
    ## disable the heading, but make it look normal
    tcl(name, "entryconfigure", 0, state="disabled")
    tcl(name, "configure", disabledforeground="black")
  }
 
  f <- function(i) {
    if (!is.null(set.fun))
      set.fun(i, tclvalue(var.name %:% i))
  }

  for (i in seq(along=values)) {
    ## For each item on the menu, we need a wrapped copy of f
    ## that is called with the index i.
    
    g <- rss.make.closure(
                          function() f(i),
                          list(i=i)
                          )
    tcl(name, "add", "checkbutton", label=values[i],  variable=var.name%:%i, command = g)
  }
  
  assign(var.name,
         function(i, set) {
           if (missing(set)) {
             tclboolvar(var.name %:% i)
           } else {
             v <- var.name %:% i
             tclvalue(v) <- as.integer(as.logical(set))
           }
         },
         .GlobalEnv)
  return(name)
}

## list of keysyms that don't change a spinbox widget's contents
gui.nochange.keysyms.Spinbox <- c("Left", "Right", "Home", "End", "Tab", "Shift", "Ctrl", "Meta", "Alt", "ISO_Left_Tab", "Shift_L", "Shift_R", "Control_L", "Control_R", "Shift_L", "Shift_R", "Meta_L", "Meta_R", "Alt_L", "Alt_R")

## list of keysyms that don't change a text widget's contents
gui.nochange.keysyms.Text <- c(gui.nochange.keysyms.Spinbox, "Up", "Down")

gui.create.gauge <- function(parent, label, range, increment, val, set.fun, short.name=tolower(gsub("[^a-zA-Z0-9_]", "_", label)),
                             setter.name=NULL, setter.env=.GlobalEnv, width=10, format = NULL) {
  ## create a gauge:  a labelled spinbox in a frame
  ## parent is the tcl name of the parent window
  ## label is the text label which will be user-visible
  ## range is the min and max values
  ## increment is the increment
  ## val is the initial value
  ## set.fun is a function to be called whenever the widget's
  ## value changes, and is meant to communicate that back
  ## to R via a global variable
  ## To set the value of the gauge, the closure
  ## GAUGE_NAME.spin can be called:
  ## e.g. .a.b.spin(set, 34)
  ## returns the name of the gauge frame window

  name <- parent %:% "." %:% short.name
  spname <- name %:% ".spin"
  lbname <- name %:% ".lab"
  tcl("frame", name)
  tcl("label", lbname, text=if (is.null(label)) "" else label %:% ":")
  if (!is.null(format))
    tcl("spinbox", spname, from=range[1], to=range[2], increment=increment, width=width, format=format)
  else
    tcl("spinbox", spname, from=range[1], to=range[2], increment=increment, width=width)

  tcl(spname, "set", val)
  ## make a closure, so that each gauge has its
  ## own environment for storing its old.field.value
  f <- rss.make.closure(
                        function(...) {
                          x <- tclreal(spname, "get")
                          if (!identical(x, old.field.value)) {
                            set.fun(x)
                            old.field.value <<- x
                          }
                          tcl(spname, "configure", background = GUI$entry.field.background.colour)
                        },
                        list(spname=spname, set.fun=set.fun, old.field.value=NULL))
  ## use the same environment for each of the other bound events
  env <- environment(f)
  
  tcl(spname, "configure", command=f)
  tcl("bind", spname, "<KeyRelease-Return>", f)
  tcl("bind", spname, "<FocusOut>", f)
  tcl("bind", spname, "<FocusIn>",
      rss.make.closure(
                       function() {
                         ## save the old field value when it gets keyboard focus
                         ## this allows cancellation by hitting Escape
                         old.field.value <<- tclreal(spname, "get")
                       },
                       env))
  tcl("bind", spname, "<KeyRelease-Escape>",
      rss.make.closure(
                       function() {
                         tcl(spname, "set", old.field.value)
                         tcl(spname, "configure", background = GUI$entry.field.background.colour)
                       },
                       env))

  tcl("bind", spname, "<KeyRelease>",
      function(K) {
        K <- as.character(K)
        if (! (K %in% gui.nochange.keysyms.Spinbox))
          tcl(spname, "configure", background = GUI$entry.field.changed.colour)
      })
      
  TCL("pack $spname $lbname -side right")
  if (!is.null(setter.name) && !is.null(setter.env)) {
    assign(setter.name,
           rss.make.closure(
                            function(x) {
                              if (missing(x)) {
                                return(tclreal(spname, "get"))
                              } else {
                                tcl(spname, "set", x - tclreal(spname, "cget", "-increment"))
                                tcl(spname, "invoke", "buttonup")
                              }
                            },
                            list(spname=spname)
                            ),
           pos = setter.env)
  }
  return (name)
}

gui.create.string <- function(parent, label, width, height, val, val.check, set.fun, short.name=gsub("[^a-zA-Z0-9_]", "_", label), setter.name=NULL, setter.env=.GlobalEnv) {
  ## create a string:  a labelled text entry box in a frame
  ## 
  ## parent is the tcl name of the parent window
  ## label is the text label which will be user-visible
  ## width is the number of characters to reserve for the box
  ## val is the initial value
  ## val.check is a function returning TRUE on valid input, FALSE otherwise
  ## returning a possibly transformed version of the field's value
  ## which is used when calling set.fun and returned by the setter function
  ##
  ## set.fun is a function to be called whenever the widget's
  ## value changes, and is meant to communicate that back
  ## to R via a global variable

  ## setter.name and setter.env specify where to create a getter/setter function
  ## which will return the current value when called with 0 args, and set the
  ## current value when called with 1 arg.

  name <- parent %:% "." %:% short.name
  txname <- name %:% ".txt"
  lbname <- name %:% ".lab"
  tcl("frame", name)
  tcl("label", lbname, text=label %:% ":")
  tcl("text", txname, height=1, width=width, height=height, wrap="char")
  tcl(txname, "delete", "0.0", "end")
  tcl(txname, "insert", "0.0", val)
  ## make a closure, so that each string has its
  ## own environment for storing its old.field.value
  f <- rss.make.closure(
                        function(...) {
                          x <- tcl1char(txname, "get", "0.0", "end")
                          if (!identical(x, old.field.value)) {
                            if (!val.check(x))
                              return()
                            set.fun(x)
                            old.field.value <<- x
                          }
                          tcl(txname, "configure", background = GUI$entry.field.background.colour)
                        },
                        list(txname=txname, set.fun=set.fun, old.field.value=NULL, val.check=val.check))
  ## use the same environment for each of the other bound events
  env <- environment(f)
  
  tcl("bind", txname, "<KeyRelease-Return>", f)
  tcl("bind", txname, "<FocusOut>", f)
  tcl("bind", txname, "<FocusIn>",
      rss.make.closure(
                       function() {
                         ## save the old field value when it gets keyboard focus
                         ## this allows cancellation by hitting Escape
                         old.field.value <<- tcl1char(txname, "get", "0.0", "end")
                       },
                       env))
  tcl("bind", txname, "<KeyRelease-Escape>",
      rss.make.closure(
                       function() {
                         tcl(txname, "delete", "0.0", "end")
                         tcl(txname, "insert", "0.0", old.field.value)
                         tcl(txname, "configure", background = GUI$entry.field.background.colour)
                       },
                       env))

  tcl("bind", txname, "<KeyRelease>",
      function(K) {
        K <- as.character(K)
        if (! (K %in% gui.nochange.keysyms.Text))
          tcl(txname, "configure", background = GUI$entry.field.changed.colour)
      })
  TCL("pack $txname $lbname -side right")
  if (!is.null(setter.name) && !is.null(setter.env)) {
    assign(setter.name,
           rss.make.closure(
                            function(x) {
                              if (missing(x)) {
                                return(tclchar(txname, "get", "0.0", "end"))
                              } else {
                                tcl(txname, "delete", "0.0", "end")
                                tcl(txname, "insert", "0.0", x)
                              }
                            },
                            list(txname=txname)
                            ),
           pos = setter.env)
  }
  return (name)
}

gui.create.datetime <- function(parent, label, val, val.check, set.fun, short.name=gsub("[^a-zA-Z0-9_]", "_", label), setter.name=NULL, setter.env=.GlobalEnv) {
  ## create a datetime chooser for allowing rapid entry of a date/time
  ## 
  ## parent is the tcl name of the parent window
  ## label is the text label which will be user-visible
  ## val is the initial value (a double timestamp)
  ## val.check is a function returning TRUE on valid input, FALSE otherwise
  ## returning a possibly transformed version of the field's value
  ## which is used when calling set.fun and returned by the setter function
  ##
  ## set.fun is a function to be called whenever the widget's
  ## value changes, and is meant to communicate that back
  ## to R via a global variable
  ##
  ## setter.name and setter.env specify where to create a getter/setter function
  ## which will return the current value when called with 0 args, and set the
  ## current value when called with 1 arg.

  name <- parent %:% "." %:% short.name
  txname <- name %:% ".txt"
  lbname <- name %:% ".lab"
  varname <- "::" %:% gsub(".", "_", name, fixed=TRUE)
  val <- round(val) ## FIXME: why does datepick.tcl not allow non-integer timestamps?  Seems to be a tcl issue...
  tcl("frame", name)
  tcl("label", lbname, text=label %:% ":")
  tcl("text", txname, height=1, width=24, wrap="char")
  tcl("set", varname, val)
  if (!is.null(set.fun))
    ff <- function(...) set.fun(tclrealvar(varname))
  else
    ff <- ""
  tcl("Datepick::datepick", txname, varname, "12", "", ff)
  TCL("pack $txname $lbname -side right")
    
  if (!is.null(setter.name) && !is.null(setter.env)) {
    assign(setter.name,
           rss.make.closure(
                            function(x) {
                              if (missing(x)) {
                                return(tclrealvar(varname))
                              } else {
                                tcl("set", varname, x)
                              }
                            },
                            list(varname=varname)
                            ),
           pos = setter.env)
  }
  return (name)
}

gui.enable.tree <- function(w, enable=TRUE) {
  ## enable or disable a window and all its
  ## descendents
  ## recursion bottoms out when a window has no children
  ## gui.print.cons(w)
  for (d in tclchar("winfo", "children", w)) {
    gui.enable.tree(d, enable)
  }
  if (any(grep("-state", tclchar(w, "configure"))))
    tcl(w, "configure", state=ifelse(enable, "normal", "disabled"))
}

gui.tcl.parent <- function(window) {
  ## return the path of the parent to window
  sub("\\.[a-zA-Z_0-9]+$", "", window)
}

gui.menu.length <- function(menu) {
  ## return the number of entries in a menu
  len <- tclchar(menu, "index", "end")
  if (len == "none")
    return (0)
  return (as.integer(len) + 1 - tclint(menu, "cget", "-tearoff"))
}

gui.menu.inds <- function(menu) {
  ## return the tcl indexes of entries in a menu
  len <- tclchar(menu, "index", "end")
  if (len == "none")
    return(c())
  have.tearoff <- tclint(menu, "cget", "-tearoff")
  seq(from = have.tearoff, length = as.integer(len) + 1 - have.tearoff)
}
    
  
gui.index.of.submenu <- function(menu) {
  ## return the index of the menu in its parent
  ## returns NA if the menu is not present in its parent

  parent <- gui.tcl.parent(menu)
  inds <- gui.menu.inds(parent)
  ## check for valid number before searching, since len might be "none"
  for (i in inds)
    if (tclchar(parent, "type", i) == "cascade" && tclchar(parent, "entrycget", i, "-menu") == menu)
      return (i)
  return(NA)
}

gui.index.of.menu.label <- function(menu, label) {
  ## return the index of the label in the menu,
  ## returning -1 if it does not exist

  rv <- -1
  rss.try(rv <- tclint(menu, "index", label))
  return(rv)
}
  
gui.drop.menu <- function(root, titles) {
  ## destroy the tcl cascading menu object named by titles
  ## and rooted at root
  ## and any contiguous sequence of ancestors
  ## which are made empty as a result of deleting the lower
  ## level item

  menu <- root %:% "." %:% paste(GUI$menu.title.to.tcl.part[titles], collapse=".")
  while (TRUE) {
    tcl("destroy", menu)
    parent <- gui.tcl.parent(menu)
    if (parent == "")
      return()
    i <- gui.index.of.submenu(menu)
    if (is.na(i))
      return()
    tcl(parent, "delete", i)
    if (gui.menu.length(parent) > 0 || parent == root)
      return()
    menu <- parent
  }
}

gui.add.menu <- function(root, titles) {

  ## add a cascading menu entry with titles given by "titles"
  ## rooted at the tcl menu root, which must exist
  ## intermediate menu names are generated based on unique numbers
  ## corresponding to the titles
  ## any intervening non-existent menus are created
  ## as cascade menu entries with labels from titles
  ## A title of "---" is used to create a separator
  ##

  ## add ID's to the menu.title.to.tcl.part

  for (t in titles) {
    if (nchar(t) > 0 && ! (t %in% names(GUI$menu.title.to.tcl.part))) {
## Redundant, and incorrect, since [[]] on a list allows partial matching:      
##      if (is.null(GUI$menu.title.to.tcl.part[[t]]))
        GUI$menu.title.to.tcl.part[[t]] <- 1 + length(GUI$menu.title.to.tcl.part)
    }
  }

  ## create any intermediate menus

  parts <- GUI$menu.title.to.tcl.part[titles]
  parent <- root
  n <- length(titles)
  for (i in 1:n) {
    menu <- paste(c(root, parts[1:i]), collapse=".")
    if (i > 1 && !tclbool("winfo", "exists", parent)) {
      tcl("menu", parent, title=titles[i-1], tearoff=1, tearoffcommand=gui.new.tearoff)
      GUI$menu.title.to.tcl.path[[titles[i-1]]] <- parent
    }
    if (is.na(gui.index.of.submenu(menu)))
      if (i < n || titles[i] != "---")
        tcl(parent, "add", "cascade", menu=menu, label=titles[i])
      else
        tcl(parent, "add", "separator")
    parent <- menu
  }
  return (menu)
}

gui.popup.message <- function(title, msg, id="msgbox" %:% GUI$num.msgboxes, time.to.live = 0) {
  ## pop up a message box
  ## id is a name for the window, without the leading "."
  ## If time.to.live is positive, it is the number of seconds to
  ## display the message before destroying it.  Otherwise, the caller
  ## is responsible for deleting the message box by calling rss.gui(DELETE_MESSAGE_BOX, id)

  GUI$num.msgboxes <- 1 + GUI$num.msgboxes
  name <- "." %:% id
  tcl("toplevel", name)
  tcl("wm", "title", name, title)
  tcl("wm", "iconbitmap", name, GUI$application.icon)
  tcl("label", labname <- name %:% ".label", text=msg)
  tcl("pack", labname)
  tcl("wm", "deiconify", name)
  x <- floor((tclint("winfo", "vrootwidth", ".") - tclint("winfo", "width", name)) / 2)
  y <- floor((tclint("winfo", "vrootheight", ".") - tclint("winfo", "height", name)) / 2)
  tcl("wm", "geometry", name, "+" %:% x %:% "+" %:% y)
  tcl("raise", name)
  rss.gui(UPDATE_GUI)
  if (time.to.live)
    TCL("after " %:% (time.to.live * 1000) %:% " {destroy " %:% name %:% "}")
  return(id)
}

gui.delete.message <- function(id) {
  try(tcl("destroy", "." %:% id), silent=TRUE)
}
  
gui.popup.dialog <- function(title, msg, fun=NULL, entry=FALSE, buttons="Ok", default=1, drop.down=FALSE, default.entry="") {
  ## pop up a dialog with a list of buttons and possibly an entry field
  ## If drop.down is TRUE, then the items in buttons are placed in a drop box, and
  ## both OK and Cancel buttons are added; when the user hits OK, the index of the selected
  ## item in the dropbox is returned.  If the user hits cancel, NA is returned

  name <- ".dlg"
  if (!entry && !drop.down) {
    which <- 1 + do.call(tclint, c(list("tk_dialog", name, title, msg, "", default - 1), buttons))
    if (!is.null(fun))
      fun(which)
    return(which)
  }
  TCL("toplevel " %:% name %:% "; wm withdraw " %:% name)
  tcl("wm", "title", name, title)
  tcl("wm", "iconbitmap", name, GUI$application.icon)
  lab <- tcl("label", name %:% ".msg", text=msg)
  tvar <- "dlg.entry.val"
  if (entry) {
    ent <- tcl("entry", name %:% ".entry", textvariable=tvar, width=60)
    tcl(ent, "insert", "end", default.entry)
    tcl(ent, "selection", "range", "0", "end")
  }

  if (drop.down) {
    choice <- NA
    my.f <- function(i, v) choice <<- i
    mb <- gui.create.menu.button(name %:% ".mbtn", values=buttons, set.fun=my.f, selected=default)
    bok <- tcl("button", name %:% ".ok", text="Ok", command=function() tcl("destroy", name), width=10)
    bcancel <- tcl("button", name %:% ".cancel", text="Cancel", command=function() {choice <<- NA; tcl("destroy", name)}, width=10)
    r <- 0
    tcl("grid", "configure", lab, row=r, column=0, columnspan=2, sticky="ew")
    r <- 1+r
    tcl("grid", "configure", mb, row=r, column=0, columnspan=2, sticky="ew")
    r <- 1+r
    if (entry) {
      tcl("grid", "configure", ent, row=r, column=0, columnspan=2, sticky="ew")
      r <- 1+r
    }
    tcl("grid", "configure", bok, row=r, column=0)
    tcl("grid", "configure", bcancel, row=r, column=1)
    tcl("wm", "deiconify", name)
    .Tcl("update; set x [expr {([winfo screenwidth .]-[winfo width .dlg])/2}];set y [expr {([winfo screenheight .]-[winfo height .dlg])/2}]; wm geometry .dlg +$x+$y")
    tcl("raise", name)
    tcl("focus", if (entry) ent else name)
    tcl("update")
    tcl("grab", name)
    tcl("tkwait", "window", name)
    if (entry) {
      v <- as.character(tclvalue(tvar))
      tcl("unset", tvar)
      return(list(choice, v))
    } else {
      return(choice)
    }
  } else {
    my.f <- function() {choice <<- b; tcl("destroy", name)}
    tcl("grid", "configure", name %:% ".msg", row=0, column=0, columnspan=1 + length(buttons), sticky="w")
    if (entry) {
      tcl("grid", "configure", ent, row=1, column=0, columnspan=1 + length(buttons), sticky="ew")
    }
    w <- max(10, max(nchar(buttons)))
    for (b in seq(along=buttons)) {
      tcl("button", name %:% "." %:% b, text=buttons[b], command=rss.make.closure(my.f, list(b=b, name=name)), width=w,
          default=ifelse(b==default, "active", "normal"))
      tcl("grid", "configure", name %:% "." %:% b, row=2, column=b-1, padx=2, pady=2, sticky="w")
    }
    tcl("bind", ".dlg", "<KeyRelease-Return>", "") ## otherwise we may get stray events from a file dialog??
    tcl("bind", ".dlg", "<Return>", name %:% "." %:% default %:% " invoke")
    tcl("wm", "deiconify", name)
    gui.centre.window(name)
    tcl("raise", ".dlg")
    tcl("focus", ifelse(entry, ".dlg.entry", ".dlg"))
    tcl("update")
    tcl("grab", name)
    tcl("update")
    tcl("tkwait", "window", name)
    if (entry) {
      v <- as.character(tclvalue(tvar))
      tcl("unset", tvar)
      return(list(choice, v))
    } else {
      return(choice)
    }
  }
}

gui.zoom.to.patch <- function(patch, blip=NULL, sarea=225) {
  ## zoom and recentre the plot so that the given blip
  ## is displayed at the centre with range cells shown
  ## as sectors of area (roughly) 'sarea' pixels.
  ## Specifying blip (using "blip=") rather than patch
  ## zooms to the given blip.
  
  if (!is.null(blip)) {
    if (blip < 1 || blip > length(RSS$blips))
      return(NULL)
    patch <- RSS$blips[blip]
  } else {
    if (patch < 1 || patch > length(RSS$patches$ns))
      return(NULL)
  }
  xy.coords <- RSS$patches[patch, c("x", "y")]
  rss.gui(ZOOM_TO_LEVEL, sqrt(RSS$scan.info$sample.dist * sqrt(sum(xy.coords^2)) * 2 * pi / (RSS$scan.info$pulses * sarea)))
  screen.coords <- gui.xy.to.plot.coords(xy.coords)
  rss.gui(SET_PLOT_ORIGIN, screen.coords, NULL)
}

gui.show.graph <- function(graph.fun, position, size) {
  ## plot an R graph into a temporary .PNG file, and load it into
  ## the plot window at the specified coordinates and dimensions
  ## graph.fun: an R function to perform the graphing; it must not change the current device
  ## position: x, y coordinates (in pixels) in the plot window at which to place the graph
  ## size: width, height (in pixels) of the graph
  ##
  ## Return: the id of this graph (which can be used in a call to delete it)
  ##
  ## Side effect: load the graph into the Tk image named "::img:" %:% id,
  ##              create a canvas image item called GUI$plot %:% "." %:% id and make it visible
  ##
  ## The caller can erase the graph by calling rss.gui(DELETE_GRAPH, id)

  id <- "graph" %:% (GUI$num.graphs <- 1 + GUI$num.graphs)
  
  png(GUI$png.plot.filename, width=size[1], height=size[2], bg="transparent")
  ## set up sensible defaults for colors, which the caller can change
  ## from inside graph.fun
  col <- "white"
  par(fg=col, col.axis=col, col.lab=col, col.main=col, col.sub=col, mai=c(0.25,0.25,0.25,0.25), mar=c(2,2,1,1), oma=c(0,0,0,0))
  
  graph.fun()
  dev.off()

  system(sprintf("convert %s %s", GUI$png.plot.filename, sub(".png", ".gif", GUI$png.plot.filename)))
  ## import the graph as a tk image and add it to the plot
  image <- "::img:" %:% id
#  tcl("image", "create", "photo", image, file=GUI$png.plot.filename, "-format", "png")
  tcl("image", "create", "photo", image, file=sub(".png", ".gif", GUI$png.plot.filename), "-format", "gif")
  tkid <- tcl(GUI$plot, "create", "image", position[1], position[2], image=image)

  ## add two tags to the graph, one for all graphs, one specific to this one
  tcl(GUI$plot, "addtag", "graph", "withtag", tkid)
  tcl(GUI$plot, "addtag", id, "withtag", tkid)
  tcl(GUI$plot, "addtag", "zoom", "withtag", tkid)
  tcl(GUI$plot, "addtag", "pan", "withtag", tkid)
  return(id)
}

gui.delete.graph <- function(id = NULL) {
  ## delete the specified graph, or all graphs if none is specified
  if (!is.null(id)) {
    tcl(GUI$plot, "delete", id)
    tcl("destroy", "::img:" %:% id)
  } else {
    graphs <- tclchar(GUI$plot, "find", "withtag", "graph")
    tcl(GUI$plot, "delete", "graph")
    for (g in graphs)
      tcl("destroy", "::img:" %:% g)
  }
}

gui.state.has.key <- function(s, k) {
  ## return TRUE if the mod key given by k is present in state s
  ## (s is an integer provided by the Tk event macro "%s")
  ## k is "Shift", "Alt", or "Control"
  
  return(as.logical(intToBits(as.integer(s))[GUI$motion.state.key.bits[[k]]]))
}
  
gui.set.coord.tx <- function(plot.to.matrix = NULL, plot.to.spatial = NULL, xy.to.plot = NULL, xyz.to.plot = NULL, matrix.to.spatial = NULL) {
  ## set the coordinate transforms; this might be called by the start.up and shut.down
  ## methods of a data source in order to implement a different coordinate system
  ## A NULL value for any parameter means to use the built-in default

  GUI$tx.plot.to.matrix <- if (is.null(plot.to.matrix)) gui.tx.plot.to.matrix else plot.to.matrix
  GUI$tx.plot.to.spatial <- if (is.null(plot.to.spatial)) gui.tx.plot.to.spatial else plot.to.spatial
  GUI$tx.xy.to.plot <- if (is.null(xy.to.plot)) gui.tx.xy.to.plot else xy.to.plot
  GUI$tx.xyz.to.plot <- if (is.null(xyz.to.plot)) gui.tx.xyz.to.plot else xyz.to.plot
  GUI$tx.matrix.to.spatial <- if (is.null(matrix.to.spatial)) gui.tx.matrix.to.spatial else matrix.to.spatial
}

