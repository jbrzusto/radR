##  svn $Id: epal.R 213 2009-02-12 16:15:11Z john $
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
## epal.R - editor for gradient palettes                
##
## This modules pops up a widget for editing a gradient palette.                                  
##                                                      
## Parameters to EPAL$edit:
##
##   name:  the name of the palette
##
##   class: the name of the class of samples for which the editor was invoked
##          (in case the user saves this palette under a different name)
##
##   range: integer vector; the minimum and maximum data values for which the
##          palette will be used. This allows the user to pick colour points
##          at precise data values.
##

EPAL <<-
  strictenv(
            pal               = NULL,
            alpha             = NULL,
            orig.pal          = NULL,
            orig.alpha        = NULL,

            ## background colour of the palette editor window
            bg.colour         = NULL,
            
            ## vars for dragging
            drag.start.x      = 0,
            drag.has.moved    = FALSE,
            drag.start.pal    = 0,
            drag.start.wedgex = 0,
            
            ## the polygon of a wedge shape
            wedge.width       = 8,
            wedge.height      = 12,
            wedge.poly        = NULL,
            wedge.outline     = "#000000",

            ## other vars
            cancel            = NULL,
            class.name        = NULL,
            clear             = NULL,
            col.height        = NULL,
            colour.photo      = NULL,
            colours           = NULL,
            col.photo.width   = NULL,
            col.width         = NULL,
            desc              = NULL,
            first.redraw      = TRUE,
            hi                = NULL,
            label             = NULL,
            lo                = NULL,
            epal              = NULL,
            ok                = NULL,
            pal.name          = NULL,
            revert            = NULL,
            save.as           = NULL,
            wedges            = NULL,
            
            edit = function(name, class, range) {
              ## let the user build a palette,
              ## namely a named vector of reals between 0 and 1
              ## with the names being tcl colour names, and the
              ## numbers being the points along the spectrum
              ## from lo to hi where the particular colours apply
              pal.name <<- name
              class.name <<- class

              ## the current palette
              pal <<- orig.pal <<- with(RSS$palettes[[pal.name]], structure(points, names=colours))
              alpha <<- orig.alpha <<- RSS$palettes[[pal.name]]$alpha
              
              ## the range
              lo <<- range[1]
              hi <<- range[2]
              
              ## flag the first redraw
              first.redraw <<- TRUE
              
              ## the wedge polygon
              wedge.poly <<- c(1, 1, 1-wedge.width / 2, 1, 1, 1+wedge.height, 1+wedge.width / 2, 1, 1, 1)

              ## set titles
              tcl(lolab, "configure", text=lo)
              tcl(hilab, "configure", text=hi)
              tcl("wm", "state", epal, "normal")
              set.title()
              tcl("raise", epal)
              tcl("focus", epal)
              tcl("bind", colours, "<Configure>", set.colours.dim)
              set.colours.dim(tclint("winfo", "width", colours), tclint("winfo", "height", colours))
              set.desc(RSS$palettes[[pal.name]]$desc)            
              redraw()
            },

            set.desc = function (s) {
              tcl("set", "epal.desc", s)
            },

            get.desc = function () {
              paste(tclchar(desc, "get"), collapse=" ")
            },
            
            set.title = function() {
              tcl("wm", "title", epal, "Edit palette '" %:% pal.name %:% "' (radR)")
            },
            
            set.colours.dim = function(w, h) {
              col.width       <<- as.integer(as.character(w))
              col.height      <<- as.integer(as.character(h))
              col.photo.width <<- col.width - wedge.width - 4
              ## recreate the colour photo holding the palette
              if (!is.null(colour.photo))
                tcl("image", "delete", colour.photo)
              colour.photo <<- tcl("image", "create", "photo", "::img::epal", width=col.photo.width, height=col.height)
              tcl(colours, "delete", "photo")
              tcl(colours, "create", "image", wedge.width / 2, 0, image=colour.photo, anchor="nw", tags="photo")
              tcl(colours, "bind", "photo", "<ButtonPress-1>", make.new.wedge)
              tcl(colours, "bind", "photo", "<Double-Button-1>", make.and.edit.new.wedge)
              ## layout items
              
              tcl("place", lolab, relx=0.0, x=2, height=15, y=0, anchor="nw")
              tcl("place", hilab, relx=1.0, x=-2, height=15, y=0, anchor="ne")
              tcl("place", wedges, relwidth=1.0, height=17, x=0, y=16, anchor="nw")
              tcl("place", colours, relwidth=1.0, height=20, x=0, y=33, anchor="nw")
              tcl("place", label, height=15, x=0, y = 53, anchor="n")
              tcl("place", desc, height=20, x=0, y = 70, anchor="nw", relwidth=1.0)
              tcl("place", info,   height=55, relx=0.5, y=90, anchor="n")
              tcl("place", ok,     width=75, x=0,   rely=1.0, anchor="sw")
              tcl("place", cancel, width=75, x=80,  rely=1.0, anchor="sw")
              tcl("place", revert, width=75, x=160, rely=1.0, anchor="sw")
              tcl("place", clear,  width=75, x=240, rely=1.0, anchor="sw")
              tcl("place", save.as,  width=75, x=320, rely=1.0, anchor="sw")

              redraw()
            },

            restore.original.palette = function() {
              ## restore the original palette
              RSS$palettes[[pal.name]]$points <- as.numeric(orig.pal)
              RSS$palettes[[pal.name]]$colours <- names(orig.pal)
              RSS$palettes[[pal.name]]$alpha <- orig.alpha
              rss.palette.changed(pal.name)
            },

            redraw.controls = function() {
              ## try on the new palette
              RSS$palettes[[pal.name]]$points <- as.numeric(pal)
              RSS$palettes[[pal.name]]$colours <- names(pal)
              RSS$palettes[[pal.name]]$alpha <- alpha
              rss.palette.changed(pal.name)
            },

            ## the intial positions of wedges

            make.wedges = function() {
              ## draw wedges at the correct spots for the current palette
              tcl(wedges, "delete", "wedges")
              for (i in seq(along=pal)) {
                id <- tcl(wedges, "create", "polygon",
                          round(c(wedge.width / 2, 0) + wedge.poly + c(pal[i] * (col.photo.width - 1), 1)),
                          tags = "wedges",
                          fill = names(pal)[i],
                          outline = wedge.outline,
                          activewidth=2)
                tcl(wedges, "bind", id, "<ButtonRelease-1>", rss.make.closure(function(X)end.drag(i), list(i=i), parent=EPAL))
                tcl(wedges, "bind", id, "<ButtonRelease-3>", rss.make.closure(function()delete(i), list(i=i), parent=EPAL))
                tcl(wedges, "bind", id, "<ButtonPress-1>", rss.make.closure(function(X)start.drag(i, id, X), list(i=i, id=id), parent=EPAL))
                tcl(wedges, "bind", id, "<Button1-Motion>", rss.make.closure(function(X)continue.drag(i, id, X), list(i=i, id=id), parent=EPAL))
                tcl(wedges, "bind", id, "<Double-Button-1>", rss.make.closure(function(X)choose.colour.for(i), list(i=i), parent=EPAL))
                f<-rss.make.closure(function()set.wedge.val(i, pal[i] +  1 / (hi - lo)), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<Button-4>", f)
                f<-rss.make.closure(function()set.wedge.val(i, pal[i] -  1 / (hi - lo)), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<Button-5>", f)
                f<-rss.make.closure(function(x)set.wedge.alpha(i, alpha[i] + 15), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<Shift-Button-4>", f)
                f<-rss.make.closure(function(x)set.wedge.alpha(i, alpha[i] - 15), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<Shift-Button-5>", f)
                f<-rss.make.closure(function(x)set.wedge.val(i, pal[i] +  as.integer(x) / GUI$mousewheel.step / (hi - lo)), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<<MouseWheel>>", f)
                f<-rss.make.closure(function(x)set.wedge.alpha(i, alpha[i] +  as.integer(x) / GUI$mousewheel.step * 15), list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<<Shift-MouseWheel>>", f)
                f<-rss.make.closure(function(x)
                                    {
                                      rv <- rss.gui(POPUP_DIALOG,
                                                    title="Transparency for colour " %:% i,
                                                    msg="Choose a transparency value from 0=fully transparent to 255=fully opaque",
                                                    entry=TRUE,
                                                    buttons=c("Ok", "Cancel"),
                                                    default.entry = as.character(alpha[i]))
                                      if (rv[[1]] == 1) {
                                        alpha <- as.integer(rv[[2]])
                                        if (!is.na(alpha))
                                          set.wedge.alpha(i, alpha)
                                      }
                                    }, list(i=i), parent=EPAL)
                tcl(wedges, "bind", id, "<Shift-Button-1>", f)
              }
            },
            
            make.new.wedge = function(x) {
              ## add a new point at the current label,
              ## with the colour taken from the existing palette
              ## return TRUE on success, FALSE on failure
              new.val <- (as.real(as.character(x)) - wedge.width / 2) / col.photo.width
              i <- which(new.val == pal)
              new.col <- tcl.colour.of(new.val, NULL)
              if (length(i) == 0) {
                i <- length(pal) + 1
                if (set.pal.val(i, new.val)) {
                  names(pal)[i] <<- c(new.col) ## strip off "alpha" attribute
                  alpha[i] <<- attr(new.col, "alpha")
                  redraw()
                } else {
                  return(0)
                }
              }
              return(i)
            },

            make.and.edit.new.wedge = function(x) {
              i <-make.new.wedge(x)
              if (i > 0)
                choose.colour.for(i, TRUE)
            },

            set.pal.val = function(i, val) {
              ## set the value (not colour) of the i'th gradient point
              ## to val
              nv <- round(lo + (hi - lo) * val)
              if (!identical(val, pal[i])) {
                if (nv >= lo && nv <= hi) {
                  pal[i] <<- val
                } else {
                  return (FALSE)
                }
              }
              redraw.label(val)
              return (TRUE)
            },
            
            set.alpha.val = function(i, alph) {
              ## set the alpha value of the i'th gradient point
              ## to val
              if (alph > 255)
                alph = 255
              if (alph < 0)
                alph = 0
              alpha[i] <<- alph
              return (TRUE)
            },

            redraw = function() {
              ## redraw wedges and the colour palette canvas
              make.wedges()
              redraw.colours()
            },

            redraw.colours = function() {
              if (col.width - wedge.width < 0 || col.height - 1 < 0)
                return()
              cols <- tcl.colour.of( (0 : (col.photo.width - 1)) / (col.photo.width - 1))
              .Tcl(paste(colour.photo, "put {{", paste(cols, collapse=" "), "}} -to 0 0 ", col.width - wedge.width, col.height - 1))
              if (!first.redraw) {
                redraw.controls()
              } else {
                first.redraw <<- FALSE
              }
            },

            redraw.label = function(val) {
              tcl(label, "configure", text=round(lo + (hi - lo) * val))
              tcl("place", label, relx = val, x = 10 - 33 * val, anchor = "n")
            },
            
            start.drag = function(i, id, X) {
              drag.start.x <<- as.integer(as.character(X))
              drag.start.wedgex <<- tclint(wedges, "coords", id) [1]
              drag.start.pal <<- pal[i]
              drag.has.moved <<- FALSE
            },

            set.wedge.val = function(i, val) {
              if (set.pal.val(i, val)) {
                redraw()
              }
            },

            set.wedge.alpha = function(i, val) {
              if (set.alpha.val(i, val)) {
                redraw()
              }
            },

            continue.drag = function(i, id, X) {
              X <- as.integer(as.character(X))
              delX <- X - drag.start.x
              newX <- delX + drag.start.wedgex
              new.val <- drag.start.pal + delX / (col.photo.width - 1)
              if (set.pal.val(i, new.val)) {
                drag.has.moved <<- TRUE
                tcl(wedges, "move", id, newX - tclint(wedges, "coords", id) [1], 0)
              }
            },

            end.drag = function(i) {
              if (drag.has.moved) {
                drag.has.moved <<- FALSE
                redraw.colours()
              } else {
                ## force the label to move
                redraw.label(pal[i])
              }
            },
            
            colour.comp = function(colour) {
              ## return the list of components of colour
              ## for the current palette, where colour is "red", "green", or "blue"
              
              from <- c(red=2, green=4, blue=6)[colour]
              to   <- c(red=3, green=5, blue=7)[colour]
              as.integer(paste("0x", sapply(names(pal), function(s)substring(s, from, to)), sep=""))
            },

            tcl.colour.of = function(val, bg=bg.colour) {
              ## return the appropriate tcl colour names for the value(s) in val
              ## (these are from 0 to 1)
              ## If bg is NULL, then don't blend the colour with bg,
              ## but return alphas as attribute "alpha".
              
              reds   <- rss.lin.interp(pal, colour.comp("red"), val)
              greens <- rss.lin.interp(pal, colour.comp("green"), val)
              blues  <- rss.lin.interp(pal, colour.comp("blue"), val)
              alphas <- rss.lin.interp(pal, alpha, val)

              if (!is.null(bg))
                return(sprintf("#%02x%02x%02x",
                               round((reds * alphas + bg[1] * (255 - alphas)) / 255),
                               round((greens * alphas + bg[2] * (255 - alphas)) / 255),
                               round((blues * alphas + bg[3] * (255 - alphas)) / 255)
                               ))
              else
                return(structure(sprintf("#%02x%02x%02x", round(reds), round(greens), round(blues)), alpha=alphas))
            },

            rgb.colour.of = function(val) {

              ## return the appropriate 32-bit RGB integers for
              ## the value(s) in val (these are from 0 to 1)
              
              reds   <- round(rss.lin.interp(pal, colour.comp("red"), val))
              greens <- round(rss.lin.interp(pal, colour.comp("green"), val))
              blues  <- round(rss.lin.interp(pal, colour.comp("blue"), val))
              return(sprintf("#%02x%02x%02x", reds, greens, blues))
            },

            nominal.value.for = function(val) {
              ## return the nominal (scale) value for val
              ## which is between 0 and 1
              return (round(lo * (1-val) + hi * val))
            },

            choose.colour.for = function(i, is.new=FALSE) {
              ## choose the colour for palette index i
              col <- tclchar("tk_chooseColor", initialcolor = names(pal)[i],
                             title="Choose a colour for value " %:% nominal.value.for(pal[i]))
              tcl("focus", ".epal")
              if (length(col) > 0) {
                names(pal)[i] <<- col
                redraw()
              } else if (is.new) {
                pal <<- pal[-i]
                alpha <<- alpha[-i]
                redraw()
              }
            },

            delete = function(i) {
              ## delete the point with index i
              if (length(pal) > 1) {
                pal <<- pal[-i]
                alpha <<- alpha[-i]
                redraw()
              }
            },

            do.revert = function() {
              pal <<- orig.pal
              alpha <<- orig.alpha
              redraw()
            },

            do.clear = function() {
              pal <<- orig.pal[orig.pal == 0.0 | orig.pal == 1.0]
              alpha <<- orig.alpha[orig.pal == 0.0 | orig.pal == 1.0]
              if (length(pal) == 0) {
                pal <<- orig.pal[1]
                alpha <<- orig.alpha[1]
              }
              redraw()
            },

            do.ok = function() {
              RSS$palettes[[pal.name]]$desc <- get.desc()
              gui.register.palette.desc.change(pal.name)
              tcl("wm", "withdraw", epal)
            },

            do.cancel = function() {
              restore.original.palette()
              tcl("wm", "withdraw", epal)
            },

            do.save.as = function() {
              ## save this palette as something else
              ## by getting a new filename,
              ## writing it to the appropriate file
              ## adding the new palette to the
              ## appropriate RSS items (palettes, palette.fileadding
              filename <- paste(tclchar("tk_getSaveFile",
                                        filetypes='{"radR palette files" {.palette.R}} {"All files" {.*}}',
                                        title="Save this palette as...",
                                        initialdir=GUI$default.dir), collapse=" ")
              if (length(filename) > 0 && nchar(filename[1]) > 0) {
                filename <- filename[1]
                GUI$default.dir <- dirname(filename)
                if (length(grep('\\.palette.R$', filename)) == 0)
                  filename <- filename %:% ".palette.R"
                if ((RSS$palette.files[[pal.name]] != filename) && !file.copy(RSS$palette.files[[pal.name]], filename, overwrite=TRUE)) {
                  gui.popup.dialog("Error opening palette file.", "Couldn't open the file " %:% filename %:% " for saving this palette.")
                  return()
                }
                new.pal.name <- sub(".palette.R", "", fixed=TRUE, basename(filename))
                if (pal.name != new.pal.name)
                  restore.original.palette()
                skip.register <- !is.null(RSS$palettes[[new.pal.name]])
                RSS$palettes[[new.pal.name]] <- list(desc = get.desc(), points = as.numeric(pal), colours = names(pal), alpha=alpha)
                RSS$palette.files[[new.pal.name]] <- filename
                RSS$palette.pathlist <- unique(c(RSS$palette.pathlist, dirname(filename)))
                rss.rewrite.config(filename, RSS$palettes[[new.pal.name]])
                if (!skip.register)
                  gui.register.new.palette(new.pal.name)
                else
                  gui.register.palette.desc.change(pal.name)
                RSS$class.palette[[class.name]] <- new.pal.name
                gui.set.class.palette(class.name, new.pal.name)
                if (skip.register)
                  rss.palette.changed(new.pal.name)
                pal.name <<- new.pal.name
                orig.pal <<- pal
                orig.alpha <<- alpha
                set.title()
              }
            },
              
            create.window = function() {
              epal <<- ".epal"
              .Tcl("toplevel " %:% epal %:% "; wm withdraw " %:% epal)
              tcl("wm", "protocol", ".epal", "WM_DELETE_WINDOW",
                  function(){
                    tcl("wm", "withdraw", epal)
                  })

              ## the wedge polygon

              wedge.poly <<- c(1, 1, 1-wedge.width / 2, 1, 1, 1+wedge.height, 1+wedge.width / 2, 1, 1, 1)
              
              ## create controls
              label   <<- tcl("label", ".epal.label", text="")
              wedges  <<- tcl("canvas", ".epal.wedges")
              colours <<- tcl("canvas", ".epal.colours")
              info    <<- tcl("label", ".epal.info", text="Spectrum:  click=new wedge;  double-click=edit new wedge.\nWedges:  click=show value;  double-click=edit;  drag=move\nWedges:  mouse wheel=small move;  right-click=delete\nWedges:  Shift-mouse wheel, shift-click = transparency", justify="left")
              desc    <<- tcl("entry", ".epal.desc", textvariable="epal.desc")
              ok      <<- tcl("button", ".epal.ok", text="Ok", command=do.ok)
              cancel  <<- tcl("button", ".epal.cancel", text="Cancel", command=do.cancel)
              revert  <<- tcl("button", ".epal.revert", text="Revert", command=do.revert)
              clear   <<- tcl("button", ".epal.clear", text="Clear", command=do.clear)
              save.as <<- tcl("button", ".epal.saveas", text="Save as...", command=do.save.as)
              lolab  <<- tcl("label", ".epal.lolab")
              hilab  <<- tcl("label", ".epal.hilab")

              ## the editor window background colour
              bg.colour <<- rss.tclcolour.to.rgbmat(gui.xlat.tcl.colour(tclchar(".epal", "cget", "-background")))
            },

            PARENT=.GlobalEnv
            
            )
