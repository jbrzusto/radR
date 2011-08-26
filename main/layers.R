##  svn $Id: layers.R 69 2008-11-12 18:50:52Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006, 2007 John Brzustowski        
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
##########################################################################
##
## layers.R - functions for radR layers.

rss.file.output.basepath <- function() {
  ## return the basename (with full path) for output files
  ## of the given type
}

###  THOUGHT NEEDED: how to generate default names

rss.conn.info <- function(conn, item="mode")
{
  ## return information about a connection
  ## by default, the mode in which it is open
  showConnections(TRUE)[1 + c(conn), item]
}

rss.file.info.macro <- function(string, filename)
{
  ## Substitute file information for macros in a string.
  ## Macros are substituted thus:
  ##
  ##  %NAME%     -> filename
  ##  %PATH%     -> dirname(filename)
  ##  %BASENAME% -> basename(filename)
  ##  %SIZE%     -> file.info(filename)$size
  ##  %DATE%     -> file.info(filename)$mtime
  ##
  ## macros requiring file.info are substituted by NA if the
  ## file does not exist.

  string <- gsub("%NAME%", filename, string)
  string <- gsub("%PATH%", dirname(filename), string)
  string <- gsub("%BASENAME%", basename(filename), string)

  tryCatch(fi <- file.info(filename),
           error = function(e) {
             fi <- file.info("")  ## this returns a list of NA
           })
    
  string <- gsub("%SIZE%", fi$size, string)
  string <- gsub("%DATE%", fi$mtime, string)
  return(string)
}

rss.file.basename <- function(filename)
{
  ## return the radR basename of the file

  return (sub("(,[0-9]+)?(\\.[^.]+)*$", "", basename(filename)))
}

rss.file.version <- function(filename)
{
  ## return the radR version number of the filename
  ## This is NA if no version string is present, otherwise
  ## the integer contained in the version string.
  return (as.integer(rss.regexp.piece("(?<=,)[0-9]+", filename)))
}

rss.file.type <- function(filename)
{
  ## return the radR type string of the filename
  ## This is the (possibly multi-part) file extension.
  return (rss.regexp.piece("(\\.[^.]+)*$", filename))
}

rss.layer.add.type <- function(name, ...)
{
  ## create a new layer type with the given parameters
  ## that inherits from RSS$layer.types$default

  RSS$layer.types[[name]] <- strictenv(..., PARENT=RSS$layer.types$default)
}

rss.layer.drop.type <- function(name)
{
  ## remove a type of layer
  RSS$layer.types[name] <- NULL
}

rss.layer.rename.existing <- function(layer) {

  ## rename the unversioned existing copies of files for this layer
  ## the appropriate version, namely 1 plus the largest
  ## existing version

  file.types <- layer$file.types

  ## lead filename:
  file <- layer$basename %:% file.types[1]

  ## nothing to do if the lead file doesn't exist.
  if (!file.exists(file))
    return ()
  
  existing.files <- dir(path=dirname(file),
                        pattern=paste("^",
                          rss.file.basename(file),
                          ".*",
                          rss.file.type(file), sep=""))

  existing.versions <- as.integer(rss.regexp.piece("(?<=,)[0-9]+", existing.files))
  existing.versions <- existing.versions[!is.na(existing.versions)]
  if (length(existing.versions) == 0)
    new.version <- 1
  else
    new.version <- 1 + max(existing.versions)

  ## rename all associated files
  for (type in file.types) {
    file.rename(layer$basename %:% type,
                paste(layer$basename, ",", new.version, type, sep=""))
  }
}

rss.layer.genid <- function() {
  ## a 128-bit random hex string
  paste(sprintf("%02x", as.integer(rss.random.bytes(16))), collapse="")
}

rss.layer.create <- function(type, name=NULL, for.C=FALSE)
{
  ## create a layer object of the given type and name
  structure ({
    strictenv(basename = name,
              type = type,
              conns = vector("list", length=length(rss.layer.param(type, "file.types"))),
              fresh = TRUE,
              open = FALSE,
              id = rss.layer.genid(),
              for.C = for.C,
              PARENT = RSS$layers[[type]]
              )
    class = c("strictenv", "layer")
  })
}

rss.layer.choose.name <- function(layer, existing=FALSE) {
  ## choose a base-name for a layer
  ## sets layer$basename, and if that has changed,
  ## also generates a new layer$id
  ## Returns TRUE, unless the user has cancelled.

## FIXME: tons to do here.
}


rss.layer.ensure <- function(layer, mode="r")
{
  ## FIXME:  refactor this horrible piece of code!!
  
  ## implement file connection logic as described in the document "radR_file_logic.sxw"
  ## mode must be "r" or "w"
  ## If for.C is TRUE, we don't actually open the file, but return the connection with its
  ## "description" field set to name, on the assumption that C code will be opening the file.
  ## returns NULL if user cancels operation

  tryCatch(if(isOpen(conn))return(conn), error=function(...){})

  is.null <- is.null(conn)
  file.exists <- file.exists(if(is.null(name)) name <- rss.file.filename(conn) else name)

  ## use the behaviour parameters for this file group
  
  with (rss.file.get.group.props(group), {
    
    ## maybe include the "All files" filter as an option
    if (RSS$all.files.in.dialogs && ! (".*" %in% names(file.type)))
      file.type <- c(file.type, list(".*" = "All files"))
    
    if (mode == "r") {
      repeat {
        ## loop until a file is successfully opened for reading,
        ## or the operation is cancelled.
        if (file.exists) {
          if (is.null)
            conn <- file(name)
          if (!for.C)
            open(conn, "rb")
          break
        } 
        choice <- rss.gui(POPUP_DIALOG,
                          title = rss.file.info.macro(not.found.title, name),
                          msg = rss.file.info.macro(not.found.msg, name),
                          buttons = c(
                            "Try again",           #1
                            "Choose another file", #2
                            "Cancel"               #3
                            ),
                          default = 2)
        if (choice == 1)
          next
        if (choice == 2) {
          name <- rss.gui(FILE_DIALOG, "open.one", open.read.title, file.type, name)
          if (length(name) == 0 || name == "") {
            conn <<- NULL
          } else {
            conn <<- file(name)
            if (!for.C)
              open(conn, "rb")
          }
        } else if (choice == 3){
          conn <<- NULL
        }
        break
      }
    } else if (mode == "w") {
      if (!file.exists) {
        if (is.null)
          conn <<- file(name)
        if (!for.C) {
          open(conn, "w+b")
          if (!is.na(header))
            writeChar(header, con=conn, eos=NULL)
        }
      } else if (!is.null) {
        if (!for.C)
          open(conn, "a+b")
      } else {
        if (auto.versionize) {
          rss.file.rename.existing(name)
          conn <<- file(name)
          if (!for.C) {
            open(conn, "w+b")
            if (!is.na(header))
              writeChar(header, con=conn, eos=NULL)
          }
        } else if (auto.overwrite) {
          conn <<- file(name)
          if (!for.C) {
            open(conn, "w+b")
            if (!is.na(header))
              writeChar(header, con=conn, eos=NULL)
          }
        } else if (appendable && auto.append) {
          conn <<- file(name)
          if (!for.C)
            open(conn, "a+b")
        } else {
          choice <- rss.gui(POPUP_DIALOG,
                            title = rss.file.info.macro(file.exists.title, name),
                            msg = rss.file.info.macro(file.exists.msg, name),
                            buttons = c(
                              "Overwrite it",                           #1
                              "Overwrite ALWAYS for this type of file", #2
                              "Preserve a version of it",               #3
                              "Preserve ALWAYS for this type of file",  #4
                              "Move it to another location",            #5
                              if (appendable) {
                                c(
                                  "Append to it",                           #6
                                  "Append ALWAYS for this type of file"     #7
                                  )
                              }
                              ),
                            default=3, drop.down=TRUE)
          if (is.na(choice)) {
            conn <<- NULL
          } else if (choice <= 2) {
            conn <<- file(name)
            if (!for.C) {
              open(conn, "w+b")
              if (!is.na(header))
                writeChar(header, con=conn, eos=NULL)
            }
            if (choice == 2)
              rss.file.set.group.props(group, list(auto.overwrite=TRUE))
          } else if (choice <= 4) {
            rss.file.rename.existing(name)
            if (choice == 4)
              rss.file.set.group.props(group, list(auto.versionize=TRUE))
          } else if (choice == 5) {
            new.dir <- rss.gui(FILE_DIALOG, "open.dir", "Enter a new location for %NAME%", init.file=name)
            if (length(new.dir) == 0) {
              conn <<- NULL
            } else {
              file.rename(name, file.path(new.dir, basename(name)))
              conn <<- file(name)
              if (!for.C) {
                open(conn, "w+b")
                if (!is.na(header))
                  writeChar(header, con=conn, eos=NULL)
              }
            }                  
          } else if (choice <= 7) {
            conn <<- file(name)
            if (!for.C)
              open(conn, "a+b")
            if (choice == 7)
              rss.file.set.group.props(group, list(auto.append=TRUE))
          }
        }
      }
    }
  })
  return (conn)
}
