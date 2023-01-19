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

## radRutil.R - R functions needed by radR


## NOTE: this is the source file, but starting radR in the usual
## manner load()s definitions of these functions from the pre-parsed
## file "radRutil.Rdata".  If radR notices a change to this file
## (based on file modification date/time), it will use the changed
## version and recreate radRutil.Rdata from this file.

######################################################################################
###                                                                                ###
### this file contains only error handlers, function definitions and library loads ###
###                                                                                ###
######################################################################################

as.real = as.double

## handle errors by dumping to a log file and/or the text console and/or the GUI console

rss.handle.error <- function(extra.msg = "", last.dump=NULL) {
  old.handler <- options()$error
  old.warn.level <- options()$warn

  options(error = expression(NULL)) ## avoid recursive handler invocation
  options(warn = 0)
  files <- list("")
  ## determine where errors should go
  ## If no enabled GUI exists, we'll dump the error to the normal print console

  dump.to.GUI <- identical(TRUE, GUI$enabled) && identical(TRUE, GUI$errors.logged.to.console)

  ## if logging to a file is enabled, output to the error log file
  if (RSS$log.errors.to.file) {
    if (is.null(RSS$error.log.connection))
      RSS$error.log.connection <- file(RSS$error.log.file, open="w")
    files <- c(files, list(RSS$error.log.connection))
  }

  ## get the error details
  dump.frames()
  if (is.null(last.dump))
    dump <- ""
  else
    paste(capture.output(last.dump), collapse="\n")

  dump <- paste(dump, paste(capture.output(traceback()), collapse="\n"), sep="\n")
  msg <- paste(extra.msg, geterrmessage(), sep="")
  warn <- capture.output(warnings())
  if (!identical(warn, "NULL"))
    msg <- paste(msg, "Warnings:\n", paste(warn, collapse="\n"), "\n", sep="")

  ## dump to appropriate places

  for (f in files) {
    cat(paste(format(Sys.time()), ":  ", msg, dump, "\n", sep=""), file = f)
    if (!is.null(RSS$source))
      cat(paste("RSS$source: ", RSS$source, "\n"), file = f)
    if (!is.null(RSS$sink))
      cat(paste("RSS$sink: ", RSS$sink, "\n"), file = f)
    if (!is.null(RSS$scan.info$timestamp))
      cat(paste("Processing scan at: ", format(RSS$scan.info$timestamp, tz="GMT"), "\n", sep=""), file=f)
  }

  ## dump to the gui console if possible

  if (dump.to.GUI) {
    rss.gui(CONSOLE_MESSAGE, "\n" %:% msg, type="error", prompt="")
    if (dump != "") {
      rss.gui(CONSOLE_MESSAGE, "\nTraceback follows:\n", type="traceback", prompt="")
      rss.gui(CONSOLE_MESSAGE, paste(dump, "\n", sep=""), type="traceback")
    }
  } else if (! rss.get.error() %in% (RSS$errors[c("NONE", "EOF_ON_PORT")]))
    rss.gui(SHOW_ERROR, rss.get.error.msg())

  ## regardless of the error, we pause play if configured to
  if (RSS$pause.play.on.errors && RSS$play.state >= RSS$PS$PLAYING) {
    rss.gui(PAUSE_PLAY, user.generated=FALSE)
    rss.do.pause()
  }

  ## re-install this handler
  options(error = old.handler)
  options(warn = old.warn.level)
}



## a function for dumping the vars in a list (e.g. a strictenv)

vars <- function(x, max.depth=1, depth=1) {
  for (n in names(x)) {
    if (is.list(x[[n]])) {
      if (depth < max.depth)
        vars(x[[n]], max.depth, depth + 1)
      else
        cat (paste("$", n, "\nlist(...)\n\n", sep=""))
    } else if (!is.function(x[[n]])) {
      if (!is.null(x[[n]]))
        print(x[n])
      else
        cat (paste("$", n, "\nNULL\n\n", sep=""))
    }
  }
}

rss.get.error <- function() {
  ## get the internal radar error number, adding one to match RSS$errors
  .Call("radR_get_error") + 1
}

rss.get.error.msg <- function() {
  .Call("radR_get_error_msg")
}

rss.sleep <- function(ms) {
  ## sleep for ms milliseconds
  .Call("radR_sleep", ms)
}

rss.process.UI.events <- function() {
  ## give up a time slice to allow GUI/tcltk events to be processed
  switch(.Platform$OS.type,
         ## on windows, because the event loop runs forever in the foreground,
         ## we need to allow for processing events
         windows = {
           .Call("radR_process_UI_events")
         },

         ## on unix, there is nothing to do since the event loop is
         ## run once from a wait-for-input-timeout handler
         unix = {}
         )
}

rss.critical.eval <- function(expr, env=.GlobalEnv) {
  ## set the value of a variable and return its
  ## old value.  This is atomic with respect
  ## to GUI and tcltk
  return(.Call("radR_critical_eval", expr, env))
}

rss.get.and.set <- function(var, val, env=.GlobalEnv) {
  ## set the value of a variable and return its
  ## old value.  This is atomic with respect
  ## to GUI and tcltk
  return(.Call("radR_get_and_set", var, val, env))
}

"%:%" <- function(x, y) {
  ## Quick flat paste syntax
  paste(x, y, sep="")
}

"% %" <- function(x, y) {
  ## Quick single-space paste syntax
  paste(x, y, sep=" ")
}

"%~%" <- function(y, x) {
  ## regexpr match: x is a regexpr, y is a character vector
  ## Function returns a character vector with the first match of x for
  ## each element of y where one is found.

  m <- regexpr(x, y, perl=TRUE)
  found <- m > 0
  mstart <- m[found]
  mlen <- attr(m, "match.length")[found]
  return(substring(y[found], mstart, mstart + mlen - 1))
}

"%~-%" <- function(y, x) {
  ## regexpr match and remove: x is a regexpr, y is a character vector
  ## Returns those elements of y which have a match for x,
  ## but removes the portion matching x.
  ## Useful for e.g. finding the right hand sides of all strings like "NominalHz = ..."
  ## in a file.

  gsub(x, "", grep(x, y, value=TRUE), perl=TRUE)
}


## list slice operators:  get/set a named slice of a list
## pull out a named item from each element of a list
## the items in the list can be lists or environments

"%$$%" <- function(x, a) {
  ## x: a list of lists
  ## a:  barename of the item wanted from each list in x
  ## returns a list whose i'th element is x[[i]]$a
  elt<-as.character(substitute(a))
  lapply(x, function(e)e[[elt]])
}

## list slice operators:  get/set a named slice of a list
## pull out a named item from each element of a list
## the items in the list can be lists or environments

"%$0%" <- function(x, a) {
  ## x: a list of lists
  ## a:  barename of the item wanted from each list in x
  ## returns a vector whose i'th element is x[[i]]$a
  ## except that if x[[i]]$a == NULL, the i'th element
  ## is NA.  Returns integer(0) rather than list() if the list is empty.
  elt<-as.character(substitute(a))
  rv <- sapply(x, function(e){ y <- e[[elt]]; if (is.null(y)) NA else y})
  if (length(rv) == 0)
    integer(0)
  else
    rv
}

## set a named item in each element of a list, recycling
## the right hand side as necessary
## the items in the list can be lists or environments

"%$$%<-" <- function(x, a, value) {
  ## x: a list of lists
  ## a:  barename of the item to assign to in each list in x
  ## value: a list or other R object; if not a list, then
  ##        list(value) is used instead
  ## the i'th element (using [[ ]]) of value is assigned to
  ## the "a" subitem of the i'th element of x
  ## elements are recycled if length(value) < length(x)
  if (!is.list(value))
    value <- list(value)
  idx <- 1 + (seq(from=0, length=length(x)) %% length(value))
  elt<-as.character(substitute(a))
  for(i in seq(along=x))
    x[[i]][[elt]] <- value[[idx[i]]]
  return(x)
}

"%drop%" <- function(x, n) {
  ## drop n items from x
  ## if n > 0, return all but the first n items of x
  ## if n < 0, return all but the last n items of x
  ## if n == 0, return x
  if (n > 0)
    x[seq(from=n+1, length=max(0, length(x) - n))]
  else if (n < 0)
    x[seq(length=max(0, length(x) + n))]
  else
    x
}

## alternatives to sapply that return a vector
## of the appropriate type even when the list
## argument is empty

int.lapply  <- function(l, f) as.integer   (lapply(l, f))
real.lapply <- function(l, f) as.real      (lapply(l, f))
char.lapply <- function(l, f) as.character (lapply(l, f))
bool.lapply <- function(l, f) as.logical   (lapply(l, f))

## a try function that temporarily disables any
## error handler and silently ignores exceptions

rss.try <- function(expr) {
  handler <- options()[["error"]]
  options(error=NULL)
  rv <- try(expr, silent=TRUE)
  options(error=handler)
  return(rv)
}

## a function for resaving config information
## as is found in radR.conf.R and gui.conf.R

rss.rewrite.config <- function(filename, x, backup=paste(filename, "#", sep=""), outfile=filename, allow.new=FALSE)
{
  ## Rewrite a configuration file with the
  ## elements from list x.
  ##
  ## filename must be the name of a file from
  ## which the list in x was originally read
  ## using source():
  ##
  ## e.g. x<-source("conffile.R")$value
  ##
  ## The list x must contain all of the elements
  ## in the configuration file (not necessarily in the same order),
  ## but may contain additional elements which will
  ## NOT be saved to the configuration file, unless allow.new==TRUE
  ## This allows runtime variables to be stored
  ## in the list without trouble.
  ##
  ## Comments lines (beginning with "#")
  ## and whitespace lines in the file are
  ## preserved, and elements of x are
  ## interspersed between them in the output
  ## in the same way they were originally.
  ##
  ## The original file is renamed to backup
  ## before being overwritten.  If an error
  ## occurs while writing the new file,
  ## the old one is restored (although the
  ## old backup file is destroyed).

  lines <- readLines(filename)
  try(file.remove(backup))
  ## fail if we can't make a backup copy
  file.rename(filename, backup)
##  tryCatch({
  out <- file(outfile, "wb")
  ## lines default to normal data
  type <- rep(0, length(lines))
    ## mark comment and whitespace lines
  type[grep("^([ \t]*#.*)?[ \t]*$", lines)] <- 2
    ## mark end of list lines
    ## these look like " )" possibly followed by a comment
    ## and always begin with some white space, which distinguishes
    ## them from the closing parenthesis of a vector, which might
    ## be on its own line, but at the left margin.
  type[grep("^[ \t]+\\)[ \t]*,?(#.*)?[ \t]*$", lines)] <- 3

  ## in case there are compound items in the file which are not present in the
  ## data, we mark all potential compound item lines
  type[grep("^[ \t]*[a-zA-Z._0-9]+[ \t]*=[ \t]*(list|strictenv)[ \t]*\\(", lines)] <- 4

  ## it's recursive to properly handle lists
##  cat("list ( ## DO NOT MODIFY THIS LINE\n", file=out)
  do.recursive.rewrite.config(out, lines, i=1, type, x, level=1, allow.new=allow.new)
##  cat("      ) ## DO NOT MODIFY THIS LINE\n", file=out)
  close(out)
##   }, error = function(e) {
##     ## abort the output and restore the backup copy
##     try(close(out), silent=TRUE)
##     try(file.remove(filename), silent=TRUE)
##     file.rename(backup, filename)
##   })
}

do.recursive.rewrite.config <- function(out, lines, i, type, x, level, allow.new) {
  ## Note: "list" below means "list or strictenv"
  ## out: connection for receiving output
  ## lines: vector of lines from the original output file contents
  ## i: index into lines of first line to be processed
  ## type: vector of line classification
  ##       0: normal data
  ##       1: start of definition of a known named list element
  ##       2: whitespace / comment
  ##       3: end of list line (a ")," )
  ##       4: start of definition of an unknown compound list element
  ## x: list to be output
  ## level: current level of nesting, to determine indenting
  ## allow.new: if TRUE, items which are not in the file are nonetheless written out.
  ##            This is set TRUE on nesting, so that if there is a new toplevel item,
  ##            all of its nested items are added too.
  ##
  ## Starting at line i, and until all an element not in x has been reached,
  ## output lines of type 2, and each time a line of type 1 is encountered,
  ## output the value from the corresponding named element of list x.
  ## If the type of that element is "list", we do so by recursively calling
  ## this function, otherwise, by calling dput

  ## Returns the value of i, the last line examined, since this is needed by all but the top-level
  ## invocation of this function.

  ## create a regexp for searching for lines which begin named elements of
  ## the list x, using the symbol form of the name (i.e. possibly surrounded by
  ## backquotes)

  nm <- char.lapply(names(x), as.symbol)
  rx <- paste('^[ \t]*(', paste(gsub("\\.", "\\.", nm),
                                collapse="|"), ')[ \t]*=', sep="")

  delim <- if(level > 1) ",\n" else " \n"

  ## use the unbackquoted form of the name for the rest

  nm <- names(x)

  ## if we're allowing new items, keep track of which
  ## items from x have been written
  if (allow.new)
    written <- structure(rep(FALSE, length(nm)), names=nm)

  ## mark lines beginning with "elementname = " which
  ## are assumed to begin definitions of the corresponding
  ## element of the list

  type[grep(rx, lines)] <- 1

  ## mark the spot for erasing the trailing comma (0 means none)
  last.comma <- 0

  ## loop through lines in the file

  indent <- paste(rep(" ", 4 + 2 * level), collapse="")
  while (i <= length(lines)) {
    if (type[i] == 2) {
      ## copy over comment, whitespace, or "element not in x" lines
      cat(paste(lines[i], "\n", sep=""), file=out)
    } else if (type[i] == 1) {
      ## which element of x does this line define?
      j <- gsub("`", "", sub('^[ \t]*((`[^`]+`)|([^ =]+)).*$', "\\1", lines[i], perl=TRUE), fixed=TRUE)
      if (j %in% nm) {
        ## this line defines an element of x
        ## so dump the definition of that element, according to x
        if (allow.new)
          written[j] <- TRUE
        cat(paste(indent, capture.output(as.symbol(j)), " = ", sep=""), file=out)
        if (any(class(x[[j]]) == c("list", "strictenv"))) {
          cat(class(x[[j]]) %:% " ( \n", file=out)
          i <- do.recursive.rewrite.config(out, lines, i + 1, type, x[[j]], level + 1, allow.new=TRUE) - 1
          cat(paste(indent,"  )\n", sep=""), file=out)
        } else {
          dput(x[[j]], file=out, control=NULL)
          ## skip over "normal" lines until we get to a blank or comment line or new definition, which always
          ## exists after an item.  This way, data lines from a non-list multiline item are
          ## skipped (dput never outputs blank or comment lines).
          i <- i+1
          while (type[i] == 0 && i<=length(lines))
            i <- i+1
          i <- i-1
        }
        ## drop the trailing newline so the comma after
        ## the definition of this element is in a more
        ## sensible place
        last.comma <- seek(con=out, where=-1, origin="current") - 1
        cat(delim, file=out)
      }
    } else if (type[i] == 4) {
      ## We've found a compound item
      ## which is not in object x, so we assume it represents
      ## an object which no longer needs configuration, and skip its information.
      ## The only instance of this is for the "windows"
      ## item in gui.conf.R, which represents open windows.

      ## FIXME: TERRIBLE KLUDGE!!! if this item is a "list (" or "strictenv (",
      ## we skip lines until we find a ")" line at the correct indentation.
      ## Really, we should replace all this R code with a modified version of the
      ## R parser that retains whitespace and comments as attributes on tokens/expressions
      ## and a deparser that can reconstruct same.

      i <- i + 1
      goal <- indent %:% "  )"  ## the same thing we write out at the end of a list
      goal.len <- nchar(goal)   ## we compare only to the correct length, since the line might end in a comma
      while (i <= length(lines) && (type[i] != 3 || substr(lines[i], 1, goal.len) != goal))
        i <- i + 1

    } else if (type[i] == 3) {
      ## end of list reached, we are done at this level
      ## but we do want to skip this line
      i <- i + 1

      ## if we are allowing new items, write any of them now
      if (allow.new && any(!written)) {
###.if $DEBUG
        print(sprintf("Before newwrite, i=%d and length(lines)=%d\n", i, length(lines)))
###.endif
        for (j in nm[!written])
          last.comma <- do.recursive.newwrite.config(out, j, x[[j]], level)
###.if $DEBUG
        print(sprintf("After newwrite, i=%d and length(lines)=%d\n", i, length(lines)))
###.endif
      }

      break
    }
    i <- i+1
  }
  ## seek back and overwrite the extra trailing comma, if necessary
  if (last.comma > 0) {
    save.pos <- seek(con=out, where=last.comma, origin="start")
    cat(" \n", file=out)
    seek(con=out, where=save.pos, origin="start")
  }
  return(i)
}

do.recursive.newwrite.config <- function(out, name, x, level, delim=",\n") {
  ## write out an item in this form:
  ##     x = whatever,
  ## or
  ##     x = list (
  ##           a = whatever,
  ##           b = whatever,
  ##           ...
  ##           z = whatever
  ##         ),
  ##
  ## and return the location of the last comma output

  indent <- paste(rep(" ", 4 + 2 * level), collapse="")
  cat(paste(indent, capture.output(as.symbol(name)), " = ", sep=""), file=out)
  if (any(class(x) == c("list", "strictenv"))) {
    cat(class(x) %:% " ( \n", file=out)
    if (length(names(x))) {
      for (j in names(x))
        last.comma <- do.recursive.newwrite.config(out, j, x[[j]], level + 1)
      if (last.comma > 0) {
        save.pos <- seek(con=out, where=last.comma, origin="start")
        cat(" \n", file=out)
        seek(con=out, where=save.pos, origin="start")
      }
    }
    cat(paste(indent,"  )\n", sep=""), file=out)
  } else {
    dput(x, file=out, control=NULL)
  }
  last.comma <- seek(con=out, where=-1, origin="current") - 1
  cat(delim, file=out)
  return (last.comma)
}


rss.save.config <- function(object, name=NULL, filename=NULL, allow.new=FALSE) {
  ## save configuration info from an object back into
  ## appropriate files; If the object is NULL, as
  ## would happen if there were no configuration file
  ## for it to begin with, or if there is no existing
  ## configuration file, then no file is written.
  ## name should include any path required to reach
  ## the plugin e.g. ./plugins/saveblips/saveblips
  ## Typically, two configuration files are updated:
  ## NAME.conf.R and NAME.PLATFORM.conf.R
  ## If filename is set, the configuration is updated for
  ## that file only (i.e. no ".PLATFORM." file), and name
  ## is not used.

  do.platform <- FALSE
  if (!is.null(object)) {
    if (is.null(filename)) {
      filename <- name %:% ".conf.R"
      do.platform <- TRUE
    }
    if (file.exists(filename))
      rss.rewrite.config(filename, object, allow.new=allow.new)
    if (do.platform) {
      filename <- name %:% "." %:% .Platform$OS.type %:% ".conf.R"
      if (file.exists(filename))
        rss.rewrite.config(filename, object, allow.new=allow.new)
    }
  }
}

rss.load.config <- function(filebase.name, into=strictenv(PARENT=.GlobalEnv)) {
  ## for both filebase.name and paste(filebase.name, .Platform$OS.type, sep="."),
  ## call rss.do.load.config, then merge in the options from
  ## the latter to the former
  ##
  ## into: the environment or strictenv into which the config is to be loaded

  rss.do.load.config(filebase.name, into)
  rss.do.load.config(filebase.name %:% "." %:% .Platform$OS.type, into)
  return(into)
}

rss.do.load.config <- function(name, into) {
  ## read a configuration file and return the object
  ## name (XXX): is the basename for the file (possibly including a leading
  ## path) which is sought as follows:
  ##
  ## if the file XXXconf.update.R exists
  ##   read LIST from it
  ##   rename XXX.conf.update.R to XXX.conf.factory.R (deleting any old copy)
  ##   if the file XXX.conf.R exists
  ##     update LIST from the file XXX.conf.R, overwriting
  ##        any entries already in LIST, but not adding any not already there
  ##     rename XXX.conf.R to XXX.conf.old.R (deleting any old copy)
  ##   endif
  ##   copy XXX.conf.factory.R to XXX.conf.R (needed for rewrite)
  ##   rewrite LIST to XXX.conf.R
  ## else
  ##   read LIST from XXX.conf.R, if it exists
  ##
  ## In summary, if the file XXX.conf.update.R exists, it is presumed to be
  ## an update, and any items in it but not in XXX.conf.R are added to the latter.
  ## If an item is in XXX.conf.R but not in XXX.conf.update.R, it is removed.
  ## The file XXX.conf.update.R is renamed so that the update only occurs once.
  ##
  ## into: the environment or strictenv into which the config is to be loaded
  ##
  ## Returns 'into', with new assignments made from the file contents, as required.
  ##
  conf.file <- name %:% ".conf.R"
  old.conf.file <- name %:% ".conf.old.R"
  update.file <- name %:% ".conf.update.R"
  saved.update.file <- name %:% ".conf.factory.R"

  if (file.exists(update.file)) {
    rss.source.into(update.file, into)
    try(file.remove(saved.update.file))
    file.rename(update.file, saved.update.file)
    if (file.exists(conf.file)) {
      OLDLIST <- source(conf.file)$value
      ## copy over old values of still existing items
      for (i in names(OLDLIST))
        if (exists(i, into))
          assign(i, OLDLIST[[i]], into, inherits=FALSE)
      try(file.remove(old.conf.file))
      file.rename(conf.file, old.conf.file)
    }
    ## a copy of the conf file must exist in order to do
    ## a "save", so we make a copy of the saved update one
    file.copy(saved.update.file, conf.file)
    rss.save.config(into, name)
  } else {
    if (file.exists(conf.file))
      rss.source.into(conf.file, into)
  }
  return(into)
}

###
### functions for R hooks
###

rss.add.hook <- function(which, plugin.name, hook, enabled=TRUE, read.only=TRUE) {
  ## set the rss hook of type "which" for "plugin.name" to "hook"
  ## which:  a literal string consisting of the characters A-Z and "_",
  ## or a character expression
  ## plugin.name: a name of a plugin or an arbitrary user-defined name
  ## hook: a function, or a list with three elements:
  ##     $f: the plugin function, which must accept the correct number
  ##     of arguments for that plugin type
  ##     $enabled: whether the hook is initially enabled
  ##     $read.only: whether the hook only reads radR data, or modifies it
  ##     (hooks for which this flag is true are run before those for which
  ##     it is false)

  ## as a special case, if nargs() is 2, then plugin.name is a
  ## function or expression and we wrap it into an enabled, read-only
  ## hook for the plugin "user"

  sw <- substitute(which)
  if (is.name(sw) && sw == toupper(sw))
    which <- as.character(sw)
  if (! which %in% names(RSS$hooks)) {
    cat("Invalid hook name:" %:% which %:% "\nValid hook names are: "
        %:% paste(names(RSS$hooks), collapse=",") %:% "\n")
    return (FALSE)
  }

  ## if we are just passed a hook name and a function definition or
  ## expression, use the default values for enabled and readonly, and
  ## wrap the function into a hook.options object, and use "user" as
  ## the plugin name

  if (nargs() == 2) {
    pn <- substitute(plugin.name)
    if (is.call(pn) && pn[[1]] != "function") {
      ## this is a call expression that isn't just a function definition
      hook <- list(enabled=enabled, read.only=read.only, f=rss.make.closure(function(...) eval(x, .GlobalEnv), list(x=substitute(plugin.name))))
      plugin.name <- "user"
    } else if (is.function(plugin.name)) {
      hook <- list(enabled=enabled, read.only=read.only, f=plugin.name)
      plugin.name <- "user"
    } else {
      error("rss.add.hook: in two arg form, 2nd arg must be a function or expression")
    }
  } else if (is.function(hook)) {
    hook <- list(enabled=enabled, read.only=read.only, f=hook)
  }

  RSS$hooks[[which]][[plugin.name]] <- hook

  ## sort the functions on this hook so that readonly ones come first
  ## (but otherwise, the order is preserved)

  RSS$hooks[[which]] <- RSS$hooks[[which]][order(RSS$hooks[[which]] %$0% read.only, decreasing=TRUE)]
  return (TRUE)
}

rss.drop.hook <- function(which, plugin.name="user") {
  ## remove the hook of the specified type
  ## for the specified plugin.  This is called when
  ## a plugin is unloaded.

  ## which:  a literal string consisting of the characters A-Z and "_",
  ## or a character expression

  ## plugin.name: a name of a plugin or an arbitrary user-defined name

  sw <- substitute(which)
  if (is.name(sw) && sw == toupper(sw))
    which <- as.character(sw)
  RSS$hooks[[which]][[plugin.name]] <- NULL
  return (TRUE)
}

rss.hook.is.active <- function(which) {
  ## is any hook of the given type enabled?
  ## Used by some processing functions to avoid
  ## work needed only if a hook is active.

  ## which:  a literal string consisting of the characters A-Z and "_",
  ## or a character expression

  sw <- substitute(which)
  if (is.name(sw) && sw == toupper(sw))
    which <- as.character(sw)
  return (any(as.logical(RSS$hooks[[which]] %$$% enabled)))
}

rss.enable.hook <- function(which, plugin.name="user", enable=TRUE)
{
  ## enable or disable the rss hook "which"

  ## which:  a literal string consisting of the characters A-Z and "_",
  ## or a character expression

  sw <- substitute(which)
  if (is.name(sw) && sw == toupper(sw))
    which <- as.character(sw)
  enable <- as.logical(enable)
  RSS$hooks[[which]][[plugin.name]]$enabled <- enable
}

rss.disable.hook <- function(which, plugin.name="user")
{
  sw <- substitute(which)
  if (is.name(sw) && sw == toupper(sw))
    which <- as.character(sw)
  rss.enable.hook(which, plugin.name, FALSE)
}

rss.call.plugin.hook <- function(which, plugin, ...) {

  ## Call the hook function for a particular hook type and a
  ## particular plugin, if it is defined and enabled.  Used instead of
  ## rss.call.hooks when the values of "..."  should vary depending on
  ## which plugin's hook function is being called.

  hook <- RSS$hooks[[which]][[plugin]]
  if (!is.null(hook) && hook$enabled)
    hook$f(...)
}

rss.call.hooks <- function(which, ...) {
  ## For each plugin that has a function defined and enabled
  ## for the hook of type "which",
  ## call it with the passed parameters.
  ## The entire vector of hook return values is
  ## returned.

  rv <- c()
  for (hook in RSS$hooks[[which]])
    if (hook$enabled && !is.null(hook$f))
##      try(rv <- c(rv, hook$f(...)), silent=TRUE)
      rv <- c(rv, hook$f(...))
  return(rv)
}

rss.call.hooks.accum <- function(which, par) {
  ## call hook functions with accumulated parameters.
  ## The first function is passed par, the second
  ## is passed the return value of the first, and so on.
  ## A function can cheaply leave the passed parameter
  ## unchanged by returning NULL.
  ## Returns a vector corresponding to the last non-NULL
  ## value returned by an enabled hook function, or par
  ## of there are none.

  for (hook in RSS$hooks[[which]])
    if (hook$enabled && !is.null(hook$f) && !is.null(par2<-hook$f(par)))
      par <- par2
  return(par)
}

###
### function for invoking an event in the GUI
###
### If there is no GUI, this function does nothing.
### If the event is unknown, a warning is printed
### on the console.

rss.gui <- function(event, ...) {
  ## perform a gui callback, if the event
  ## is registered, otherwise do nothing
  ## if debugging, report a warning

  evt <- toupper(as.character(substitute(event)))
  if (RSS$have.gui)
    gui.handle.event(evt, ...)
}

###
### debugging functions
###

## Call the dll gdb() function, where the debugger
## has set a breakpoint so that
## we can enter gdb without a SIGINT, which we'd
## prefer to have sent to R so we can interrupt R processes
## within R.
## This requires the command "handle SIGINT noprint nostop pass" in gdb

gdb <- function(x) {

  .Call("gdb", x)
}

## Functions for dealing with actions that should be deferred until we
## start processing the next scan.  This is to avoid race conditions
## caused by interleaving of GUI events vs scan processing (which only
## happens under windows) and GUI events vs threaded get.data (which happens
## on both platforms).
## Examples of race conditions:
##
## - the user changes a blip-finding parameter part-way through processing
##   a scan; this would lead to a scan processed with a parameter set that
##   was changed at an undetermined point, leaving a poorly defined data matrix.
##
## - the user turns off blip-finding while a thread is doing get.data;
##   this will zero out the class matrix, but when the getter thread completes,
##   it will still process the scan and classify the samples, leaving junk
##   in the class matrix for the subsequent unclassified scans.
##
## Therefore, gui events which affect processing parameters or which change
## the gui's state in a way that might allow access to actions inconsistent
## with the current processing state should defer these actions until the
## start of processing for the next scan.
## This is achieved by a deferral mechanism implemented by four functions:
##
##   rss.defer.assign(sym, value, env=.GlobalEnv)
##             Defer the assignment of value (evaluated immediately) to sym in env.
##
##   rss.defer.call(f(...))
##             Defer the call to f, after evaluatings its arguments immediately.
##
##   rss.defer.eval(x, env=.GlobalEnv)
##             Defer evaluation of the expression x; NO evaluation is performed immediately.
##             And therefore, all symbols in x must be available in env.
##
##   rss.do.defers() Perform deferred assignments, calls, and evals in the
##             same order in which they were deferred
##
## Deferred calls and evals can themselves call rss.defer.*(); these 2nd
## order deferrals will be evaluated after all 1st order deferrals have been evaluated.
## 2nd order deferrals can call rss.defer.*() to create 3rd order deferrals, and so on.
##
## Notes:
##
## 1.  Deferrals only happen when RSS$play.state >= RSS$PS$PLAYING; i.e. while scans
##     are being processed.
##
## 2.  tcltk event handlers are non-reentrant, so R code called by a GUI event handler
##     does not need to be protected with rss.critical.eval

rss.defer.assign <- function(sym, value, env=.GlobalEnv) {
  ## defer the assignment of value to sym in env
  ## "value" is evaluated immediately
  ## The deferral is until the next call to rss.do.defers()

  if (RSS$play.state >= RSS$PS$PLAYING)
    RSS$defers <- c(RSS$defers, call("assign", as.character(substitute(sym)), value, envir=env))
  else
    assign(as.character(substitute(sym)), value, env)
}

rss.defer.call <- function(x) {
  ## defer the call in x
  ## arguments to the call in x are evaluated immediately
  ## (To also defer argument evaluation, use rss.defer.eval() instead)
  ## The deferral is until the next call to rss.do.defers()

  if (RSS$play.state >= RSS$PS$PLAYING) {
    x <- substitute(x)
    if (mode(x) != "call")
      stop("rss.defer.call: x must be a call")
    f <- x[[1]]
    x[[1]] <- `list`
    RSS$defers <- c(RSS$defers, as.call(c(list(eval(f, parent.frame())), eval(x, parent.frame()))))
  } else {
    ## evaluate immediately
    force(x)
  }
}

rss.defer.eval <- function(x, env=.GlobalEnv) {
  ## defer the evaluation of expression x in environment env
  ## Any symbols in x must be available in env.
  ## The deferral is until the next call to rss.do.defers()

  if (RSS$play.state >= RSS$PS$PLAYING)
    RSS$defers <- c(RSS$defers, call("eval", substitute(x), envir=env))
  else
    eval(x, env)
}

rss.do.defers <- function() {
  ## evaluate any deferred assignments, calls, and evals
  ## Some of these might themselves generate deferrals, so
  ## we continue in breadth-first order until no deferrals remain.
  ## We treat this as a critical section to prevent the GUI "thread"
  ## from interfering.

  rss.critical.eval(quote({
    while (length(RSS$defers)) {
      tmp <- RSS$defers
      RSS$defers <- list()
      for (e in tmp)
        eval(e)
    }
    rss.call.hooks(RSS$UPDATE_PARMS_HOOK)
  }))
}


rss.finalize <- function(save.config=TRUE) {
  try ({
    ## shut down the source and sink ports

    if (!is.null(RSS$source)) {
      shut.down(RSS$source)
      RSS$source <- NULL
    }

    if (!is.null(RSS$sink)) {
      shut.down(RSS$sink)
      RSS$sink <- NULL
    }

    ## unload plugins in reverse order
    ## (some might already have been unloaded by the user, so ignore those)

    for (plugin.name in rev(RSS$plugins))
      rss.unload.plugin(plugin.name, save.config)

    if (exists("gui.finalize"))
      gui.finalize()

  }, silent=TRUE)
}

rss.install.event.loop <- function()
{
  ## Set up the appropriate event handling code
  switch(.Platform$OS.type,
         ## On unix, the event loop is called with run.once=TRUE
         ## from the timeout code in Rstd_ReadConsole
         unix = .Call("radR_install_handler", "rss.event.loop"),

         ## On Windows, we do nothing.  The event loop is called
         ## with run.once = FALSE, and runs forever, calling
         ## rss.process.UI.events() at each iteration
         windows = {}
         )
}

rss.remove.event.loop <- function()
{
  ## remove the handler installed by rss.install.event.loop
  switch(.Platform$OS.type,
         ## remove the handler that calls rss.event.loop
         unix = .Call("radR_remove_handler"),

         ## on Windows, we do nothign
         windows = {}
         )
}

rss.enable.event.loop <- function(enab)
{
  ## enable/disable the radR event handler according to enab
  switch(.Platform$OS.type,
         unix = .Call("radR_enable_handler", as.integer(enab)),
         ## do nothing on windows
         windows = {}
         )
}

rss.original.debug <- debug
rss.original.undebug <- undebug

debug <- function(f) {
  ## wrapper for debug that turns off event loop handling
  ## in R
  switch(.Platform$OS.type,
         unix = {
           RSS$debug.funs <- c(RSS$debug.funs, f)
           rss.enable.event.loop(FALSE)
         },
         windows = {}
         )
  rss.original.debug(f)
}

undebug <- function(f=NULL) {
  ## wrapper for undebug that restores event loop handling
  ## in R if no other functions are being debugged
  ## With no argument, restores event loop handling unconditionally.
  switch(.Platform$OS.type,
         unix = {
           if (!is.null(f))
             RSS$debug.funs <- RSS$debug.funs[! int.lapply(RSS$debug.funs, function(x) identical(x, f))]
           if (length(RSS$debug.funs) == 0 || is.null(f)) {
             ## forget that we are debugging any functions if
             ## called with NULL
             RSS$debug.funs <- c()
             rss.enable.event.loop(TRUE)
           }
         },
         windows = {}
         )
  if (!is.null(f))
    rss.original.undebug(f)
}

rss.set.method.envs <- function (obj, env)
{
  ## for every function that is an item of obj or
  ## one of its descendents via a sequence of named
  ## lists or environments (strict or otherwise)
  ## set its environment to env

  for (i in seq(length=length(obj))) {
    if (is.function(obj[[i]])) {
      environment(obj[[i]]) <- env
    } else if (inherits(obj[[i]], c("list", "environment"))) {
      obj[[i]] <- rss.set.method.envs(obj[[i]], env)
    }
  }
  return(obj)
}

rss.load.palettes <- function() {
  ## load all available palette files
  files <- dir(path = RSS$palette.pathlist, pattern="\\.palette\\.R$", recursive=TRUE, full.names=TRUE)
  for (f in files) {
    short.name <- gsub(".palette.R", "", basename(f))
    try({
      rss.source.into(f, RSS$palettes[[short.name]] <- strictenv(PARENT=.GlobalEnv))
      RSS$palette.files[[short.name]] <- f
      ## for back compatibility, set alpha to fully opaque if it is not present
      if (is.null(RSS$palettes[[short.name]]$alpha))
        RSS$palettes[[short.name]]$alpha <- rep(255, length(RSS$palettes[[short.name]]$colours))
    }, silent=TRUE)
  }
  for (p in seq(along=RSS$class.palette))
    rss.realize.class.palette(p)
}

rss.save.palettes <- function() {
  ## resave all palettes to their
  ## original files
  for (name in names(RSS$palettes))
    try({
      rss.save.config(RSS$palettes[[name]], filename=RSS$palette.files[[name]])
    } , silent=TRUE)
}

rss.realize.class.palette <- function(class) {
  ## create the RGB matrix for the given class (1, 2, 3, ...)

  pal.name <- RSS$class.palette[[class]]
  gamma <- RSS$class.gamma[[class]]
  if (is.null(pal <- RSS$palettes[[pal.name]]))
    stop("unknown palette name: '" %:% pal.name %:% "'")

  # rss.realize.palette returns an
  RSS$palette.mat[,class] <- rss.realize.palette(pal, gamma, rss.plot.data.source.signed(), RSS$plot.is.tk.image)
}

rss.set.plot.data.source <- function(m) {
  ## set the plot data source to m
  which <- match(m, RSS$plot.data.sources)
  if (!which)
    stop("Unknown data source: " %:% m)
  if (m != RSS$plot.data.source) {
    RSS$plot.data.source <- m
    for (c in seq(along=RSS$CLASS.VAL)) {
      rss.realize.class.palette(c)
      rss.gui(PALETTE_CHANGED, c)
    }
  }
}

rss.plot.data.source.signed <- function() {
  ## is the current plot data source signed ?
  switch(RSS$plot.data.source,
         scan.mat = FALSE,
         score.mat = TRUE
         )
}

rss.realize.palette <- function(pal, gamma, signed=FALSE, tkPixelLayout = FALSE) {
  ## create the rgbint vector for the given palette
  ## and gamma.
  ## If signed is TRUE, the palette values are arranged
  ## for signed data.
  ## If tkPixelLayout is TRUE, the palette colours are returned
  ## in the order needed for a tk Image: ##AABBGGRR
  ## where AA is the alpha channel, which is set to 0xff
  ## The realized palette is returned as an rgbint vector,
  ## which is actually a real vector so that 0x80000000 can be represented
  ## without any NA problems.

  rgb <- rss.tclcolour.to.rgbmat(pal$colours, alpha=pal$alpha)
  if (!signed) {
    max.col <- 2 ^ RSS$pixel.data.bits - 1
    points.out <- rss.gamma.ramp((0:max.col) / max.col, gamma)
  } else {
    ## the trickier case of a signed palette
    ## In this case, we need to order colours according to two's complement
    ## ordering of signed values:
    ## - colours for positive values go in the first half of the palette
    ## in increasing order
    ## - colours for negative values go in the second half of the palette
    ## in decreasing order

    max.col <- 2 ^ (RSS$pixel.data.bits - 1) - 1
    points.out <- rss.gamma.scurve(c(0.5 + (0:max.col) / max.col / 2, (0:max.col) / max.col / 2), gamma)
  }
  r <- as.integer(round(rss.lin.interp (pal$points, rgb[,1], points.out)))
  g <- as.integer(round(rss.lin.interp (pal$points, rgb[,2], points.out)))
  b <- as.integer(round(rss.lin.interp (pal$points, rgb[,3], points.out)))
  alpha <- as.integer(round(rss.lin.interp (pal$points, rgb[,4], points.out)))
  return(rss.rgbmat.to.rgbint(cbind(r, g, b, alpha), tkPixelLayout))
}

rss.palette.changed <- function(pal.name) {
  ## Given that the named palette has changed, update
  ## the palette matrix (and any gui controls)
  ## for each class which uses that palette.

  for (class in which(pal.name == RSS$class.palette)) {
    rss.realize.class.palette(class)
    rss.gui(PALETTE_CHANGED, class)
  }
}


## conversions between colour representations
## tclcolour: "#R1G1B1", "#R2G2B2", ...
## rgbint: 0xA1R1G1B1, 0xA2R2G2B2, ...
##         Despite the name, these are stored as reals because we want to allow 0x80000000
##         which is as.integer(NA)
## rgbmat: matrix(c(R1, R2, ..., G1, G2, ..., B1, B2, ..., A1, A2, ...), dim=(n,4))

rss.tclcolour.to.rgbint <- function(col, for.tk=FALSE, alpha=0xff) {
  alpha.comp <- 2^24 * ifelse(alpha <= 127, alpha, alpha - 256)
  if (for.tk) {
    as.integer(paste("0x", substring(col, 6, 7), substring(col, 4, 5), substring(col, 2, 3), sep="")) + alpha.comp
  } else {
    as.integer(paste("0x", substring(col, 2), sep="")) + alpha.comp
  }
}

rss.rgbint.to.tclcolour <- function(x, for.tk=FALSE) {
  if (for.tk) {
    ## drop alpha and swap red, blue for tk image
    i24 <- as.integer(2^24)
    i16 <- as.integer(2^16)
    i8 <- as.integer(2^8)
    x <- x %% i24
    x <- (x %/% i16) + ((x %/% i8) %% i8) * i8 + (x %% i8) * i16
    sprintf("#%0.6x", x)
  } else {
    sprintf("#%0.6x", x %% as.integer(2^24))
  }
}

rss.rgbint.to.rgbmat <- function(rgbi, for.tk = FALSE) {
  alpha <- (256 + rgbi %/% as.integer(2^24)) %% 256
  rgbi <- rgbi %% as.integer(2^24)
  if (for.tk) {
    cbind(rgbi %% as.integer(256), (rgbi %/% as.integer(256)) %% as.integer(256), rgbi %/% as.integer(65536), alpha)
  } else {
    cbind(rgbi %/% as.integer(65536), (rgbi %/% as.integer(256)) %% as.integer(256), rgbi %% as.integer(256), alpha)
  }
}

rss.rgbmat.to.rgbint <- function(rgbm, for.tk = FALSE) {
  alpha <- as.integer(2^24) * ifelse(rgbm[,4] <= 127, rgbm[,4], rgbm[,4] - 256)
  if (for.tk) {
    rgbm[,3] * as.integer(65536) + rgbm[,2] * as.integer(256) + rgbm[,1] + alpha
  } else {
    rgbm[,1] * as.integer(65536) + rgbm[,2] * as.integer(256) + rgbm[,3] + alpha
  }
}

rss.tclcolour.to.rgbmat <- function(col, for.tk = FALSE, alpha = as.integer(0xff)) {
  rss.rgbint.to.rgbmat(rss.tclcolour.to.rgbint(col, alpha=alpha), for.tk)
}

rss.rgbmat.to.tclcolour <- function(rgbm, for.tk = FALSE) {
  rss.rgbint.to.tclcolour(rss.rgbmat.to.rgbint(rgbm), for.tk)
}

rss.rgbmat.blend <- function(rgbm, bg) {
  ## blend the rgb colours in rgbm with bg, using the alpha from
  ## rgbm
  alphas <- rgbm[,4]
  return (cbind(
                round((rgbm[,1] * alphas + bg[1] * (255 - alphas)) / 255),
                round((rgbm[,2] * alphas + bg[2] * (255 - alphas)) / 255),
                round((rgbm[,3] * alphas + bg[3] * (255 - alphas)) / 255),
                alphas
                ))
}



rss.get.plugin.list <- function() {
  ## Get the list of available radR plugins
  ## These are files ending in ".plugin.R" and in one
  ## of the folders (or its subfolders) listed in
  ## RSS$plugin.pathlist.
  ##
  ## Creates a list whose names are unique short plugin names,
  ## namely the part of the filename before ".plugin.R", not including any path,
  ## and whose elements are two element lists:
  ##  $label = a short description of the plugin
  ##  $file = the full pathname to the plugin
  ##  $version = the plugin version
  ##
  ## The list is saved in RSS$available.plugins

  files <- dir(path = RSS$plugin.pathlist, pattern="\\.plugin\\.R$", recursive=TRUE, full.names=TRUE)
  rv <- list()
  for (f in files) {
    short.name <- gsub(".plugin.R", "", basename(f))
    try({
      info <- source(file.path(dirname(f), paste(short.name, ".desc.R", sep="")))$value
      rv[[short.name]] <- strictenv(name=short.name, plugin.label=info$label, plugin.file=f, version=info$version, PARENT=.GlobalEnv)
    }, silent=TRUE)
  }
  RSS$available.plugins <- rv
  RSS$all.plugin.names <- char.lapply(rv, function(x)x$name)
  rss.gui(HAVE_PLUGINS)
}

rss.source.into <- function(filename, env) {
  ## parse a file and evaluate the expressions in
  ## the given environment
  e <- parse(filename)
  for (i in seq(along=e))
    eval(e[[i]], env)
}

rss.source.as.list <- function(filename) {
  e <- strictenv(PARENT=.GlobalEnv)
  rss.source.into(filename, e)
  return(as.list(e))
}

rss.load.object.and.config <- function(filename, into = new.env(hash=TRUE, parent=.GlobalEnv)) {
  ## load the object in filename "XXX.WHATEVER.R", along with the
  ## configuration stored in "XXX.conf.R" into the environment (or strictenv)
  ## "into".
  ## We check for an up-to-date binary preparsed version of the
  ## file and load that if it exists.

  base.name <- sub("\\.[^.]*\\.R", "", filename)
  pp <- filename %:% "Data"
  if (file.exists(pp)) {
    conf.names <- base.name %:% c(".plugin.R", ".conf.R", "." %:% .Platform$OS.type %:% ".conf.R", ".conf.update.R")
    mtimes <- file.info(c(pp, filename, conf.names[file.exists(conf.names)]))$mtime
    if (all(mtimes[1] >= mtimes[-1])) {
      load(pp)
      return(into)
    }
  }
  rss.load.config(base.name, into)
  ## load and evaluate the object into the "into" object,
  exprs <- parse(file=filename)
  for (expr in exprs) {
    eval(expr, into)
  }
  ## WARNING: we don't want to write the actual parent environment
  ## for an object (e.g. a tracker model's parent environment
  ## is TRACKER, not .GlobalEnv.  So set the parent.env() to
  ## .GlobalEnv, on the assumption that after load()ing the object
  ## back in, the caller will take care of setting its parent.env

  if (!identical(saved.parent <- parent.env(into), .GlobalEnv))
    parent.env(into) <- .GlobalEnv
  save(list="into", file=pp)
  parent.env(into) <- saved.parent
  return(into)
}

rss.load.plugin <- function(plugin.name, manually=TRUE) {
  ## load a plugin identified by plugin.name
  ## This will be an index into the list returned by rss.get.plugin.list()
  ## If the plugin has already been loaded but not unloaded since then,
  ## it is NOT reloaded.
  ## Returns the plugin object or NULL on failure

  if (!plugin.name %in% names(RSS$available.plugins)) {
    cat(paste("Not loading unknown plugin: '", plugin.name, "'\n", sep=""))
    return (NULL)
  }

  ## is plugin already loaded?
  if (plugin.name %in% RSS$plugins)
    return ()

  ## the plugin includes the fields from the available plugin list
  plugin <- RSS$available.plugins[[plugin.name]]
  plugin$name <- plugin.name

  ## read the plugin and its configuration file, appending them to the object
  ## with the fields from the ".desc.R" file
  ## The plugin is saved in a global object whose name is the plugin.name in upper case
  ## The configuration file is PATH_TO_PLUGIN/plugin.nameconf.R

  plugin <- rss.load.object.and.config(plugin$plugin.file, plugin)
  assign(toupper(plugin.name), plugin, .GlobalEnv)

  ## make sure any functions defined by the plugin have
  ## the plugin as their environment so that plugin
  ## functions and variables can be referred to by their unqualified names.

###  No longer needed, since we're evaluating the parsed expressions in the
###  plugin's environment, so all functions defined there get the plugin as their
###  environment.
###
###  rss.set.method.envs(plugin, plugin)

  ## for each item in the plugin's "globals" list,
  ## install that item globally.

  if ("globals" %in% plugin) {
    n <- names(plugin$globals)
    for (i in n)
      assign(i, plugin$globals[[i]], .GlobalEnv)
    rm("globals", pos=plugin)
  }

  ## call the plugin's load() function.
  ## The load() function can make any required changes to the plugin
  ## object before it is installed, including changing its enabled flag,
  ## or customizing its menu.  If it returns NA, then the plugin is
  ## not loaded, and is removed from the "plugins.to.load" list.

  if ("load" %in% plugin) {
    if (identical(plugin$load(), NA)) {
      rm(list=c(n, toupper(plugin.name)), envir=.GlobalEnv)
      RSS$plugins.to.load <- RSS$plugins.to.load[RSS$plugins.to.load != plugin.name]
      rss.gui(POPUP_MESSAGEBOX, "Plugin load cancelled", "The " %:% plugin.name %:% " plugin was not loaded.  To prevent me from\ntrying to load it next time you run radR,\nsave settings when you quit radR.")
      return()
    }
  }

  ## install this plugin in the global list
  ## its name is appended to RSS$plugins, and the plugin object itself
  ## is installed in the global environment under its upper-cased name
  ## WARNING: any changes made to the variable "plugin" within
  ## this function will have no effect on the global plugin object

  RSS$plugins <- c(RSS$plugins, plugin.name)

  ## remove this plugin from the available list
  RSS$available.plugins[[plugin.name]] <- NULL

  ## install the plugins hook functions
  if ("hooks" %in% plugin)
    for (hook in names(plugin$hooks))
      rss.add.hook(hook, plugin$name, plugin$hooks[[hook]])

  ## for each plugin method (i.e. each item in the
  ## plugin's strictenv or in any descendent that is a function), set its
  ## environment to the plugin itself.  That way items
  ## of the plugin are directly accessible.

  ## enable the plugin, if it wants to be

  if (identical(TRUE, plugin$enabled))
    rss.enable.plugin(plugin.name)

  ## tell the GUI
  rss.gui(PLUGIN_LOADED, plugin, manually)

  return(plugin)
}

rss.enable.plugin <- function(plugin.name, enable = TRUE) {
  ## enable this plugin
  ## i.e. enable all hooks set by this plugin

  ## is plugin loaded?
  if (! plugin.name %in% RSS$plugins)
    return (FALSE)

  plugin <- get(toupper(plugin.name), envir=.GlobalEnv)

  ## call the plugin enable function, if defined
  if ("enable" %in% plugin)
    plugin$enable(enable)

  ## mark the plugin as enabled or not
  plugin$enabled <- enable
}

rss.disable.plugin <- function(plugin.name) {
  ## disable this plugin
  rss.enable.plugin(plugin.name, FALSE)
}

rss.unload.plugin <- function(plugin.name, save.config=TRUE) {

  if (! plugin.name %in% RSS$plugins)
    stop("rss.unload.plugin: plugin '" %:% plugin.name %:% "' is not loaded")
  plugin <- get(toupper(plugin.name), envir=.GlobalEnv)

  ## remove the plugins hook functions
  if ("hooks" %in% plugin)
    for (hook in names(plugin$hooks))
      rss.drop.hook(hook, plugin$name)

  ## save the configuration (if any) associated with this plugin
  if (identical(TRUE, save.config)) {
    conf.name <- file.path(dirname(plugin$plugin.file), plugin.name)
    rss.save.config(plugin, conf.name)
  }

  ## move this plugin from the list of loaded plugins
  ## to the list of available ones
  RSS$available.plugins[[plugin.name]] <- as.strictenv(plugin[c("name", "plugin.label", "plugin.file", "version")], PARENT=.GlobalEnv)
  RSS$plugins <- RSS$plugins[RSS$plugins != plugin.name]

  ## update the GUI to reflect this
  rss.gui(PLUGIN_UNLOADED, plugin)

  ## drop the source and sink if they belong to the corresponding plugin class

  if (inherits(RSS$source, plugin.name))
    rss.set.no.port("source")

  if (inherits(RSS$sink, plugin.name))
    rss.set.no.port("sink")

  ## call the plugin's unload function (if any)

  if ("unload" %in% plugin)
    try(plugin$unload(save.config), silent=TRUE)

  ## remove the plugin's uppercase name binding in the global environment
  ## (do this after rss.gui(PLUGIN_UNLOADED, ...) so that any plugin calls required there that use
  ## the upper case name still work)
  rm(list=toupper(plugin.name), envir = .GlobalEnv)
}

rss.get.shared.constants <- function() {
  ## Read the list of shared constants, namely error and hook codes
  ## from the file radRshared.h
  ## this is an easy way to keep the codes consistent between
  ## the R and C code, but means radRshared.h must be distributed along
  ## with radR.  Not a problem.  You get all the source for free anyway.

  lines <- readLines("main/radRshared.h")

  ## The types for large data matrices

  typedefs <- grep("^#define T_[A-Z_]*_TYPE", lines, value=TRUE)
  typenames <- tolower(gsub("(^#define T_)|(_TYPE .*$)", "", typedefs, perl=TRUE))
  typetypes <- gsub("(^#define[A-Z_ \t]*)|([ \t]*(//.*)?$)", "", typedefs, perl=TRUE)

  RSS %$% types <- list()
  for (i in seq(along=typenames))
    RSS$types[[typenames[i]]] <- typetypes[i]

  ## Miscellaneous data-size constants

  RSS %$% num.sample.classes <- as.integer(strsplit(grep("^#define NUM_SAMPLE_CLASSES", lines, value=TRUE), "[ \t]+")[[1]][3])
  RSS %$% pixel.data.bits <- as.integer(strsplit(grep("^#define T_PIXEL_DATA_BITS", lines, value=TRUE), "[ \t]+")[[1]][3])
  RSS %$% score.bits <- as.integer(strsplit(grep("^#define T_SCORE_BITS_USED", lines, value=TRUE), "[ \t]+")[[1]][3])
  RSS %$% score.scale <- 2 ^ as.integer(strsplit(grep("^#define T_SCORE_FRACTIONAL_BITS", lines, value=TRUE), "[ \t]+")[[1]][3])

  ## The error codes
  e<-list()
  lapply(strsplit(grep("^#define RADR_ERROR", lines, value=TRUE), "[ \t]+"),
         function(x) e[sub("RADR_ERROR_", "", x[2])]<<-1 + as.numeric(x[3]))
  em <- character(0)
  em[e[["NONE"]]] <- "(no error)"
  em[e[["NOMEM"]]] <- "Out of memory"
  em[e[["BAD_SERVER"]]] <- "Bad server number"
  em[e[["BAD_PORT"]]] <- "Bad port number"
  em[e[["EOF_ON_PORT"]]] <- "End of file reading from port"
  em[e[["FILE_ERROR_ON_PORT"]]] <- "Unknown file error on port"
  em[e[["CANT_OPEN_ARCHIVE"]]] <- "Can't open archive:  filename invalid or file already in use"
  em[e[["INVALID_ARCHIVE"]]] <- "File is not an archive"
  em[e[["NO_ARCHIVE_DIR"]]] <- "Archive does not contain a directory"
  em[e[["NO_CURRENT_SRC"]]] <- "No source port selected"
  em[e[["SOURCE_NOT_ARCHIVE"]]] <- "Source is not an archive"
  em[e[["INVALID_RUN"]]] <- "Run index out of range for archive"
  em[e[["PORT_NOT_SOURCE"]]] <- "Port is not a source"
  em[e[["PORT_NOT_SINK"]]] <- "Port is not a sink"
  em[e[["SEEK_BEYOND_ARCHIVE"]]] <- "Attempt to seek to a time outside of archive"
  em[e[["INVALID_SCAN"]]] <- "Scan index out of range for this archive run"
  em[e[["ARCHIVE_IN_USE"]]] <- "Archive file in use by another port"
  em[e[["UNKNOWN_PORT_ERROR"]]] <- "Unknown error on port"
  em[e[["CONTENTS_UNAVAILABLE"]]] <- "Port does not have a table of contents"
  RSS$errors <- e
  RSS$error.messages <- em

  ## The radR processing hooks.
  ## each hook corresponds to a list whose names
  ## are plugin names, and whose values are functions
  ## A definition like
  ##
  ##   #define RADR_HOOK_FULL_SCAN 4
  ##
  ## leads to
  ##
  ##   RSS$FULL_SCAN_HOOK <- 4
  ##   RSS$hooks[[FULL_SCAN]] <- list()
  ##

  RSS$hooks <- strictenv()

  lapply(strsplit(grep("#define RADR_HOOK", lines, value=TRUE), "[ \t]+"),
         function(x) {
           hookname <- sub("RADR_HOOK_", "", x[2])
           RSS[[paste(hookname, "HOOK", sep="_"), NEW=TRUE]] <- as.integer(x[3])
           RSS$hooks[[hookname, NEW=TRUE]] <- list()
         })

}

rss.time.now <- function() {
  ## return the time of the beginning of the current scan
  ## as a double; it is seconds since the Epoch
  as.numeric(RSS$scan.info$timestamp)
}

rss.get.message.for.error <- function(e) {
  if (is.numeric(e))
    RSS$error.messages[1 + e]
  else
    paste("Problem getting error message for ", as.character(e))
}

rss.make.closure <- function(f, vars, parent=.GlobalEnv) {
  ## define a closure with the formals and body of f and a fixed environment
  ## consisting of a list of defined variables
  ## and a parent frame
  if (!is.environment(vars)) {
    e <- new.env(parent = parent)
    for (n in names(vars))
      assign(n, vars[[n]], e)
  } else {
    e <- vars
  }
  environment(f) <- e
  return(f)
}


## replace the quit function
rss.old.q.function <- q

q <- function(force=FALSE) {
  try({
    if (RSS$have.gui && !force) {
      switch(rss.gui(CONFIRM_QUIT),
             yes = {
               rss.gui(SAVE_CONFIG)
               rss.save.config(RSS, "main/radR")
               rss.save.palettes()
               ## flag to save configuration, used by
               ## plugin finalizers
               RSS$save.config <- TRUE
             },
             no = {
               RSS$save.config <- FALSE
             },
             cancel = {
               return()
             })
    }
    ## time to quit, call any installed hooks
    rss.call.hooks(RSS$ONEXIT_HOOK)

  }, silent=TRUE)
  ## call the builtin quit function, which in turn
  ## will call the .Last function

  .Last()

  ## fix a readline bug that leaves the terminal in a non-echoing state
  if (.Platform$OS.type == "unix")
    .Call("radR_fix_readline_problem")

  ## restore the original q() function and remove .Last()
  rss.old.q.function(save="no", runLast=FALSE)
}

## create the function to shut down radR on exit

.Last = function() {
  rss.finalize(RSS$save.config)
}

rss.patch.at.sample.pulse <- function(s, p) {
  ## return an n by 2 matrix of coordinates for the patch which
  ## includes the given sample (s) and pulse(p).
  ## Both sample and pulse are numbered starting from 1.
  ## The first column of the matrix is the sample, the second is the pulse.
  ## and n is the number of samples in the patch.
  ## (returns NULL if there is no such patch)

  ## If p is missing, s is assumed to be a vector providing both coordinates.
  ## The returned coordinate matrix can be used directly to index
  ## external matrices such as the scan mat:
  ## e.g. RSS$scan.mat[rss.patch.at.rc(c(100,140))]
  ## The matrix has attribute "index", which as a 2 element integer vector:
  ## attr(, "index")[1] : the index of the patch within all patches from this scan
  ## attr(, "index")[2] : if this patch is a blip, the index of the blip within all
  ##                      blips from this scan, otherwise 0.

  if (!missing(p))
    s <- c(s, p)

  ## we subtract 1 to convert from origin 1 to origin 0
  .Call("radR_patch_at_sp", s - 1, RSS$patch.buffer)
}

rss.rps <- function() {
  ## return the true range per sample, in metres
  RSS$scan.info$sample.dist
}

rss.antenna.angle <- function() {
  RSS$scan.info$antenna.angle[1] * pi / 180
}

rss.planar.rps <- function() {
  ## return the planar range per sample, in metres
  ## this is the axial range per sample, adjusted for the antenna axis angle
  rss.rps() * cos (rss.antenna.angle())
}

rss.elev.per.sample <- function() {
  ## how much does elevation increase per sample, in metres
  ## zero, if RSS$scan.info$antenna.angle == 0
  rss.rps() * sin (rss.antenna.angle())
}

rss.origin.elev <- function() {
  ## return the elevation of the origin, in metres
  RSS$scan.info$elevation
}

rss.xy.to.sp <- function(x, y) {
  ## convert xy locations to sample, pulse pairs
  ##
  ## where x is metres East of the radar, and y is metres North
  ##
  ## Returns an n x 2 integer matrix with columns sample and pulse

  cbind(as.integer(
                   1 + floor(sqrt(x^2 + y^2) / rss.planar.rps()
                             - RSS$scan.info$first.sample.dist / RSS$scan.info$sample.dist)),
        as.integer(
                   1 + floor(((pi / 2 - atan2(y, x) + (180 / RSS$scan.info$pulses - RSS$scan.info$bearing.offset) * pi / 180) %% (2 * pi)) / (2 * pi) * RSS$scan.info$pulses)))
}

rss.sp.to.rb <- function(s, p) {
  ## convert sample, pulse coordinates to range, bearing
  ## the middle of a [range x pulse] cell is used, and the pulse
  ## at North is treated as being centred on North.
  ## s: a vector of sample indexes, 1 <= s
  ## p: a vector of pulse indexes, 1 <= p <= RSS$scan.info$pulses
  ##
  ## returns a list with elements "range" (m) and "bearing" (degrees)
  return (list (
                range   = RSS$scan.info$first.sample.dist + (s - 0.5) * RSS$scan.info$sample.dist,
                bearing = RSS$scan.info$bearing.offset    + (p - 1.0) * (360 / RSS$scan.info$pulses)
                )
          )
}

rss.sp.to.sph <- function(s, p, meta = RSS$scan.info) {
  ## convert sample,pulse coordinates to spherical coordinates (r, theta, phi)
  ## where:
  ##  cbind(s, p): an n x 2 matrix of sample,pulse coordinates
  ## returns an n x 3 matrix = cbind(r, theta, phi) where:
  ##  r: range from radar (metres)
  ##  theta: compass angle (degrees clockwise from north)
  ##  phi:  elevation angle (degre
  ##
  ## Uses metadata in meta.
  if (!missing(p))
    s <- cbind(s, p)
  with(meta,
    cbind(r = first.sample.dist + sample.dist * (s[,1] - 0.5),
          theta = (bearing + bearing.offset) + orientation * (s[, 2] - 1) * (360 / pulses),
          phi = antenna.angle[1])
       )
}

rss.blip.perim <- function(blip, spatial=TRUE, num.pulses = RSS$scan.info$pulses, dr = rss.planar.rps()) {
  ## blip is an n x 2 matrix of
  ## sample locations, column 1 is sample slot,
  ## column 2 is pulse #

  ## num.pulses is the number of pulses in a full scan, and is
  ## required to properly calculate the perimeter of a patch that
  ## straddles the zero degree azimuth.

  ## spatial, if TRUE, means samples are treated in space, and so
  ## their inner and outer sides change in perimeter with increasing
  ## range.

  ## dr is the change in range per sample.  the perimeter will be returned
  ## in the same units.  if spatial==FALSE, no value for dr is required
  ## we assume that sample 1 spans the range [orig, dr], sample 2 the range [orig+dr, orig+2*dr],
  ## and so on, where orig is RSS$scan.info$first.sample.dist

  num.samples <- dim(blip)[1]

  if (spatial) {
    dtheta <- 2 * pi / num.pulses
    orig <- RSS$scan.info$first.sample.dist
  }

  ## get the total perimeter of all samples

  all.sample.perim <- if (spatial) {
    (2 * num.samples * dr) + dtheta * (dr * sum(2 * blip[,1] - 1) + 2 * num.samples * orig)
  } else {
    4 * num.samples
  }

  ## now compute the "total" perimeter accounted for by edges
  ## between adjacent samples

  ## a big number, larger than one plus the max possible pulse or sample #

  big.num <- max(blip) + 2

  ## sort the blip array samplewise, then pulsewise

  sample.by.sample <- blip[order(blip[,2], blip[,1]),]

  ## if the patch straddles the zero azimuth, we duplicate its last pulse
  ## but give it pulse # 0 so that it appears adjacent to pulse # 1

  if (any(blip[,2] == num.pulses) && any(blip[,2] == 1))
    blip <- rbind(blip, cbind(blip[blip[,2] == num.pulses, 1], 0))

  pulse.by.pulse <- blip[order(blip[,1], blip[,2]),]

  ## linearize the sample slot and pulse numbers so that adjacent
  ## samples differ by one in the appropriate vector

  lin.sample.nums <- sample.by.sample[,1] + big.num * sample.by.sample[,2]
  lin.pulse.nums  <- pulse.by.pulse  [,2] + big.num * pulse.by.pulse  [,1]

  int.edge.perim <- if (spatial) {
    radial.int.edges <- which(diff(lin.sample.nums) == 1)
    dtheta * (dr * sum(sample.by.sample[radial.int.edges, 1]) + orig * length(radial.int.edges)) +
      dr * sum(diff(lin.pulse.nums) == 1)
  } else {
    sum(diff(lin.sample.nums) == 1) + sum(diff(lin.pulse.nums) == 1)
  }

  ## the perimeter is the sum of all sample perimeters minus twice the
  ## lengths of internal edges

  return(all.sample.perim - 2 * int.edge.perim)
}

rss.get.all.blips <- function(blip.nums=TRUE, linear.coords=FALSE, collapse=TRUE, which.patches=NULL) {
  ## return an integer matrix with coordinates of all blips, or specified patches
  ## this will have dimensions
  ##    n x (2 + blip.nums - linear.coords)
  ## where n is the total number of hot samples in all
  ## blips or specified patches.
  ## If blip.nums==TRUE, the first column is blip number.
  ## If linear.coords==TRUE, the last (and possibly only) column
  ## holds the linear coordinates of each sample in each blip.
  ## If linear.coords==FALSE, the last two (and possibly only two)
  ## columns hold the sample# and pulse# of each sample in each blip.
  ## If blip.nums == FALSE and linear.coords == TRUE, the n x 1
  ## matrix is collapsed to a vector
  ## If which.patches = NULL, those patches currently accepted as blips
  ## (in RSS$patch.buffer) are retreived.
  ## Otherwise, which.patches represents the indices of patches to retrieve,
  ## either as a logical vector with length nrow(RSS$patches), or as an integer vector
  ## all of whose elements are in 1..nrow(RSS$patches).

  if (is.integer(which.patches)) {
    tmp <- rep(FALSE, nrow(RSS$patches))
    tmp[which.patches] <- TRUE
    which.patches <- tmp
  }
  if (RSS$have.valid$patches && (RSS$num.blips > 0 || sum(which.patches) > 0))
    rv <- .Call("radR_get_all_blips", RSS$patch.buffer, as.logical(blip.nums), as.logical(linear.coords), which.patches)
  else
    rv <- matrix(integer(0), 0, 2 + blip.nums - linear.coords)
  if (collapse && linear.coords && ! blip.nums )
    dim(rv) <- NULL
  rv
}

rss.summarize.patches <- function(patches, area.weighting = TRUE) {
  ## given a matrix of coordinates for one or more patches,
  ## returns a data frame with one row per patch summarizing
  ## the patch's geometry etc.
  ##
  ## if area.weighting == TRUE,  centroid weighting is by sample area
  ## if area.weighting == FALSE, centroid weighting is by sample intensity
  ##
  ## Columns in patches:
  ##   patch  number   (optional)
  ##   sample number
  ##   pulse  number
  ## Rows with the same patch number are part of the same patch.
  ## If there are only two columns, patches is treated as a single
  ## patch.
  ##
  ## Columns in the return value:
  ## x, y, z : the patch centroid in metres east, north, and up from the radar
  ## t       : the time corresponding to the azimuth of the patch centroid
  ## area    : area of the patch in metres squared
  ## intensity: from 0 to 1

  if (dim(patches)[1] == 0)
    return(data.frame())

  info             <- RSS$scan.info
  scan.date        <- as.numeric(info$timestamp)
  max.sample.value <- 2 ^ info$bits.per.sample - 1
  dtheta           <- 2 * pi / info$pulses
  area.conversion  <- rss.planar.rps() ^ 2 * dtheta

  ## make blip.num a factor
  if (dim(patches)[2] == 3) {
    sn <- 2
    pn <- 3
    blip.num <- as.factor(patches[, 1])
  } else {
    sn <- 1
    pn <- 2
    blip.num <- as.factor(rep(1, dim(patches)[1]))
  }

  ## grab raw sample values for all tracks; drop=FALSE in case there's only one row in patches

  value <- RSS$scan.mat[patches[, sn:pn, drop=FALSE]]

  ## Because a sample at close range represents a smaller
  ## area of coverage than one at longer range,
  ## we weight all samples according to their range.

  ## area of each blip in "virtual sectors"
  weight <- as.vector(tapply(patches[, sn], blip.num, sum))

  ## area of each blip in square metres
  area <- weight * area.conversion

  ## intensity will be from 0 to 1 and is absolute, not z-scored
  int <- as.vector(tapply(value * patches[, sn], blip.num, sum) / weight / max.sample.value)

  ## convert the nominal angles to radians; pulse 1 is centred at info$bearing + info$bearing.offset

  theta <- pi / 2 - info$orientation * (patches[, pn] - 1) * dtheta + (info$bearing + info$bearing.offset) * (pi / 180)

  ## calculate the range of the sample midpoint, in samples
  axial.range <- (info$first.sample.dist / info$sample.dist + patches[, sn] - 0.5)

  ## x = easting, y = northing, z = elevation

  rps <- rss.planar.rps()
  x <- axial.range * cos(theta) * rps
  y <- axial.range * sin(theta) * rps
  z <- axial.range * rss.elev.per.sample()

  if (area.weighting) {
    ## centroid is area-weighted x, y, and z coordinates:

    x <- as.vector(tapply(x * patches[, sn], blip.num, sum) / weight)
    y <- as.vector(tapply(y * patches[, sn], blip.num, sum) / weight)
    z <- as.vector(tapply(z * patches[, sn], blip.num, sum) / weight) + RSS$scan.info$elevation
  } else {
    ## centroid is weighted by intensity
    weight <- as.vector(tapply(value, blip.num, sum))
    x <- as.vector(tapply(x * value, blip.num, sum) / weight)
    y <- as.vector(tapply(y * value, blip.num, sum) / weight)
    z <- as.vector(tapply(z * value, blip.num, sum) / weight) + RSS$scan.info$elevation
  }
  ## the time we associate with a blip is the timestamp plus timeoffset plus
  ## a number of seconds corresponding to the mean angle
  ## we're assuming the timestamp corresponds to the first
  ## scan row

  t <- scan.date + as.vector(tapply(patches[, pn], blip.num, mean)) / info$pulses * info$duration / 1000

  ## bind all calculated columns together into a data frame
  return(data.frame(t, x, y, z, area, int, row.names=NULL))
}

rss.set.palette <- function(class, rgb) {
  ## set the palette for class to rgb
  ## "class" is 1..RSS$num.sample.classes
  ## "rgb" is a 3 x (2^RSS$pixel.data.bits) matrix
  ## with first row giving red, second green, third blue
  rgb <- structure(as.integer(rgb), dim=dim(rgb))
  RSS$palette.mat[, class] <- rgb[1,] * 65536 + rgb[2,] * 256 + rgb[3,]
}

## A gamma function for a palette that "shifts" toward its
## higher values for gamma > 1 is:

rss.gamma.ramp <- function(x, gamma) x ^ (1 / gamma)

## A gamma function for a palette that shifts symmetrically away from
## its midpoint for gamma > 1 (i.e. an S curve)

rss.gamma.scurve <- function (x, gamma) {
  ifelse(x >= 0.5,
         0.5 + ((2*(x - 0.5)) ^ (1 / gamma)) / 2,
         0.5 - ((2*(0.5 - x)) ^ (1 / gamma)) / 2)
}

## a linear interpolation function that deals
## with (effectively) single point input and collapses
## to the endpoints if x.out is out of range

rss.lin.interp <- function(x.in, y.in, x.out) {
  if (length(unique(sort(x.in))) > 1)
    approx(x.in, y.in, x.out, rule=2)$y
  else
    rep(y.in[1], length(x.out))
}

rss.convert.scan <- function(reconv,
                             scan.mat = RSS[[rss.plot.data.source()]],
                             class.mat = RSS$class.mat,
                             show.class = as.integer(RSS$show.class),
                             paint.background = TRUE,
                             scan.converter = NULL,
                             geom=c(0, 0, dim(RSS$pix.mat))
                             ) {
  ## if necessary, scan-convert the data and copy the new pixels to the plot device

  ## make sure we have a pixel matrix, either provided by the GUI or
  ## our own; get the original dimensions so we can check whether they
  ## have changed (which forces a scan conversion)

  ## Return Value: an EXTPTRSXP pointer to a scan_converter object which can be used
  ## the next time the same geometrical conversion occurs

  old.dims <- dim(RSS$pix.mat)
  if (! GUI$plot.is.tk.image) {
    if (GUI$enabled)
      RSS$pix.mat <- .Call("prepare_for_plot", PACKAGE = GUI.PACKAGE)
    else if (is.null(RSS$pix.mat))
      RSS$pix.mat <- extmat("pixels of scan-converted data" , type=RSS$types$pixel, dim=RSS$default.pix.mat.dim)
  }


  ## if required to paint the background and the PAINT_BACKGROUND
  ## hook doesn't return any TRUE values, paint the pixel matrix
  ## with background colour

  if (paint.background && !any(rss.call.hooks(RSS$PAINT_BACKGROUND_HOOK)))
    RSS$pix.mat[NOTRIGGER=TRUE] <- rss.tclcolour.to.rgbint(RSS$pix.mat.background, RSS$plot.is.tk.image)

  RSS$have.valid$bitmap <- TRUE

  if(RSS$have.valid$scan.data && ! RSS$skip$convert.scan &&
     (reconv || !identical(old.dims, dim(RSS$pix.mat)))) {

    ## call the pre scan convert hook
    rss.call.hooks(RSS$PRE_SCAN_CONVERT_HOOK)

    ## if rectangular coordinates, allow an offset
    if (isTRUE(RSS$scan.info$is.rectangular)) {
      plot.origin <- GUI$plot.origin  + RSS$scan.info$origin * gui.pps() * c(1, -1)
    } else {
      plot.origin <- GUI$plot.origin
    }
    ## convert the scan
    if(!is.null(scan.converter <- .Call("radR_convert_scan",
                                        scan.mat,
                                        RSS$pix.mat,
                                        class.mat,
                                        RSS$palette.mat,
                                        plot.origin,
                                        gui.pps(),
                                        GUI$north.angle + RSS$scan.info$bearing + RSS$scan.info$bearing.offset,
                                        as.integer(rss.plot.data.source.bit.shift()),
                                        show.class,
                                        scan.converter,
                                        geom,
                                        RSS$scan.info$first.sample.dist / RSS$scan.info$sample.dist,
                                        isTRUE(RSS$scan.info$is.rectangular)
                            )))
    rss.call.hooks(RSS$POST_SCAN_CONVERT_HOOK)
  }
  return(scan.converter)
}

rss.plugin.error <- function(msg) {
  ## a plugin has generated an error
  RSS$R.level.error <- RSS$errors$PLUGIN_ERROR
  RSS$R.level.error.msg <- msg
}

rss.plot.data.source <- function() {
  ## return the current plot data source
  ## ensuring that the appropriate data are
  ## present

  switch(RSS$plot.data.source,
         score.mat = {
           if (RSS$have.valid$scores)
             "score.mat"
           else
             "scan.mat"
         },
         ## the default value
         "scan.mat"
         )
}

rss.plot.data.source.bit.shift <- function() {

  ## for the current plot data source, returns a pair (x, y)
  ## where x is the number of bits to right-shift the data value to
  ## get the pixel value, and y is the number of bits to left-shift the
  ## class value to move it into the bits above the data bits in the pixel value.

  switch(rss.plot.data.source(),
         "scan.mat" = c(RSS$scan.info$bits.per.sample - RSS$pixel.data.bits, RSS$pixel.data.bits),
         "score.mat"  = c(RSS$score.bits - RSS$pixel.data.bits, RSS$pixel.data.bits)
         )
}

rss.dist.3d <- function(x1, y1, z1, x2, y2, z2) {
  ## For two sets of points S and T, return a matrix of dimensions
  ## length(S) x length(T) giving the euclidean distance between
  ## each pair (s, t) where s is in S and t is in T.
  ## The coordinates of points in S are given by x1, y1, z1
  ## The coordinates of points in T are given by x2, y2, z2
  dx <- outer(x1, x2, `-`)
  dy <- outer(y1, y2, `-`)
  dz <- outer(z1, z2, `-`)
  sqrt(dx^2 + dy^2 + dz^2)
}

rss.cos.angle.3d <- function(x1, y1, z1, x2, y2, z2) {
  ## For two sets of vectors S and T, return a matrix of dimensions
  ## length(S) x length(T) giving the cosine of the angle between
  ## each pair (s, t) where s is in S and t is in T.
  ## If one or both of the vectors in a pair is the zero vector,
  ## the matrix entry for that pair is NaN
  ## The coordinates of points in S are given by x1, y1, z1
  ## The coordinates of points in T are given by x2, y2, z2

  dp <- outer(x1, x2, `*`) + outer(y1, y2, `*`) + outer(z1, z2, `*`)
  m1 <- sqrt(x1^2 + y1^2 + z1^2)
  m2 <- sqrt(x2^2 + y2^2 + z2^2)
  return(dp / m1 / rep(m2, each = length(x1)))
}

## wrap the dynamic loader/unloader with one that automatically appends
## the correct platform extension and full path name if none is given
## an option argument in.dir.of specifies the name of a directory or a file
## in the same directory as the dynamic library.

rss.dyn.load <- function(x, local=FALSE, in.dir.of)
{
  full.file <- x
  if (basename(full.file) == full.file) {
    if (missing(in.dir.of))
      dir <- getwd()
    else
      dir <- dirname(in.dir.of)
    full.file <- file.path(dir, full.file)
  }
  if (substring(full.file, 1 + nchar(full.file) - nchar(.Platform$dynlib.ext)) != .Platform$dynlib.ext)
    full.file <- paste(full.file, .Platform$dynlib.ext, sep="")

  dyn.load(full.file, local=local)
  RSS$full.lib.path[[x]] <- full.file
}

rss.dyn.unload <- function(x)
{
  if (!is.null(RSS$full.lib.path[[x]]))
    try(dyn.unload(RSS$full.lib.path[[x]]), silent=TRUE)
  RSS$full.lib.path[[x]] <- NULL
}

rss.regexp.piece <- function(regexp, string) {
  ## extract the pieces from each element of string that
  ## match the regexp
  v <- regexpr(regexp, string, perl=TRUE)
  if (length(v) == 0)
    return ("")
  return(substring(string, v, v + attr(v, "match.length") - 1))
}


## a conversion stub for numbers to POSIXct so that we can
## use rbind to add POSIXct times to a dataframe

as.POSIXct.numeric <- function(x, tz="") structure(x, class="POSIXct")


rss.random.bytes <- function(n) {
  ## return a RAWSXP of n truly random bytes (i.e. generated using
  ## hardware, so that two calls of this function will (almost)
  ## never return the same value, regardless of the machine etc.
  .Call("random_bytes", n)
}

## read an entire file into a raw buffer

rss.read.file.as.raw <- function(f) readBin(f, "raw", file.info(f)$size)

## get the path to a foreign file

rss.get.foreign.path <- function(path, title, keep.basename=FALSE) {
  ##
  ## Find the location of a foreign file, prompting the user if we can't find it.
  ## Return the location, or NULL if the user cancelled.
  ##
  ## path: the full path, including name, to the file sought.
  ## title: a title for the "open file" dialog in case path is not found
  ## keep.basename: if TRUE, the basename (Y) of the file must be preserved, so if
  ##    the user directs us to a file (X) that has a different basename, we try to
  ##    make a copy of it in the same folder, and return the path to the copy.
  ##    This is intended for occasions when references to a shared library have been
  ##    compiled into the radR code, but an updated version of the library has a different name.
  ##    On linux, this does not matter as long as the library with changed name is loaded
  ##    before the library that refers to it, but on windows, the search for a symbol is apparently only
  ##    made in the specified library, and so library names matter.

  if (file.exists(path))
    return (path)

  ext <- path %~% "\\.[^.]+$"
  new.path <- rss.gui(FILE_DIALOG, "open.one", title,
                       types = structure(list(ext % % "files", "All files"), names=c(ext, ".*")),
                       init.file = path)
  if (length(new.path) < 1 || nchar(new.path)[1] == 0)
    return(NULL)

  if (keep.basename) {
    new.path.keep <- sub(basename(new.path), basename(path), new.path, fixed=TRUE)
    if (new.path.keep != new.path) {
      resp <- rss.gui(POPUP_DIALOG, title="Copy File?",
                msg="I need a file with the basename '" %:% basename(path) %:% "'.\nShould I copy\n'" %:% new.path %:% "'\nto\n'" %:% new.path.keep %:% "'?",
               buttons=c("Yes", "No"))
      if (resp == 2)
        return (NULL)

      if (file.exists(new.path.keep)) {
        resp <- rss.gui(POPUP_DIALOG, title="Overwrite File?",
                         msg="The file '" %:% new.path.keep %:% "'\n already exists.  Should I overwrite it?",
                         buttons=c("Yes", "No"))
        if (resp == 2)
          return (NULL)
      }
      file.copy (new.path, new.path.keep, overwrite=TRUE)
    }
    new.path <- new.path.keep
  }
  return (new.path)
}

rss.validate.parms <- function(valid.names, valid.vals, ...) {
  ## Determine whether the parameter assignments in "..." are permitted.
  ## ... is a sequence of NAME=VALUE pairs, or a single list with NAME=VALUE entries
  ## valid.names: a vector of the exact names which are permitted as "NAME"
  ## valid.vals:  a list with NAME=ALLOWED entries;  ALLOWED is either a vector
  ##              or an R expression in the variable "x"
  ## If "..." contains an element called ".ERRLAB", then errors are generated with its value prefixed to them.
  ##
  ## Returns: if .ERRLAB is missing, a list with only the valid NAME=VALUE pairs.  If a NAME is repeated,
  ##             it appears only once, with the last VALUE.
  ##          if .ERRLAB is given, and all assignments are valid, then the list of assignments;
  ##             otherwise, an error is generated for the first invalid assignment
  ##
  ## A parameter assignment NAME=VALUE is valid if:
  ##  - NAME is in valid.names
  ##  - and if NAME is in names(valid.vals), then:
  ##      - valid.vals[[NAME]] is a vector and VALUE is in valid.vals[[NAME]]
  ##    or
  ##      - valid.vals[[NAME]] is an expression, and eval(valid.vals[[NAME]], list(x=VALUE)) is TRUE
  ##
  ## For maximum flexibility, it is not required that valid.names == names(valid.vals).
  ## This means some parameters might not have validity tests beyond existence of the parameter name,
  ## and there can be validity tests for parameters which are not permitted.
  ## Note: parameter names are matched exactly, not partially.

  parms <- list(...)
  .ERRLAB <- parms[".ERRLAB"][[1]]
  parms[".ERRLAB"] <- NULL
  ## unpack the parms if a single list was provided
  if (length(parms) == 1 && is.list(parms[[1]]))
    parms <- parms[[1]]

  rv <- list()
  for (p in seq(along = parms)) {
    name <- names(parms)[p]
    if (name %in% valid.names) {
      value <- parms[[p]]
      valid <- valid.vals[name][[1]] ## avoid partial matching
      if (!is.null(valid)) {
        if (is.expression(valid)) {
          if (eval(valid, list(x=value))) {
            rv[[name]] <- value
          } else if (!is.null(.ERRLAB)) {
            stop (.ERRLAB %:% "value x=" %:% value %:% " for parameter '" %:% name %:% "' does not satisfy " %:% valid)
          }
        } else if (is.vector(valid)) {
          if (value %in% valid) {
            rv[[name]] <- value
          } else if (!is.null(.ERRLAB)) {
            stop (.ERRLAB %:% "value " %:% value %:% " for parameter '" %:% name %:% "' is not one of c(" %:% paste(valid, collapse=",") %:% ")")
          }
        }
      } else {
        rv[[name]] <- value
      }
    } else if (!is.null(.ERRLAB)) {
      stop (.ERRLAB %:% "'" %:% name %:% "' is not a valid parameter name")
    }
  }
  return(rv)
}

rss.free.patch.image <- function(pi) {
  .Call ("radR_free_patch_image", pi)
}

rss.alloc.patch.image <- function() {
  pi <- .Call ("radR_alloc_patch_image")
  reg.finalizer(pi, rss.free.patch.image)
  return(pi)
}

callback.to.fun <- function(s) {
  ## return the R function represented by the tcl callback in s
  ## i.e. s is what is returned by e.g. tcl("bind", ".a", "<Button-1>")
  s <- as.character(s)
   if (s[1] == "R_call")
    return (.Call("int_wrapped_pointer_to_sexp", as.integer(s[2])))
  return(NULL)
}

make.env <- function(...) {
  ## make an environment from a single untagged list argument
  ## or from a set of tagged arguments.
  ## The tags (or names of the list) become the symbol names in the environment.

  e <- new.env()
  v <- list(...)
  if (!length(names(v)))
      v <- v[[1]]
  for (i in seq(along=v))
    e[[names(v)[i]]] <- v[[i]]
  return(e)
}

rss.visible.only <- function(e) {
  ## return a tagged list containing all visible contents of an environment
  ## i.e. all values of symbols whose names do not start with "."
  n <- ls(e)
  structure(lapply(n, function(s) get(s, e)), names=n)
}

rss.source.tcl <- function(f) {
  ## read a file into the tcl interpreter
  ## doesn't catch exceptions!
  .Tcl(paste(readLines(f), collapse="\n"))
}

rss.tcl.stubs <- function() {
  ## define stubs to replace some Tcl/Tk functions to avoid having
  ## to always check for whether a GUI exists
  .Tcl <<- function(...) {NULL}
  tcl <<- TCL <<- .Tcl
}

### CONSTANTS
## the speed of light in a vacuum, in m/s; FIXME: get a sensible atmospheric value
rss.speed.of.light <- 2.99792458E8

rss.read.text <- function(f) readChar(f, file.info(f)$size)
rss.write.text <- function(s, f) writeChar(s, f, eos=NULL)

rss.create.plugin <- function (name) {
  ## create the template files for a new plugin
  ## by copying the "example" plugin with appropriate renaming

  opath = "plugins/" %:% name %:% "/"
  ipath = "plugins/" %:% "example/"

  dir.create(opath, showWarnings=FALSE)
  for (f in dir(ipath, full.names=FALSE)) {
    ## we're creating from an installation directory, so ignore the factory conf file
    if (f == "example.conf.factory.R")
      next
    t <- rss.read.text(ipath %:% f)
    t <- gsub("EXAMPLE", toupper(name), t, fixed=TRUE)
    t <- gsub("example", name, t, fixed=TRUE)
    t <- gsub("Example", toupper(substring(name, 1, 1)) %:% substring(name, 2), t, fixed=TRUE)
    t <- gsub("Copyright \\(C\\) 20[0-9][0-9]-20[0-9][0-9] John Brzustowski", "Copyright (C) " %:% format(Sys.time(), "%Y") %:% " YOUR NAME HERE", t, perl=TRUE)
    f <- gsub("example", name, f, fixed=TRUE)
    f <- gsub(".conf.update.R", ".conf.R", f, fixed=TRUE)
    rss.write.text(t, opath %:% f)
  }
}

rad <- function(x) x * (pi/180)
deg <- function(x) x * (180 / pi)

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

###.include "pulses.R"
