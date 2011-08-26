## svn: $Id: copy_params_from_old_install.R 551 2010-04-16 06:07:49Z john $

## Copy parameter values and extra files (antennas, palettes, zones)
## from a previous installation of R to the current one.

## Some parameters should not be copied from sufficiently old installations,
## because they have changed in structure, for example.  This list
## indicates the minimum revision required for certain parameters to
## be copied. A value of Inf means never copy the parameter.
## Parameters not specified here are always copied, unless
## the user deselects their group.

rss.min.parm.revision <-
  list (
        "gui/gui" = list (
          default.scripts.folder = 543,           ## folder renamed from "util" to "scripts"
          class.palette.gamma.increment = 405     ## new "excluded" class added
          ),
        "main/radR" = list (
          CLASS.VAL              = 405,
          class.gamma            = 405,
          class.named            = 405,
          class.palette          = 405,
          show.class             = 405,
          splash.text            = Inf
          ),
        "plugins/antenna/antenna" = list (
          parm.names             = 488,
          default.parms          = 488,
          override               = 488
          )
        )


rss.get.installation.parms <- function(path) {
  ## get a list of parameters and extra files from a previous installation
  ## rooted at "path" which are still applicable to the current installation.
  ##
  ## The return value is a list with these items:
  ##
  ## modules: the names of modules for which parameters exist in the
  ##    previous installation and which are relevant in the current
  ##    one.  This is a path to the module's base config file, relative
  ##    to the radR installation root.
  ##
  ## old: for each module, a list of parameter values relevant in the
  ##    current installation
  ##
  ## curr: for each module, a list of the parameter values in the
  ##    current installation
  ##
  ## extra: paths to all antenna, palette and zone files in the
  ##    previous installation
  ##
  ## inst: path to the old installation
  ##
  ## rev: radR revision number of the old installation

  ## get the revision of the old installation

  version.file <- file.path(path, "VERSION.TXT")
  if (file.exists(version.file)) {
    rev <- as.numeric(strsplit(readLines(version.file, 1), " ")[[1]][4])
  } else {
    rev <- 404 ## latest revision without a VERSION.TXT file
  }
  
  ## regular expression suffix for radR config files
  conf.suffix.regex <- "(\\.windows|\\.unix|)\\.conf\\.R$"

  ## regular expression suffix for radR extra files
  extra.suffix.regex <- "(\\.antenna|\\.palette|\\.zone)\\.R$"
  
  get.names <- function(path, suffix=conf.suffix.regex) unique(sub(suffix, "", dir(path, pattern=suffix, recursive=TRUE)))

  ## basenames of all config files in current installation
  curr.conf.names <- get.names(".")
  
  ## basenames of all config files in old installation
  old.conf.names  <- get.names(path)

  ## retain only those names common to both
  conf.names <- intersect(curr.conf.names, old.conf.names)

  ## get the extra files we might want to copy over
  extra.files <- dir(path, pattern=extra.suffix.regex, recursive=TRUE)

  ## move gui.bindings from conf.names to extra fiels
  conf.names <- conf.names[conf.names != "gui/gui.bindings"]
  extra.files <- c("gui/gui.bindings.conf.R", extra.files)
  
  ## read current and old parm values
  current.parms <- lapply(file.path(".",  conf.names), rss.load.config)
  old.parms     <- lapply(file.path(path, conf.names), rss.load.config)

  filter.bad <- function(curr, old, module) {
    ## filter out names from old that are not in curr, whose class is different,
    ## or whose minimum revision for copying is later than that of the old installation
    for (n in names(old))
      if (! exists(n, curr) || ! identical(class(curr[[n]]), class(old[[n]])) ||
          max(rss.min.parm.revision[[module]][[n]], rev) > rev) ## uses the fact that max(NULL, a) == a
        rm(list=n, envir=old)
    return(old)
  }

  ## filter out inapplicable parms from old set
  
  for (i in seq(along = old.parms))
    old.parms[[i]] <- filter.bad(current.parms[[i]], old.parms[[i]], conf.names[[i]])

  return (list(modules=conf.names, old=old.parms, curr=current.parms, extra=extra.files, inst=path, rev=rev))
}

rss.set.installation.parms <- function(plist) {
  ## copy parms and extra files from an old installation to the current installation
  ## plist is a list as returned by rss.get.installation.parms, with these items:
  ##
  ## modules: the names of modules for which parameters exist in the
  ##    previous installation and which are relevant in the current
  ##    one.  This is a path to the module's base config file, relative
  ##    to the radR installation root.
  ##
  ## old: for each module, an environment of parameters relevant in the
  ##    current installation; these will override values in the current installation
  ##
  ## curr: for each module, a list of the parameter values in the
  ##    current installation; these can be different from values already in config
  ##    files of the current installation; any value here for which a value is also
  ##    provided in the corresponding slot of "old" will be overridden.  Values here
  ##    will override those in the config files.
  ##
  ## extra: paths to all antenna, palette and zone files in the
  ##    previous installation; all of these will be copied to the current installation;
  ##    existing files with the same name will be renamed by adding the suffix ".saved",
  ##    if such a ".saved" file does not already exist.
  ##
  ## inst: path to the old installation, from which files in "extra" are copied 
  ##
  ## rev: radR revision number of the old installation
  ##
  ## The caller can have modified plist (after obtaining it from rss.get.installation.parms) in various ways:
  ##  - remove modules for which parameters are not to be obtained from the old installation; this means
  ##    removing the module name from the "modules" item, and removing the corresponding items of "old" and
  ##    "curr".
  ##  - remove parameters from an item in "old", so that the corresponding value in the current installation is preserved
  ##  - remove names from "extra", so that certain files are not copied from the old installation
  ##

  for (i in seq(along = plist$modules)) {
    ## copy parameter values from old installation into current one
    for (n in names(plist$old[[i]]))
      plist$curr[[i]][[n]] <- plist$old[[i]][[n]]
    ## write curr parameter values back to files
    rss.save.config (plist$curr[[i]], plist$modules[[i]])
  }

  for (f in plist$extra) {
    ## if necessary, create a ".saved" version of the file in the current installation
    if (file.exists(f) && ! file.exists(save.file <- paste(f, ".saved", sep="")))
      file.rename(f, save.file)
    ## copy the file from the old installation
    file.copy(file.path(plist$inst, f), f, overwrite=TRUE)
  }
}

rss.at.end.do <- function() {
  ## clean up after ourselves

  rm (list = c("rss.get.installation.parms", "rss.set.installation.parms", "rss.at.end.do", "rss.min.parm.revision", "copy.doit",
        "COPY_PARM_SETTER", "COPY_FILE_SETTER"),
      envir=.GlobalEnv)
}
  

copy.doit <- function() {
  menu.id <- ""  ## need this binding for closing the menu later
  
  ## The GUI part of the code to prompt the user through the process

  ok <- rss.gui("POPUP_DIALOG", "Copy parameters",
                "This script will copy parameter values from a previous installation of radR to the current installation.  It will also copy antenna, palette, and zone files.\n\nChanges you have made to plugin parameters etc. in the current session but have not yet saved will NOT be reflected in the updated configuration files.  However, you may save such changes manually from the plugin menu before quitting this radR session.\n\nYou will be given the chance to review and modify the list of plugins and files being copied.\n\nDo you wish to proceed?",
                buttons = c("Yes", "No"))

  if (ok == 1) {
    d <- rss.gui("FILE_DIALOG", mode="open.dir", title="Choose the folder of the old radR installation", init.file=".")
    if (length(d) > 0 && d[1] != "") {
      id <- rss.gui("POPUP_MESSAGEBOX", "Scanning old installation...", "Please wait while I scan for parameters and extra files from\n" %:% d)
      x <- rss.get.installation.parms(d)

      ## clean up confusing names for modules
      m <- x$modules
      confusing <- basename(dirname(m)) == basename(m)
      m[confusing] <- dirname(m)[confusing]
      m[m == "main/radR"] <- "main, including blip finding / filtering parameters"
      m[m == "gui"] <- "general GUI options"
        
      use <- structure(rep(TRUE, length(m)), names = m)     
      usex <- structure(rep(TRUE, length(x$extra)), names=c("gui keyboard and mouse bindings", x$extra[-1]))
      
      mlist <- list("Copy parameters and files now..." = function() {
        x$modules <- x$modules[use]
        x$old <- x$old[use]
        x$curr <- x$curr[use]
        x$extra <- x$extra[usex]
        id <- rss.gui("POPUP_MESSAGEBOX", "Copying old parameters...", "Please wait while I copy parameters and files from\n" %:% x$inst)
        
        rss.set.installation.parms(x)
        rss.gui("DELETE_MESSAGEBOX", id)
        rss.gui(DESTROY_MENU, menu.id)
        resp <- rss.gui("POPUP_DIALOG", "Finished copying", "The old parameters and files have been copied to this radR installation.  To use all of them, you should quit radR (without saving configuration) and then restart.\nIf you wish to use copied parameters only for a particular plugin, you can simply reload the plugin now.\n\nShould I quit radR now?", buttons=c("Yes - quit radR", "No"))
        if (resp == 1)
          q(force=TRUE)
        rss.at.end.do()
      },
                    "Cancel - do not copy anything..." = function() {
                      rss.at.end.do()
                      rss.gui(DESTROY_MENU, menu.id)
                    },
                    "Parameters from these groups:",
                    "   select all" = function() {use[] <<- TRUE; for(i in seq(along=use)) COPY_PARM_SETTER(i, TRUE)},
                    "   select none" = function() {use[] <<- FALSE; for(i in seq(along=use)) COPY_PARM_SETTER(i, FALSE)},
                    c(list("choose.any",
                           on.set = function(i, v) use[i] <<- v,
                           set.or.get = "COPY_PARM_SETTER"),
                      use
                      ),
                    "These extra files:",
                    "   select all" = function() {usex[] <<- TRUE; for(i in seq(along=use)) COPY_FILE_SETTER(i, TRUE)},
                    "   select none" = function() {usex[] <<- FALSE; for(i in seq(along=use)) COPY_FILE_SETTER(i, FALSE)},
                    c(list("choose.any",
                           on.set = function(i, v) usex[i] <<- v,
                           set.or.get = "COPY_FILE_SETTER"),
                      usex
                      )
                    )
      rss.gui("DELETE_MESSAGEBOX", id)
      menu.id <- rss.gui(POPUP_NEW_MENU, "Copy from old installation", mlist)
    }
  } else {
    rss.at.end.do()
  }
}
                         
copy.doit() 

  

  
        
