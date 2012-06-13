##
## Convert a set of folders with XIR3000ARCH .REC files into blipmovies, one per
## folder.  The folders must all be in the same folder, which is where blipmovies
## will be written.
##

save.pe <- GUI$plot.enabled
GUI$plot.enabled <- FALSE
dir.to <- rss.gui("FILE_DIALOG", mode="open.dir", title="Choose the folder whose sub-folders of .REC files will each be converted to blipmovies", init.file=".")
dirs.from <- dir(dir.to, full.names=TRUE)
dirs.from <- dirs.from[file.info(dirs.from)$isdir]

if (!exists("XIR3000ARCH"))
  rss.load.plugin("xir3000arch")

if (!exists("BLIPMOVIE"))
  rss.load.plugin("blipmovie")

CONVREC <<- strictenv(dirs.from = dirs.from, dir.to=dir.to, save.pe=save.pe, dlg=NULL)
rm(dirs.from, dir.to, save.pe)

convert.one.REC.folder <- function(i) {
  ## convert one REC folder
  ## the folder is stored in .GlobalEnv$CONVREC$dirs.from[i]
  ## the output blipmovie folder is stored in .GlobalEnv$CONVREC$dir.to

  if (!is.null(CONVREC$dlg))
    rss.gui(DELETE_MESSAGEBOX, CONVREC$dlg)
  
  if (i > length(CONVREC$dirs.from)) {
    GUI$plot.enabled <- CONVREC$save.pe
    rss.gui("POPUP_MESSAGEBOX", "Finished converting",
            sprintf ("I converted these %d directories to blipmovies:\n%s", i-1, paste(CONVREC$dirs.from, collapse="\n")))
    rm(CONVREC, convert.one.REC.folder)
    return()
  }

  d <- CONVREC$dirs.from[i]
  first.file <- dir(d, pattern=".*\\.[rR][eE][cC]$", full.names=TRUE)[1]
  rss.set.port(XIR3000ARCH$get.ports()[[1]], filename=first.file)
  
  file.to <- file.path(CONVREC$dir.to, paste(basename(d), sub("\\.[rR][eE][cC]$", ".bm", basename(first.file)), sep="_"));

  rss.set.port(BLIPMOVIE$get.ports()[[2]], filename=file.to)

  CONVREC$dlg <<- rss.gui("POPUP_MESSAGEBOX", "Converting REC files",
          sprintf("I am converting the files in folder %s to blipmovie %s\nThis may take some time...",
                  d, file.to));

  RSS$recording<-TRUE

  rss.add.hook("ONPAUSE", function() {
    rss.do.stop()
    rss.gui("SET_NO_PORT", "sink")
    rss.gui("SET_NO_PORT", "source")
    RSS$recording <- FALSE
    rss.defer.call(convert.one.REC.folder(i+1))
  })
    
  rss.gui("START_PLAY")
}

rss.defer.call(convert.one.REC.folder(1))
