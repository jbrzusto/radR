## given a blipmovie and a file of tracks,
## apply a function to every blip in every track

process_track_blips = function(fb, ft, blipfun) {
  ## remove interference from event loop (linux)
  rss.remove.event.loop()
  
  ## not sure why the following line is needed
  num.sources <<- 1
  
  tracks = read.csv(ft, as.is=TRUE)
  ## sort tracks by scan, so we don't process any scan
  ## more than once.
  tracks = tracks[order(tracks$scan.no, tracks$blip.no),]

  ## open the blipmovie by getting a "port" object
  ## from the blipmovie plugin
  
  bm = BLIPMOVIE$get.ports()[[1]]

  ## configure its filename
  
  config(bm, filename = fb)

  ## start it up.
  start.up(bm)

  ## pretend this is the source; some code expect it
  RSS$source = bm

  ## get the table of contents
  toc = get.contents(bm)

  ## process the first scan to get scan info 
  seek.scan(bm, 1, 1)
  rss.get.scan()

  ## scan.no is only well-defined for files with a single segment (run)
  
  if (nrow(toc) > 1)
    warning("Table of contents shows more than one segment in file ", fb, ".\nOnly using first one.\n")


  ## a function to process all blips in one scan
  ## i are the indexes of tracks.csv from the same scan
  
  doScan = function(i) {
      ## i is the set of rows in tracks corresponding to a single
      ## scan.no
       
      ## seek to the scan and process it (this loads blips
      ## and applies curent filtering criteria; these
      ## must match those in effect when the tracker
      ## was run, otherwise the wrong blips will be selected.

      seek.scan(bm, 1, tracks$scan.no[i[1]])
      rss.get.scan()
      rss.process.patches()

      if (length(RSS$blips) == 0)
        return()
      
      ## process each required blip from this scan

      for (j in i) {
        coord = rss.patch.at.sample.pulse(rss.xy.to.sp(tracks$x[j], tracks$y[j]))
        blipfun(coord, RSS$scan.mat[coord], tracks$timestamp[j], tracks$track.no[j])
      }
    }

  ## now apply the doScan function to each scan where track blips were found
  invisible(tapply(1:nrow(tracks), tracks$scan.no, doScan))

  shut.down(bm)

  RSS$source = NULL
  
  ## restore event loop (linux)
  rss.install.event.loop()
}

if (!exists("trackfile"))
  trackfile <<- ""

ft = rss.gui("FILE_DIALOG",
  mode = "open.file",
  title = "Choose the tracks.csv file",
  types = list(".csv" = "CSV file", ".*" = "All files"),
  init.file = trackfile
  )

if (length(ft) == 0)
  return (NULL)

trackfile <<- ft

if (!file.exists(ft))
    return (NULL)

## if the user called their track files BLIPMOVIE_NAME_tracks.csv,
## then we know the blipmovie filename; regardless, use this
## as a guess

if (!exists("blipfile"))
  blipfile <<- ""

fb.guess = sub("_tracks", "", trackfile)
fb.guess = sub(".csv$", ".bm", fb.guess)

if (!file.exists(fb.guess))
  fb.guess = blipfile

fb = rss.gui("FILE_DIALOG",
  mode = "open.file",
  title = "Choose the blipmovie file",
  types = list(".bm" = "radR blipmovie", ".*" = "All files"),
  init.file = fb.guess
  )

if (length(fb) == 0)
  return (NULL)

## strip any trailing ".i" (if user selected index file)
  
fb = sub("\\.i$", "", fb)
blipfile <<- fb

## apply a function called blipfun to each blip
## this file defines blipfun as

if (! exists("blipfun")) {
  blipfun = function(sp, v, t, tn) {
    ## plot 2-d shape of blip in space
    try (
      plot(gui.tx.matrix.to.spatial(sp)[,1:2],
           xlab="x",
           ylab="y",
           main=sprintf("At %s (track %d) blip has %d pixels\n", strftime(as.POSIXlt(structure(t,class="POSIXct")), "%Y-%m-%d %H:%M:%OS3"), tn, nrow(sp))
           )
      )
  }
}

process_track_blips(fb, ft, blipfun)
