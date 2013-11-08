blip.centred.at = function(x, y) {
  coord = rss.patch.at.sample.pulse(rss.xy.to.sp(x, y))
  if (! is.null(coord))
    return (coord)
  ## okay, slow case - find the row in RSS$patches whose centroid
  ## most closely matches 
  i = which.min((x - RSS$patches$x[])^2 + (y - RSS$patches$y[])^2)
  return (structure(rss.get.all.blips(which.patches = i, blip.nums = FALSE), index=i))
}
  
processTrackBlips = function(fb, ft, blipfun) {
  ## remove interference from event loop (linux)
  rss.remove.event.loop()
  
  ## not sure why the following line is needed
  num.sources <<- 1
  
  cat("Reading tracks file at ", ft, "\n")
  
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

      ## not sure why the following is needed, but it is
	 RSS$have.valid$patches = TRUE
      
      if (length(RSS$blips) == 0)
        return()
      cat(format(structure(tracks$timestamp[i[1]], class="POSIXct"),"Doing scan @ %Y-%m-%d %H:%M:%OS3\r"))
      
      ## process each required blip from this scan

      for (j in i) {
        coord = blip.centred.at(tracks$x[j], tracks$y[j])
        if (is.null(coord)) {
           print("Couldn't find patch in scan ", tracks$scan.no[j], " for track ", tracks$track.no[j])
        } else {
          blipfun(coord, RSS$scan.mat[coord], tracks$timestamp[j], tracks$track.no[j])
        }
      }
    }

  ## now apply the doScan function to each scan where track blips were found
  invisible(tapply(1:nrow(tracks), tracks$scan.no, doScan))

  shut.down(bm)

  RSS$source = NULL
  
  ## restore event loop (linux)
  rss.install.event.loop()
}
