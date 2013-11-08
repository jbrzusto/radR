## given a blipmovie and a file of tracks,
## apply a function to every blip in every track

## First, a function to lookup blip given the x, y coordinates
## of its centroid.  There is a possibility of ambiguity
## here, as two nested blips might have the same centroid, but this
## should be extremely rare, and in any case the outer such blip, at least,
## is clearly not a point target.

## There are generally two cases:
##  - the centroid is actually inside the blip (usually the case for point-sources, unless
##    two nearby point-sources have been incorrectly joined into a single blip)
##    In this case, use the faster rss.patch.at.sample.pulse() method
##  - the centorid is not inside the blip (curvy shape); in this case, search the
##    RSS$patches table directly

source("utils/processTrackBlips.R")

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

if (fb.guess == "") {
  fb.guess = trackfile
}

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
#### adding these parameters into blipfun: theta ,phi ,dbm, max,etc.
#### I don't know how you obtain sp,v, t, tn. Where are they specified?

#### JMB: the rest of the code in this file has the job of working out what
#### the sp, v, t, and tn are for each blip in each track, and calls (your) blipfun
#### with these values, once per blip per track.  The job of your blipfun
#### is to do something with these values.

####   try (
####     write.table((sp)[,1:2], file=MYFILE, sep=",")
####       )
        
#### This only extracts information from the very last blip in the tracks.csv
#### not sure if I should run a "for loop" looping through each pixel row 
#### within the blipfun, to obtain all information from every pixel of every blip

#### JMB: write.csv writes its first arg to a file, but does not append.
#### it's intended for writing a large dataframe to a file all at once, not incrementally.
#### To write incrementally, you need to open the file outside of this function,
#### then use e.g. cat(stuff, file=MYFILE)
#### then close the file when finished
#### e.g. this writes the timestamp, track number, number of samples (which is the number of rows
#### in the matrix sp) and the blip area

    cat(sprintf("%.3f,%d,%d,%.1f\n", t, tn, nrow(sp), RSS$patches$area[attr(sp, "index")[1]]), file=MYFILE)
  }
}

#### JMB: open the output file
MYFILE = file(subst(".csv$", "_blips.csv", trackfile), "w")
#### JMB: write the header line
cat ("timestamp,trackNo,numPixels,area\n", file=MYFILE)

processTrackBlips(fb, ft, blipfun)

#### JMB: close the output file
close(MYFILE)
