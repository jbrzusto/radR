## fix_big_bad_blipmovie.R

## Some users have encountered blipmovies of size ~45 GB and up which
## are unreadable.  Investigation shows the TOC entry at biglist slot
## [[1]] is NULL, and moreover, a large tail (~ 1GB) of the .bm file is
## all zeroes.  I've seen these pre-allocated all-zero files in .REC
## archives recorded by Russell Technologies' WinHorizon software, so
## it appears to be a windows-related issue.

## This script will examine a blipmovie (biglist, really) and drop trailing all-zero
## entries by modifying only the index bigframe.

## This file must be run from within a radR session, typically from
## the radR menu's "Source an R script..." entry


fix_big_bad_blipmovie = function() {
  f = rss.gui("FILE_DIALOG",
    mode = "open.file",
    title = "Choose the blipmovie to fix",
    init.file = big_bad_blipmovie_file
    )

  if (length(f) == 0)
    return (NULL)

  big_bad_blipmovie_file <<- f

  if (!file.exists(f))
    return (NULL)

  # strip trailing ".i" if user selected index file
  if (length(grep("\\.bm.i$", f)) == 1)
    f = substr(f, 1, nchar(f)-2)
  

  bl <- NULL
  try ({bl <- biglist(f, read.only=TRUE)}, silent=TRUE)
  
  if (is.null(bl)) {
    rss.gui("DELETE_MESSAGEBOX", id)
    id = rss.gui("POPUP_DIALOG", "Not a Blipmovie", "Either the file you selected is not a blipmovie,\nor it is damaged enough that I don't know how to repair it.\nSorry about that.", buttons="Ok")
    return(TRUE)
  }
  
  if (!is.null(bl[[1]])) {
    rss.gui("DELETE_MESSAGEBOX", id)
    id = rss.gui("POPUP_DIALOG", "Blipmovie has a TOC", "There seems to be a table of contents for this blipmovie,\nso I probably don't know how to fix its issues.\nSorry about that.", buttons="Ok")
    return(TRUE)
  }
  id = rss.gui("POPUP_MESSAGEBOX", "Examining blipmovie...", sprintf("This blipmovie is missing its table of contents:\n%s\n", basename(f)))


  ## is entry i of the biglist valid?
  valid = function(i) {
    rv = FALSE
    try ({x = bl[[i]]; rv = TRUE}, silent=TRUE)
    return (rv)
  }

  ## functions for getting text from and appending to the message box (KLUDGE!)
  label = sprintf(".%s.label", id)
  tcl(label, "configure", "-justify", "left")

  pg = function() {
    tclvalue(tcl(label, "cget", "-text"))
  }
  pp = function(s) {
    tcl(label, "configure", "-text", paste(pg(), s, sep=""))
    tcl("update")
  }

  ## find the first invalid entry in the biglist; invalid entries
  ## comprise a tail of the list
  
  n = length(bl)

  if (valid(n)) {
    pp("\n\nOops - this blipmovie has problems I don't know how to fix.\nSorry about that.\n")
    return (TRUE)
  }

  pp("\nThis blipmovie seems to have the 'zero-tail' problem.\nPlease wait while I find the size of the zero tail")
  i1 = 1
  i2 = n
  while(valid(i1)) {
    m = floor((i1 + i2) / 2)
    if (valid(m)) {
      i1 = m + 1
    } else {
      i2 = m
    }
  }

  rss.gui("DELETE_MESSAGEBOX", id)
  
  filesize = file.info(f)$size
  badbytes = filesize - attr(bl,"ndx")[i1,1] + 1
  choice = rss.gui("POPUP_DIALOG", sprintf("Should I fix blipmovie'%s'?", basename(f)), sprintf("The last %d out of %d scans of this blipmovie are invalid.\nThis corresponds to the last %.0f bytes of this file; ie %.1f%%\nShould I chop the invalid tail from this blipmovie?\nIf I do this, the tail will be destroyed, so you should backup the file first.  Doing this should allow the blipmovie's table of contents to be recreated\nwhen you next open the blipmovie in radR", ceiling((n - i1 + 1) / 4), ceiling((n - 2) / 4), badbytes, badbytes / filesize * 100), buttons = c("Yes - fix", "No - do NOT fix"), default=2)
  if (choice == 2)
    return(TRUE)

  close(bl)
  bl <- NULL
  try ({bl <- biglist(f, read.only=FALSE)}, silent=TRUE)
  
  if (is.null(bl)) {
    rss.gui("DELETE_MESSAGEBOX", id)
    id = rss.gui("POPUP_DIALOG", "You don't have write permission", "It appears that you don't have write permission for this blipmovie.\nSorry about that.", buttons="Ok")
    return(TRUE)
  }

  dim(attr(bl, "ndx")) <- c(i1 - 1, 2)
  close(bl)
  id = rss.gui("POPUP_DIALOG", "Tail truncated", sprintf("I chopped off the zero tail of the blipmovie\n'%s'\n\nOpening it in radR should automatically rebuild the table of contents.\n", f), buttons="Ok")
  
}

if (! exists("big_bad_blipmovie_file"))
  big_bad_blipmovie_file <<- "."

repeat {
  f = fix_big_bad_blipmovie()
  if (is.null(f))
    break
}
