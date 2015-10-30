"[[.biglist" <-
function(x, i) {
  ## - returns the ith item from the biglist x (i >= 1)
  ##   if there are fewer than i items in the biglist, return NULL
  ## - if the index bigframe has a header with a names item, it is
  ##   used as the names for the items

  if (i < 0 || i > x$length)
    return (NULL)
  
  offs <-  c(x$ndx[i, 1])
  if (is.na(offs))
    return(NULL)
  
  seek(x$file, offs, rw="read")
  if (x$numver == 1) {
    ## deal with crufty legacy where nul-containing strings were allowed
    ## there was only one case where this applied: a single-element character
    ## vector, so we check for that
    check = readBin(x$file, raw(), n=30)
    if (identical(check[1:26], biglist.legacy.header)) {
      len = readBin(check[27:30], integer(), endian="big")
      return (readBin(x$file, raw(), n=len))
    }
    ## go back to the start of the object
    seek(x$file, offs, rw="read")
  }
  return(unserialize(x$file))
}

