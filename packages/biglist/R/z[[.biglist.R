"[[.biglist" <-
function(x, i) {
  ## - returns the ith item from the biglist x (i >= 1)
  ##   if there are fewer than i items in the biglist, return NULL
  ## - if the index bigframe has a header with a names item, it is
  ##   used as the names for the items

  .Call("biglist_get", x, i, PACKAGE="biglist")

  ## ==================== in pure R: ====================
  ##
  ##   if (i > dim(x$ndx)[1])
  ##     return(NULL)
  ##   offs <-  x$ndx[i]$offs[1]
  ##   if (is.na(offs))
  ##     return(NULL)
  ##   seek(x$file, offs, rw="read")
  ##   ## BUGGY: load(file=file)
  ##   .Internal(loadFromConn2(x$file, environment()))
  ##   ## if the object has no names, restore them from the header
  ##   if (is.null(names(value)))
  ##     return(structure(value, names=x$ndx$header$names))
  ##   else
  ##     return(value)
}

