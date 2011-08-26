"names.biglist" <-
function(x) {
  ## return the names attribute saved in the
  ## index file header
  ## It is not possible to change this attribute
  ## after the biglist is created.
  ## Note that this is not the names of the
  ## items in the biglist, but rather, the names of
  ## the subitems in each item of the biglist.
  attr(attr(x, "ndx"), "header")$names
}

