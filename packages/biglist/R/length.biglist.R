"length.biglist" <-
function(x) {
  ## return the number of items in the biglist
  .Call("biglist_length", x, PACKAGE="biglist")
}

