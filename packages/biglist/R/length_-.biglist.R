"length<-.biglist" <-
function(x, value) {
  ## set the length of the biglist to value
  ## if value >= length(x), this has no effect
  ## because attempts to get elements with
  ## larger indexes already return NULL.
  ## if value < length(x), this backs up and
  ## truncates the data file pointer as
  ## far as possible (i.e. before any
  ## contiguous deleted tail)
  n <- value
  if (n < length(x)) {
    flush(x)
    drop.items(x, (n+1):length(x))
  }
  invisible(x)
}

