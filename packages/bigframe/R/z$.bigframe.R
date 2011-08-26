"$.bigframe" <-
function(x, a) {
  ## return a column from a dataframe
  m <- match(substitute(a), names(x))
  if (is.na(m))
    NULL
  else
    unclass(x[,m][[1]])
}

