"$<-.bigframe" <-
function(x, a, value) {
  ## return a column from a dataframe
  m <- match(substitute(a), names(x))
  if (!is.na(m))
    x[,m]<-value
  x
}

