"[.biglist" <-
function(x, i) {
  ## return a list of items from the biglist x
  ## each element of i is between 1 and length(x)
  rv <- vector("list", length(i))
  for (ii in seq(along=i))
    rv[ii] <- list(x[[i[ii]]])
  return(rv)
}

