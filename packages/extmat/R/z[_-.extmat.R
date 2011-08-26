"[<-.extmat" <-
function(x, i, j, NOTRIGGER=FALSE, value) {
  ## assign to a submatrix of x
  ## the index i has the same conventions as for "[.extmat"
  k <- 1
  nargs <- nargs()
  if (NOTRIGGER)
    nargs <- nargs - 1
  if (!missing(i) && is.matrix(i)) {
    k <- dim(i)[2]
  } else if(nargs == 3 && !missing(i)) {
    k <- 0
  } else {
    if (missing(i))
      i <- seq(length=dim(x)[1])
    if (missing(j))
      j <- seq(length=dim(x)[2])
  }

  if (k == 0)
    return(.Call(extmat_index_assign, x, i, NULL, value, 2L, as.logical(NOTRIGGER)))
  if (k == 1)
    return(.Call(extmat_index_assign, x, i, j, value, 0L, as.logical(NOTRIGGER)))
  else if (k >= 2)
    return(.Call(extmat_index_assign, x, i[,k-1], i[,k], value, 1L, as.logical(NOTRIGGER)))
  else
    return()
}

