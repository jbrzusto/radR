"[.extmat" <-
function(x, i, j, drop=TRUE, raw=FALSE) {
  
  ## if i is an n x 2 matrix the result
  ## is a vector with the same length as i[,1] consisting
  ## of the elements x[i[1,1], i[1,2]], x[i[2,1], i[2,2]], ...
  ## if i is an n x k matrix with k > 2, the result is a
  ## matrix with the first (k-2) columns of i cbind()ed
  ## to the vector of elements x[i[1,k-1], i[1,k]], x[i[2,k-1], i[2,k]], ...
  ## Otherwise, the result is a matrix with length(i) rows
  ## and length(j) columns consisting of the elements
  ## x[i[1], j[1]], x[i[2], j[1]], ..., x[i[1], j[2]], ...
  ## if i is an n x 0 matrix, the result is an n x 0 matrix
  
  k <- 1
  if (!missing(i) && is.matrix(i)) {
    k <- dim(i)[2]
  } else if (nargs() == 2 && ! missing(i)) {
    k <- 0
  } else {
    if (missing(i))
      i <- seq(length=dim(x)[1])
    if (missing(j))
      j <- seq(length=dim(x)[2])
  }
  
  if (k == 0) {
    .Call(extmat_index, x, i, NULL, 2L, as.logical(drop), as.logical(raw))
  } else if (k == 1) {
    .Call(extmat_index, x, i, j, 0L, as.logical(drop), as.logical(raw))
  } else if (k == 2) {
    .Call(extmat_index, x, i[,1], i[,2], 1L, as.logical(drop), as.logical(raw))
  } else {
    cbind(i[,1:(k-2)], .Call(extmat_index, x, i[,k-1], i[,k], 1L, as.logical(drop), as.logical(raw)))
  }
}

