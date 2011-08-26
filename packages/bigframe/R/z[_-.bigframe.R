"[<-.bigframe" <-
function(x, i=1:dim(x)[1], j=1:length(attr(x, "header")$signature), value) {
  ## - set rows of the bigframe starting at "i" to data
  ## we allow permissive recycling of elements, coercing as appropriate

  if (length(i) == 1) {
    i <- seq(from=i, length=dim(value)[1])
  } else {
    if (!is.null(dim(value))){
      if (length(i) != dim(value)[1])
        stop("bigframe: length of row index of LHS is neither 1 nor the number of rows in RHS of assignment")
    } else if (!is.list(value)) {
      value <- list(value)
    }
  }
  if (length(i) > 0)
    .Call("bigframe_put_data", x, i, j, value)
  invisible(x)
}

