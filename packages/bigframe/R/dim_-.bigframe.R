"dim<-.bigframe" <-
function(x, value) {

  ## truncate (or expand) the bigframe to have n.rows rows
  if (length(value) > 1 && value[2] != length(attr(x, "header")$signature))
    stop("the number of columns in a bigframe cannot be changed")
  if (value[1] < 0)
    stop("the number of rows in a bigframe must be at least zero")
  .Call("bigframe_set_num_rows", x, as.integer(value[1]))
  invisible(x)
}

