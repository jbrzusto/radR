"dim.bigframe" <-
function(x) {
  ## return the number of rows in the bigframe
  c(.Call("bigframe_get_num_rows", x), length(attr(x, "header")$signature))
}

