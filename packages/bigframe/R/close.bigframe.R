"close.bigframe" <-
function(con, ...) {
  ## flush the bigframe and close the file
  ## This is meant to be called by the user when
  ## an immediate close of the backing files is
  ## required.

  .Call("bigframe_close", con)
  invisible(con)
}

