"close.biglist" <-
function(con, ...) {
  .Call("biglist_close", con, PACKAGE="biglist")
  close(attr(con, "ndx"))
  invisible(con)
}
