"close.biglist" <-
function(con, ...) {
  close(con$file)
  close(con$ndx)
  invisible(con)
}
