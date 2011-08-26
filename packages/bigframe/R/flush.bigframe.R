"flush.bigframe" <-
function(con) {
  ## flush unwritten data to disk
  .Call("bigframe_flush", con)
  invisible(con)
}

