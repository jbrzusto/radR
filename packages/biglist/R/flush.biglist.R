"flush.biglist" <-
function(con) {
  ## flush unwritten data to disk
  .Call("biglist_flush", con, PACKAGE="biglist") ## flush the cache to the file connection
  flush(attr(con, "ndx")) ## flush the bigframe index file 
  invisible(con)
}

