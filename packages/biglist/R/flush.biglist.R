"flush.biglist" <-
function(con) {
  flush(con$file)
  flush(con$ndx) ## flush the bigframe index file 
  invisible(con)
}

