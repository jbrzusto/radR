"[[<-.biglist" <-
function(x, i, value) {
  ## - set item i of the biglist to data
  ## FIXME: this always writes data at the end of the data file,
  ## then points the index bigframe to it, leaving holes

  .Call("biglist_put", x, i, value, PACKAGE="biglist")

  ## ==================== in pure R: ====================
  ##
  ##   seek(x$file, 0, origin="end", rw="write")
  ##   offs <- seek(x$file, rw="write")
  ##   ## don't save names if they match those from the header
  ##   value <- value
  ##   if(identical(names(value), x$ndx$header$names))
  ##     names(value) <- NULL
  ##   save(list="value", file=x$file, compress=FALSE)
  ##   x$ndx[i] <- data.frame(offs=offs, size=seek(x$file, rw="write") - offs)
  ##   invisible(x)
}

