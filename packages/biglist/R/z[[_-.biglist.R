"[[<-.biglist" <-
function(x, i, value) {
  ## - set item i of the biglist to data
  seek(x$file, 0, origin="end", rw="write")
  offs <- seek(x$file, rw="write")
    ## don't save names if they match those from the header
  value <- value
  if(identical(names(value), x$ndx$header$names))
    names(value) <- NULL
  serialize(value, x$file, ascii=FALSE, xdr=FALSE)
  x$ndx[i] <- data.frame(offs=offs, size=seek(x$file, rw="write") - offs)
  if (i > x$length)
    x$length = i
  invisible(x)
}

