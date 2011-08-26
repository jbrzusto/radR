"drop.items" <-
function(x, i) {
  ## set the items with indexes in i to NULL and
  ## free storage in the data file, if possible
  ## which: integer vector of indexes of list items to be
  ## dropped
  
  .Call("biglist_drop", x, i, PACKAGE="biglist")
  
  ## ==================== in pure R: ====================
  ##
  ##   if (length(i) == 0)
  ##     return()
  ##   seek(x$file, 0, "end", rw="write")
  ##   last.pos <- seek(x$file, rw="write")
  ##   item.info <- x$ndx[i,]
  ##   ## How much of the tail of x$file consists of data for elements
  ##   ## being dropped, and hence can be truncated?
  ##   item.info <- item.info[order(item.info$offs, decreasing=TRUE),]
  ##   num.to.drop <- i(last.pos - cumsum(item.info$size) != item.info$offs)[1] - 1
  ##   if (is.na(num.to.drop))
  ##     num.to.drop <- length(i)
  ##   if (num.to.drop > 0) {
  ##     seek(x$file, item.info$offs[num.to.drop], rw="write")
  ##     ## R or linux bug?: need to flush the file after seek 
  ##     ## to guarantee truncate will work
  ##     flush(x$file)
  ##     truncate(x$file)
  ##   }
  ##   ## mark the storage for these items as undefined
  ##   x$ndx[i, 1] <- data.frame(offs=rep(as.double(NA), length(i)))
  ##   seek(x$file, 0, "end", rw="write")
}

