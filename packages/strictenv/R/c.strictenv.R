"c.strictenv" <-
function(x, ..., FORCE=FALSE) {
  ## catenate elements by adding them to the strict environment
  ## ... can be named arguments and/or lists
  ## named arguments and named elements of lists are
  ## added to the strict environment se
  ## Unless FORCE=TRUE, all symbols must be new
  ## (and hence unique among the arguments)
  
  l<-list(...)
  for (i in seq(along=l)) {
    if (inherits(l[[i]], c("strictenv", "list"))) {
      for (a in names(l[[i]]))
        x[[a, NEW=TRUE, FORCE=FORCE]] <- l[[i]][[a]]
    } else if (inherits(l[[i]], "environment")) {
      for (a in ls(l[[i]], all.names=TRUE))
        x[[a, NEW=TRUE, FORCE=FORCE]] <- l[[i]][[a]]
    } else {
      x [[names(l)[i], NEW=TRUE, FORCE=FORCE]] <- l[[i]]
    }
  }
  return(x)
}

