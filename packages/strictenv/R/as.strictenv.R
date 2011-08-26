"as.strictenv" <-
function(lst, PARENT=emptyenv(), LOCALIZE.FUNS=TRUE) {
  ## generate a new strict environment from a list or environment
  x<-new.env(hash=TRUE, PARENT)
  if (inherits(lst, "list"))
    for (n in names(lst))
      x[[n]] <- lst[[n]]
  else if (inherits(lst, "environment"))
    for (n in ls(lst, all.names=TRUE))
      x[[n]] <- get(n, lst)
  if(LOCALIZE.FUNS)
    for (n in names(x))
      if (is.function(x[[n]]))
        environment(x[[n]]) <- x
  class(x)<-"strictenv"
  return(x)
}

