"strictenv" <-
function(..., PARENT=emptyenv(), LOCALIZE.FUNS=TRUE) {
  ## generate a new strict environment from a list or environment
  x<-new.env(hash=TRUE, PARENT)
  lst <- list(...)
  for (n in names(lst)) {
    x[[n]] <- lst[[n]]
    if (LOCALIZE.FUNS && is.function(x[[n]]))
      environment(x[[n]]) <- x
  }
  if (LOCALIZE.FUNS)
    for (n in names(x))
      if (is.function(x[[n]]))
        environment(x[[n]]) <- x
  class(x)<-"strictenv"
  return(x)
}

