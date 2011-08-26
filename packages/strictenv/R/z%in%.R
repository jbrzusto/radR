`%in%` <- function(any.sym, x) {
  if (inherits(x, "strictenv"))
    exists(any.sym, x, inherits=FALSE)
  else
    base::`%in%`(x=any.sym, table=x)
}

