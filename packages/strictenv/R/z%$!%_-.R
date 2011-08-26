"%$!%<-" <-
function(x, any.sym, value) {
  any.sym<-as.character(substitute(any.sym))
  assign(any.sym, value, x)
  invisible(x)
}

