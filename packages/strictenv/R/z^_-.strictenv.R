"^<-.strictenv" <-
function(se, sym, value) {
  sym<-as.character(substitute(sym))
  if (!exists(sym, se))
    stop("Unknown symbol '", sym, "' in strict environment.")
  assign(sym, value, se, inherits=TRUE)
  invisible(se)
}

