"$<-.strictenv" <-
function(se, sym, value) {
  sym<-as.character(substitute(sym))
  if (!exists(sym, se))
    stop("Unknown symbol '", sym, "' in strict environment.\nUse '%$% instead of '$' if you really want to create a new symbol.")
  assign(sym, value, se)
  invisible(se)
}

