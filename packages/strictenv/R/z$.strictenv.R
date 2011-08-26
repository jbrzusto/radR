"$.strictenv" <-
function(se, sym) {
  sym<-as.character(substitute(sym))
  if (!exists(sym, se))
    stop("Unknown symbol '", sym, "' in strict environment.")
  return (get(sym, se))
}

