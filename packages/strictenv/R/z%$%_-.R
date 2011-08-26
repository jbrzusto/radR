"%$%<-" <-
function(x, new.sym, value) {
  new.sym<-as.character(substitute(new.sym))
  if (exists(new.sym, x))
    stop("Attempt to create symbol '", new.sym, "' in a strict environment where it already exists.\nUse '%$!%' instead of '%$%' to bind the symbol regardless of whether it exists or not.")
  assign(new.sym, value, x)
  invisible(x)
}

