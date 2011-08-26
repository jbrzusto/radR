"[[<-.strictenv" <-
function(se, sym, NEW=FALSE, FORCE=FALSE, INHERITS=FALSE, value) {
  if (is.numeric(sym)) {
    if (sym < 1 || sym > length(se))
      stop("Numeric index ", sym, " out of bounds for strict environment with ", length(se), " elements.")
    sym <- names(se)[[sym]]
  } else {
    if (!NEW && !FORCE && !exists(sym, se))
      stop("Unknown symbol '", sym, "' in strict environment.\nAdd ',new=TRUE' inside the brackets if you really want to create a new symbol.")
    if (NEW && !FORCE && exists(sym, se))
      stop("Attempt to create symbol '", sym, "' in a strict environment where it already exists.\nUse ',FORCE=TRUE' instead of ',NEW=TRUE' to bind the symbol whether or not it exists.")
  }
  assign(sym, value, se, inherits=INHERITS)
  invisible(se)
}

