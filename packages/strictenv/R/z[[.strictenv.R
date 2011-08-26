"[[.strictenv" <-
function(se, sym) {
  if (is.numeric(sym)) {
    if (sym < 1 || sym > length(se))
      stop("Numeric index ", sym, " out of bounds for strict environment with ", length(se), " elements.")
    sym <- names(se)[[sym]]
  } else {
    if (!exists(sym, se))
      stop("Unknown symbol '", sym, "' in strict environment.")
  }
  return (get(sym, se))
}

