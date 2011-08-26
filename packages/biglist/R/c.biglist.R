"c.biglist" <-
function(x, ...) {
  ## - append item(s) to list x
  for (i in list(...))
    x[[length(x) + 1]] <- i
  invisible(x)
}

