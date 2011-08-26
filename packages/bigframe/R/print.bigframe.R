"print.bigframe" <-
function(x, ...) {
  tryCatch({
    print(paste("bigframe with", dim(x)[1], "rows and columns", paste(names(x), ":", sapply(attr(x, "header")$signature, typeof), collapse=", ", sep=""), collapse=", "), ...)
  }, error = function(e) print("closed bigframe object"))
}

