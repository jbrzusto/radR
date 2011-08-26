"print.biglist" <-
function(x, ...) {
  tryCatch({
    print(paste("biglist object with length = ", length(x), sep=""), ...)
  }, error = function(e) print("closed biglist object"))
}

