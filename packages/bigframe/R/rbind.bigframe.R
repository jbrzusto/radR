"rbind.bigframe" <-
function(x, data) {
  ## - append the rows in data to the bigframe fr
  fr[1 + dim(fr)[1],] <- data
  invisible(fr)
}

