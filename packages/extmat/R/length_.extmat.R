"length<-.extmat" <-
function(x, value) {
  dim(x) <- c(value, 1)
}
