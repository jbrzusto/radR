"as.vector.extmat" <-
function(x, ...) {
  as.vector(structure(x[], class=class(x)[-1]), ...)
}
