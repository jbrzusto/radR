"format.extmat" <-
function(x, ...) {
  format(structure(x[], class=class(x)[-1]), ...)
}
