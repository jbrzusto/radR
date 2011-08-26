"as.character.extmat" <-
function(x, ...) {
  .Call(extmat_get_name, x)
}

