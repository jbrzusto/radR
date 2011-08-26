"name<-.extmat" <- function(x, ..., value) {
  .Call(extmat_set_name, x, value)
}
