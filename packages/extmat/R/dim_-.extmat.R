"dim<-.extmat" <-
function(x, NOTRIGGER=FALSE, value) {
  rv <- .Call(extmat_set_dim, x, value, as.logical(NOTRIGGER))
  if (!is.null(rv))
    return(rv)
  stop(paste("extmat: storage for '", .Call(extmat_get_name, x), "' is owned by a module and can not be resized"))
}

