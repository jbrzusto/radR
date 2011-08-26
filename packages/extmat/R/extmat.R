"extmat" <-
function(name="anonymous extmat", type="int", dim=c(10, 10), elt.size=extmat.sizes[type], slop=0) {
  type <- tolower(type)
  if (!type %in% names(extmat.types))
    stop(sprintf("unknown extmat type %s", type))
  type <- extmat.types[type]
  if (type != 11)
    elt.size <- extmat.sizes[type]
  else if (missing(elt.size))
    stop("'elt.size' parameter must be specified for extmat with type='user'")
  elt.size <- as.integer(elt.size)
  if (elt.size < 0)
    stop("'elt.size' parameter must be a positive integer")
  dim <- as.integer(dim)
  rv <- .Call(extmat_create, name, type, elt.size, dim, slop)
  reg.finalizer(rv, function(x) .Call(extmat_destroy, x))
  rv
}

