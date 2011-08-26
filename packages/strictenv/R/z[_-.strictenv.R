"[<-.strictenv" <-
function(se, index, INHERITS=FALSE, value) {
  if (is.null(value)) {
    if (INHERITS)
      stop("Only non-inherited symbols can be deleted:  do not specify 'inherits=TRUE'.")
    for (i in seq(along=index))
      if (!exists(index[[i]], se, inherits=FALSE))
        stop("Unknown non-inherited symbol '", index[[i]], "' in strict environment.")
    rm(list=as.character(index), envir=se)
  } else {
    if (!identical(length(index), length(value)))
      stop("Attempt to assign to a strictenv subset with length(subset) != length(replacement)")
    switch(mode(index),
           character = {
             for (i in seq(along=index)) {
               if (!exists(index[[i]], se))
                 stop("Unknown symbol '", index[[i]], "' in strict environment.")
               assign(index[[i]], value[[i]], se, inherits=INHERITS)
             }
           },
           numeric = {
             nm <- names(se)
             for (i in index)
               assign(nm[i], value[[i]], se)
           },
           stop("Mode of index for [<-.strictenv must be 'character' or 'numeric'")
           )
  }
  invisible(se)
}

