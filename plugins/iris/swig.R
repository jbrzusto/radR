## update the object system cache ?
cacheMetaData(1)

## get the table of SWIG types.  This is a hashed environment whose
## symbols are the names of R classes used to represent the SWIG
## objects.  Each symbol is bound to an integer vector that gives the
## index of that symbol in the C-level swig_types table, and the size
## of the type in bytes.

SWIG.types <- .Call("get_type_table")

## Return a SWIG ExternalReference object pointing to
## the address of a RAW vector or an existing EXTPTR.
## In the former case, the returned object refers
## to the RAW vector in its source attribute, and so
## protects it.  We add a "_p_" to the class name because
## SWIG does.

cast <- function(x, class) {
  class <- paste("_p_", class, sep="")
  i <- get(class, SWIG.types)
  structure(.Call("ptr_cast", x, i[1]), class=class)
}

## return the size of the object pointed to by a
## SWIG ExternalReference

sizeof <- function(e) {
  if (is(e, "ExternalReference"))
    get(class(e), SWIG.types)[2]
  else
    get(paste("_p_", e, sep=""), SWIG.types)[2]
}

setMethod("names", "ExternalReference", function(x) {names(eval(body(getMethod("$", class(x)))[[2]]))})
          
toRaw <- function(ptr, len) {
  .Call("ptr_to_raw", ptr, as.integer(len))
}

setMethod("print", "ExternalReference",
          function(x,...) {
            print.default(paste(substring(class(x), 4), capture.output(print.default(x))[1]))
          }
          )

setMethod("[[", "ExternalReference",
          function(x, i, j) getMethod("$", class(x))(x, i))

setMethod("[", "ExternalReference",
          function(x, i, j, drop) {
            f <- getMethod("$", class(x))
            if (missing(i))
              i <- names(x)
            g <- function(i) f(x,i)
            structure(lapply(i, g), names=i)
          }
          )

setMethod("str", c("ExternalReference"),
          function(object){cat(paste(paste(sprintf("%30s: %s", names(object), as.character(object[names(object)])), collapse="\n"), "\n", sep=""))}
          )
  
## Pointer arithmetic. The RHS is a one or two element
## vector.  RHS[1] is how many object-sized slots to change the pointer by,
## and RHS[2] is how many bytes to change the pointer by.  Either can be
## positive or negative.  RHS[2] is set to zero if not supplied.

setMethod("+", c("ExternalReference", "numeric"),
          function(e1, e2) {
            if (length(e2) < 2)
              e2[2] <- 0
            structure(.Call("ptr_incr", e1, e2[1] * sizeof(e1) + e2[2]), class=class(e1))
          }
          )

setMethod("-", c("ExternalReference", "numeric"),
          function(e1, e2) {
            if (length(e2) < 2)
              e2[2] <- 0
            structure(.Call("ptr_incr", e1, -e2[1] * sizeof(e1) - e2[2]), class=class(e1))
          }
          )

## ptr arithmetic, returning differences in bytes

setMethod("-", c("ExternalReference", "ExternalReference"),
          function(e1, e2) {
            .Call("ptr_diff", e1, e2)
          }
          )

setMethod("-", c("ExternalReference", "raw"),
          function(e1, e2) {
           .Call("ptr_diff", e1, e2)
          }
          )


setMethod("-", c("raw", "ExternalReference"),
          function(e1, e2) {
            .Call("ptr_diff", e1, e2)
          }
          )

