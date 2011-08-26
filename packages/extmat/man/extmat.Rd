\name{extmat}
\alias{extmat}
\alias{as.character.extmat}
\alias{as.vector.extmat}
\alias{dim.extmat}
\alias{dim<-.extmat}
\alias{format.extmat}
\alias{length.extmat}
\alias{length<-.extmat}
\alias{name}
\alias{name.extmat}
\alias{name<-}
\alias{name<-.extmat}
\alias{pointer}
\alias{pointer.extmat}
\alias{print.extmat}
\alias{zero}
\alias{zero.extmat}
\alias{[.extmat}
\alias{[<-.extmat}
\alias{extmat.sizes}
\alias{extmat.types}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{extmat - large non-copying matrices of fixed-sized elements}
\description{
  For storing large matrices of fixed-sized elements.  These matrices
  are passed and assigned by reference within R, and hence are never
  copied.
}
\usage{
extmat(name = "anonymous extmat", type = "int", dim=c(10,10), elt.size=extmat.sizes[type], slop=0)
extmat.types
extmat.sizes
\method{as.character}{extmat}(x, ...)
\method{as.vector}{extmat}(x, ...)
\method{dim}{extmat}(x)
\method{dim}{extmat}(x, NOTRIGGER=FALSE)<-value
\method{format}{extmat}(x, ...)
\method{length}{extmat}(x)
\method{length}{extmat}(x)<-value
\method{name}{extmat}(x, ...)
\method{name}{extmat}(x, ...)<-value
\method{pointer}{extmat}(x, ...)
\method{zero}{extmat}(x, ...)
\method{print}{extmat}(x, ...)
\method{[}{extmat}(x, i, j, drop=TRUE, raw=FALSE)
\method{[}{extmat}(x, i, j, NOTRIGGER=FALSE)<-value
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{character vector describing the \code{extmat}}
  \item{type}{character vector element of names(extmat.types)}
  \item{value}{replacement values for elements of \code{x}, or new
dimensions or length for \code{x}, or a new name for \code{x}.  For
indexed assignment, \code{value} may be a character scalar like that
returned from indexing with \code{raw=TRUE}, in which case data are
copied from \code{value}, preserving their size.}
  \item{elt.size}{size in bytes of each matrix element; only specified
        if \code{type=="user"}}
  \item{dim}{integer vector of matrix dimensions}
  \item{x}{an extmat}
  \item{slop}{the number of extra elements to allocate after the matrix,
for those applications needing an additional buffer}
  \item{...}{additional arguments;  unused for \code{zero} and
   \code{name}}
  \item{i}{numeric vector of row indexes for matrix; defaults to all rows}
  \item{j}{numeric vector of column indexes for matrix; defaults to all
columns}
  \item{drop}{logical; if TRUE, a 1 x n or n x 1 matrix is returned as a
vector of length n}
  \item{raw}{logical; if TRUE, data are returned or expected as a packed
character vector of length one.  This is useful for integer types which
are smaller than the native R integer.}
  \item{NOTRIGGER}{if \code{TRUE}, disables any automatic action
registered to the extmat from being invoked}
}
\details{
An extmat can be redimensioned by \code{dim(x)<-}.  This will retain as
many elements of \code{x} as possible, and set any new elements to 0.
The name of an extmat can be extracted or set using \code{name(x)} and
\code{name(x)<-}.  All elements of an extmat can be zeroed using
\code{zero(x)}.  Indexes of extmats must be either all positive, or all
negative (meaning the negated indexes are excluded).  The index to an
extmat can be an integer matrix.  If it has two or more columns, the
last two columns are used as row and column indices, respectively.  Any
additional index columns are cbind() ed to the vector of indexed values.
Using \code{length(x)<-n} will make \code{dim(x) == c(n, 1)} and
preserve elements of \code{x}, adding zeroes for any new ones.  The
address of the first byte of the extmat data (as opposed to the header)
is returned as an externalptr object by vector \code{pointer(x)}.
\code{as.vector} and \code{format} allow a data.frame to include columns
of type extmat; the data.frame must be constructed as a list, and then
assigned class extmat.
}
\value{
}
\references{}
\author{John Brzustowski <jbrzusto@fastmail.fm>}
\note{}
\seealso{}
\examples{
}
\keyword{data} % at least one, from doc/KEYWORDS
\keyword{array} % __ONLY ONE__ keyword per line
