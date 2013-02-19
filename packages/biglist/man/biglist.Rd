\name{biglist}
\alias{biglist}
\alias{biglist.default.cache.size}
\alias{biglist.header.string}
\alias{biglist.indexfile.suffix}
\alias{c.biglist}
\alias{close.biglist}
\alias{drop.items}
\alias{flush.biglist}
\alias{length<-.biglist}
\alias{length.biglist}
\alias{names.biglist}
\alias{print.biglist}
\alias{[[.biglist}
\alias{[[<-.biglist}
\alias{[.biglist}
\title{biglist - simple disk-based lists of arbitrary R objects}
\description{
Creates file-backed lists of arbitrary R objects, indexed by integers.
Elements can be assigned in any order without space/time penalties.
This package is intended for keeping persistent lists of items that are
rarely changed after being created.  WARNING:  changing or deleting
biglist items after they are created leaves holes in the file,
wasting disk space.
}
\usage{
biglist(filename, overwrite = FALSE, names = NULL, header =
NULL, cache.size = biglist.default.cache.size, read.only=FALSE)
biglist.default.cache.size
biglist.header.string
biglist.indexfile.suffix
drop.items(x, i)
\method{c}{biglist}(x, ...)
\method{close}{biglist}(con, ...)
\method{flush}{biglist}(con)
\method{length}{biglist}(x)
\method{length}{biglist}(x)<-value
\method{names}{biglist}(x)
\method{print}{biglist}(x, ...)
\method{[}{biglist}(x, i)
\method{[[}{biglist}(x, i)
\method{[[}{biglist}(x, i) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{path to the file for storing the bigframe data }
  \item{overwrite}{if TRUE, create a new biglist, overwriting any data already in
    the file; if FALSE, use/append to data already in file}
  \item{names}{for biglists whose items are lists, this will be the
    default \code{names} attribute for those items.  Avoids repeatedly
    writing a shared \code{names}attribute.}
  \item{header}{an arbitrary R list associated with this biglist}
  \item{cache.size}{maximum number of biglist items cached in memory}
  \item{read.only}{logical: should biglist be open biglist for read only?}
  \item{x}{a biglist}
  \item{con}{a biglist (parameter named to match generic method)}
\item{i}{positive integer vector of list indexes}
\item{value}{an integer (for \code{length<-}) or an arbitrary R object}
(for \code{[[<-})
\item{...}{additional arguments to the print method}
}
\details{
  A biglist is meant to store a large list of R objects on disk.  Only a small
  sublist (the cache) is kept in memory at any time, but access to other
  items in the list requires only 1 or 2 file seeks.  Items in a biglist
  are retrieved or set by using the \code{[[} and \code{[} methods (and
  their corresponding replacement methods), as
  for regular R lists, except that indexes can only be positive integers.
  by using a \code{bigframe} object as an index to record offsets and sizes of list
  items in the file.  The biglist file is compact: assigning biglist
  items beyond the current maximum index does not leave holes; however,
  the size of the index file is proportional to the maximum index of an
  assigned biglist item.  Items not yet assigned return NULL when the
  biglist is indexed.  Storage in the biglist file is NOT recovered when
  items are deleted or replaced, unless those items form a contiguous block at the
  end of the file (i.e. deleting the last n items added to a list,
  regardless of their indexes, truncates the corresponding tail of the file).
  A biglist is best suited (under the current implementation) for storing a dynamically calculated set of R
  objects which, once assigned to a slot in the list, rarely or never
  change, since storage is not usually reclaimed when slots are
reassigned values.

  To force cached data to be written to disk, use \code{flush(x)}.  To
  flush data and close the backing files, use \code{close(x)}, after
  which \code{x} is no longer usable.
}  
  
  
  
}
\value{
  \code{biglist} returns an EXTPTR object of class "biglist" with these attributes
  \item{ndx} a bigframe object (the biglist index) opened with the name filename.ndx
  \item{filename} filename for this biglist
  \item{file} an open binary R read/write file connection for the
  biglist
  
  \code{names} returns the value of \code{names} with which the
  biglist was created, and is NOT the names of the items in the
  biglist, but the default names attributes of any biglist items which are
  themselves lists.
}
\references{}
\author{John Brzustowski  <jbrzusto@fastmail.fm>}
\note{This package is part of the radR project http://radr.wik.is}
}
\seealso{\code{bigframe}}
\examples{
}
\keyword{ data }% at least one, from doc/KEYWORDS
\keyword{ array }% __ONLY ONE__ keyword per line
