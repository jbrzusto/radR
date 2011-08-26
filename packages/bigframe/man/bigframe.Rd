\name{bigframe}
\alias{bigframe}
\alias{bigframe.default.cache.size}
\alias{bigframe.header.string}
\alias{bigframe.type.sizes}
\alias{close.bigframe}
\alias{dim<-.bigframe}
\alias{dim.bigframe}
\alias{flush.bigframe}
\alias{length.bigframe}
\alias{names.bigframe}
\alias{print.bigframe}
\alias{rbind.bigframe}
\alias{[<-.bigframe}
\alias{[.bigframe}
\alias{[[.bigframe}
\alias{$<-.bigframe}
\alias{$.bigframe}
  
\title{bigframe - disk-backed data frames with fixed row size}
\description{
Creates file-backed data.frame like objects.  Data are inserted/extracted/deleted using indexing and indexed assignment. Allowed column types are logical, integer, real, complex, and character.  Character columns are given maximum string lengths (from 4 to 512) at creation time.  Assigning to non-contiguous parts of the frame fills intervening rows with NA values, extending the file. Deleting rows from the end of a bigframe truncates the associated file.
}
\usage{
bigframe(filename, frame = NULL, header = NULL, cache.size =
bigframe.default.cache.size, read.only = FALSE)
bigframe.default.cache.size
bigframe.header.string
bigframe.type.sizes
\method{close}{bigframe}(con, ...)
\method{dim}{bigframe}(x)
\method{dim}{bigframe}(x) <- value
\method{flush}{bigframe}(con)
\method{length}{bigframe}(x)
\method{names}{bigframe}(x)
\method{print}{bigframe}(x, ...)
\method{rbind}{bigframe}(x, data)
\method{[}{bigframe}(x, i=1:dim(x)[1], j=1:length(attr(x, "header")$signature))
\method{[}{bigframe}(x, i=1:dim(x)[1], j=1:length(attr(x, "header")$signature)) <- value
\method{[[}{bigframe}(x, i)
\method{$}{bigframe}(x, a)
\method{$}{bigframe}(x, a) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ path to the file for storing the bigframe data }
  \item{frame}{ data.frame from which to create the bigframe. }
  \item{header}{ an arbitrary R list to store in bigframe header; any items named \code{signature} or \code{row.bytes} will be overwritten (see below)}
  \item{cache.size}{ number of bigframe rows to maintain in memory cache }
  \item{read.only}{logical: if TRUE, open bigframe for read only}
  \item{x}{bigframe object}
  \item{con}{bigframe object; parameter named for consistency with generic \code{flush} method}
  \item{a}{unquoted name of bigframe column}
  \item{data}{data.frame for appending to a bigframe}
  \item{i}{bigframe row indexing expression}
  \item{j}{bigframe column indexing expression}
  \item{value}{value to assign to bigframe dimension, column, or entries}
  \item{...}{additional arguments to the \code{print} function}
}
\details{
A bigframe is meant to store a large data.frame on disk.  Reasonable read/write
performance is maintained by caching rows in memory.  Calling \code{bigframe()} with
\code{frame==NULL} opens a link to an existing bigframe on disk.  Otherwise,
a new bigframe object is created from the data.frame.  Indexing and indexed
assignment support a subset of the functionality of that for data.frame.
Use \code{flush(x)} to force all cached bigframe data in \code{x} to be
written to disk.
Use \code{close(x)} to flush cached data and close associated files,
after which \code{x} is no longer usable.
}
\value{
  \code{bigframe(...)} returns an R external pointer object of class \code{bigframe} with
  attribute \code{header}.  The header contains whatever the user specified in the \code{header}
  paramter to \code{bigframe(...)}, as well as these fields
  \item{row.bytes}{total number of bytes occupied on disk by each row of the bigframe}
  \item{signature}{a one-row data.frame whose columns are of the same type as those in the bigframe.  The value in each character column has \code{nchar()} equal to the maximum allowed for that column.}
  ...
}
\references{}
\author{John Brzustowski <jbrzusto@fastmail.fm>}
\note{}
\seealso{\code{biglist}}
\examples{
}
\keyword{data}% at least one, from doc/KEYWORDS
\keyword{array}% __ONLY ONE__ keyword per line
