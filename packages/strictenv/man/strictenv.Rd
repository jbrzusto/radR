\name{strictenv}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{strictenv}
\alias{as.strictenv}
\alias{$.strictenv}
\alias{[.strictenv}
\alias{[[.strictenv}
\alias{$<-.strictenv}
\alias{^<-}
\alias{^<-.strictenv}
\alias{[<-.strictenv}
\alias{[[<-.strictenv}
\alias{\%$\%<-}
\alias{\%$!\%<-}
\alias{\%in\%}
\alias{as.list.strictenv}
\alias{c.strictenv}
\alias{names.strictenv}
\alias{names<-.strictenv}
\title{Strict Environments}
\description{
  \code{strictenv} is a wrapper class for environments that enforces strict semantics for
  symbol creation and lookup.  New symbols must be created using the \code{\%$\%<-}
  operator or the \code{NEW=TRUE} argument to the \code{[[} operator.
  Presence of a symbol can be tested using the \code{\%in\%} operator.
  Attempts to lookup or assign to non-existing symbols generate errors.
  Multiple existing symbols can be looked-up or assigned to using \code{[}
  and \code{[<-}.  Strictenv objects can inherit symbols from a parent,
  specified when the object is created.
  Lookup retrieves the first instance of the symbol found along the chain of
  inheritence starting at \code{x}. By default, assignment binds
  the symbol in \code{x} itself.
  However, if  \code{^<-} or \code{INHERITS=TRUE} is used, then assignment
  binds the symbol in the first place where it is found along
  chain of inheritence.  This is either in \code{x} or in one of its
  ancestors.
}
\usage{
`^<-`<-function(x, ...) UseMethod("^<-", x)
x <- strictenv(..., PARENT=emptyenv(), LOCALIZE.FUNS=TRUE)
x <- as.strictenv(lst, PARENT=emptyenv(), LOCALIZE.FUNS=TRUE)
x \%$\% new.sym <- value
x [["new.sym", NEW=FALSE, FORCE=FALSE]] <- value
x \%$!\% any.sym <- value
x $ sym
x [["sym"]]
x [[i]]
"any.sym" \%in\% x
x [index]
x $ sym <- value
x [index] <- value.lst
x [index] <- NULL
x [sym.vec, INHERITS=FALSE] <- value.lst
x [[sym, INHERITS=FALSE]] <- value

\method{as.list}{strictenv}(x, ...)
\method{names}{strictenv}(x)
\method{names}{strictenv}(x) <- value
\method{c}{strictenv}(x, ..., FORCE=FALSE)
}

\arguments{
  \item{PARENT}{the enclosing frame for the strict environment}
  \item{LOCALIZE.FUNS}{if \code{TRUE}, functions in the strictenv will
    have their environment set to the strictenv, so that they may refer
    to symbols there by their unqualified names}
  \item{lst}{ an environment or a list with named items}
  \item{value.lst}{a list or vector of values; names are ignored}
  \item{x}{a \code{strictenv}}
  \item{sym}{a symbol in the \code{strictenv}}
  \item{sym.vec}{a vector of symbols in the \code{strictenv}}
  \item{i}{index of a symbol in \code{names(x)}}
  \item{new.sym}{a symbol not in the \code{strictenv}}
  \item{any.sym}{any symbol, whether in the \code{strictenv} or not}
  \item{index}{character vector of names of symbols in the
    \code{strictenv}, or numeric vector of indexes}
  \item{value}{a value for binding to a symbol}
  \item{NEW}{causes a new symbol to be created; generates an error if
    the symbol already exists}
  \item{FORCE}{cause a symbol to be bound whether it exists or not; if
    it does not exist, it is added to the \code{strictenv}}
  \item{INHERITS}{logical: if TRUE, symbol binding occurs where the
    symbol is already found, either in \code{x} or one of its ancestors.
    If FALSE, symbol binding occurs in \code{x}, regardless of whether
    a copy already is bound in one of its ancestors.}
  \item{...}{one or more of the following, separated by commas:
    an environment, a list with named items, a \code{sym=value} term}
}
\details{
  The function \code{strictenv} creates an object of class
  \code{"strictenv"} from its \code{lst} argument.  
  New symbols must be explictly added to a \code{strictenv}.
  Attempting to add a symbol as "new" when it already exists causes an
  error.  Use the \code{\%$!\%} operator or the \code{FORCE=TRUE} option
  to the \code{[[} operator to bind a symbol whether or not it is
  already in the \code{strictenv}.
  Numeric indexes for \code{[[} and \code{[} access the elements in the same
  order as they appear in \code{names()}, which in the current
  implementation is the same alphabetical order given by \code{ls()}.
  The \code{c(se, sym.source)} function creates new bound symbols in the first argument
  from symbol sources listed in the rest of its arguments. A symbol
  source can be an environment, a list with named items, or simply a
  \code{sym=value} argument.  If \code{FORCE=FALSE}, all
  symbols in the symbol sources must be new (i.e. not present in the
  first argument), and no symbol may occur more than once
  among the symbol source arguments.  If \code{FORCE=TRUE},
  existing symbols are rebound and since arguments are processed from
  left to right, the last instance of a given symbol among the arguments
  will be the one whose binding is copied to \code{se}.
  The expression \code{names(se)<-...} will remove from \code{se} any symbols whose names are
  not in the character vector on the RHS, and will bind any new names
  there to \code{NULL}.

  Symbols are looked up with inheritence along the path implied by the
  \code{PARENT} parameters used to create \code{x} and its ancestors (if
  any).  However, \code{\%in\%} only looks at \code{x} itself.
  
  
  The \code{^<-} variant and \code{INHERITS=TRUE} option are provided to allow class variables rather
  than instance variables:  These forms of assignment will bind the
  symbol wherever it is found in the inheritence chain starting at
  \code{x}, whereas \code{$<-} will always bind the symbol in \code{x}
  itself, creating a new binding if the symbol was inherited. 

  The form \code{x[index] <- NULL} deletes those symbols named in
  \code{as.character(index)} from object \code{x}.  It is an error
  to specify a symbol which is not bound in \code{x} itself (rather
  than in one of its ancestors).  This is the recommended way to remove a
  local binding for an inherited symbol, so that the inherited binding
  is once again visible.
  
}
\value{
  If \code{lst} is a
  list, it is converted to an environment with symbols \code{names(lst)}
  bound to the corresponding items in \code{lst}.  Then the environment
  \code{lst} is given the class \code{"strictenv"}.  For convenience,
  methods are provided to allow subsetting and subset assignment with
  character indexed vectors, as for lists.  \code{as.list(x)} returns a
  list with the same names and values as \code{x}.
   
}
\author{John Brzustowski <jbrzusto@fastmail.fm>}

\examples{
## create a strict environment directly
se <- strictenv(x=1, y=2)

## create a strictenv from a list with named items
lst <- list(a=45, .b="blatto")
se <- as.strictenv(lst)

## create a strictenv that inherits from an existing one

se2 <- strictenv (c=23, PARENT=se)

## use the value of an inherited symbol

se2$a + 3

## make a local copy of an inherited symbol

se2$a <- 10

## the ancestor object retains the original symbol binding
se$a != se2$a

## remove the local copy of an inherited symbol
se2["a"] <- NULL
se$a != se2$a

## change the ancestral binding of an inherited symbol
se2^a <- "some new value"
se$a == se2$a

## set the list of existing symbols in a strict environment
## (new symbols are bound to NULL; existing ones are left unchanged)
names(se) <- c(names(se), "me", "metoo")

## combine a strict environment with new bindings
se <- c(se, eee="help", sos="help faster", list(extra="stuff"))

## return the value of a symbol
se$a
se[[".b"]]

## assign a new value to a symbol
se$.b <- "new blatto"

## add and bind a new symbol
se\%$\%funny <- "joke"
se[["more.funny", NEW=TRUE]] <- "two jokes"

## bind a symbol, creating it if it does not exist
se\%$!\%funny <- "punchline"
se[["even.more.funny", FORCE=TRUE]] <- "three jokes"

## subset with a character vector, showing a strictenv behaving
## more like a list than an environment
se[c("funny", "even.more.funny")]

## assignment to a subset, showing a strictenv behaving
## more like a list than an environment
se[c("funny", "even.more.funny")] <- list(1:10, "some stuff")

## addition of new symbols from other sources to a strictenv
c(se, aaa="before the rest", list(pi=3, e=2), bbb="again")

## addition of new symbols and rebinding of existing symbols 
c(se, aaa="first try", list(aaa="second try"), FORCE=TRUE)

## list elements
ls(se)    # will not list symbols beginning with "."
names(se) # list all symbols

## error upon look up of non-existent symbol
\dontrun{se$f}
\dontrun{se[["g"]]}

## error upon attempt to implicitly create a new symbol
\dontrun{se$f <- 1:10}
\dontrun{se[["g"]] <- "won't work"}

## error upon attempt to explicitly create new symbol
## when it already exists
\dontrun{se\%$\%funny <- "not so much"}
\dontrun{se[["even.more.funny", NEW=TRUE]] <- "bad jokes"}

## test for existence of a symbol
"g"  \%in\% se    # returns FALSE
".b" \%in\% se    # returns TRUE

}
\keyword{ data }% at least one, from doc/KEYWORDS
\keyword{ misc }% __ONLY ONE__ keyword per line
