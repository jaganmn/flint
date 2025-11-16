\name{asVector}
\alias{asVector}
\alias{asMatrix}
\alias{asArray}
\title{Coerce an Object to a Vector Class}
\description{
  \code{asVector} is a generalization of \code{\link{as.vector}}
  enabling coercion from and to \code{\linkS4class{flint}} vector
  classes (in addition to basic vector classes) and providing more
  uniform handling of attributes.

  \code{asMatrix} and \code{asArray} are analogues generalizing
  \code{\link{as.matrix}} and \code{\link{as.array}}.
}
\usage{
asVector(x, mode = "any", strict = TRUE)
asMatrix(x, mode = "any", strict = TRUE)
asArray (x, mode = "any", strict = TRUE)
}
\arguments{
  \item{x}{
    an \R{} object coercible to the target class.}
  \item{mode}{
    a character string indicating the target class.}
  \item{strict}{
    a logical indicating if attributes of \code{x} should be discarded
    and if the class of the return value must match the target class
    exactly (and hence not be a subclass of the target class).}
}
\value{
  The result of coercing \code{x} to the target class indicated by
  \code{mode}.
}
\details{
  Argument \code{mode} can be one of the basic vector classes
  \code{"raw"}, \code{"logical"}, \code{"integer"}, \code{"numeric"}
  (synonym \code{"double"}), \code{"complex"}, \code{"character"},
  \code{"list"}, and \code{"expression"}; one of the \code{flint} vector
  classes \code{"ulong"}, \code{"slong"}, \code{"fmpz"}, \code{"fmpq"},
  \code{"mag"}, \code{"arf"}, \code{"acf"}, \code{"arb"}, and
  \code{"acb"}; or one of \code{"any"}, \code{"vector"}, and
  \code{"flint"}, indicating the vector class, basic vector class, and
  \code{flint} vector class \dQuote{nearest} the class of \code{x}.
  Note that \code{as.vector} supports \code{mode} equal to \code{"name"}
  (synonym \code{"symbol"}) or \code{"pairlist"}.  \code{asVector} does
  not: names and pairlists are not vectors!
}
\seealso{
  Virtual class \code{\linkS4class{vector}} and related functions
  \code{\link{as.vector}} and \code{\link{as}}.
}
\examples{
str(J  <- diag(ulong(1L), 2L))

as.integer(J)
as.vector(J, "integer")
as(J, "integer")
asVector(J, "integer")
asVector(J, "integer", FALSE)

setClass("ulongExtension", contains = "ulong")
str(J. <- new("ulongExtension", J))

str(asVector(J , "ulong"))
str(asVector(J., "ulong"))
str(asVector(J , "ulong", FALSE))
str(asVector(J., "ulong", FALSE))
}
\keyword{classes}
