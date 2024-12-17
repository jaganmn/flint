\name{c.flint}
\alias{c.flint}
\title{Concatenate Vectors}
\description{
  Primitive function \code{\link{c}} is internally generic but
  dispatches only on its first argument.  A corollary is that
  \code{c(x, \dots)} does \emph{not} dispatch the S4 method with
  signature \code{x="flint"} if \code{x} is not a \code{flint} vector,
  even if a \code{flint} vector appears later in the call as an element
  of \dots.  An S3 method \code{c.flint} is registered \emph{and}
  exported so that users can concatenate with \code{c.flint(x, \dots)}
  instead of \code{c(as(x, "flint"), \dots)}.
}
\usage{
\method{c}{flint}(\dots)
}
\arguments{
  \item{\dots}{
    objects inheriting from virtual class \code{\linkS4class{flint}},
    atomic (except character) vectors, and \code{NULL}.}
}
\value{
  If at least one argument is a \code{flint} vector, then the return
  value is a \code{flint} vector.  The length is the sum of the lengths
  of the arguments.  The class is the most specific subclass of
  \code{flint} containing the ranges of the classes of the arguments.

  If none of the arguments is a \code{flint} vector, then the return
  value is \code{c(\dots)}.
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\examples{
x <- slong(x = 2:5)
c(x, 6L)
c(1L, x)
c.flint(x, 6L)
c.flint(1L, x)
}
