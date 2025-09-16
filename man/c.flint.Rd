\name{c.flint}
\alias{c.flint}
\alias{cbind.flint}
\alias{rbind.flint}
\title{Concatenate Vectors}
\description{
  Function \code{\link{c}} is primitive and internally generic but it
  dispatches only on its first argument.  A corollary is that
  \code{c(x, \dots)} does \emph{not} dispatch the S4 method with
  signature \code{x="flint"} if \code{x} is not a \code{flint} vector,
  even if a \code{flint} vector appears later in the call as a
  component of \code{\dots}.

  Functions \code{\link{cbind}} and \code{\link{rbind}} are internally
  generic and dispatch on all components of \code{\dots}, creating the
  possibility of dispatch ambiguities; see \code{\link{cbind2}} and
  \code{\link{rbind2}}.

  S3 methods \code{c.flint}, \code{cbind.flint} and \code{rbind.flint}
  are registered \emph{and exported} to enable users to bypass internal
  dispatch.
}
\usage{
\method{c}{flint}(\dots, recursive = FALSE, use.names = TRUE)
\method{cbind}{flint}(\dots, deparse.level = 1)
\method{rbind}{flint}(\dots, deparse.level = 1)
}
\arguments{
  \item{\dots}{
    objects inheriting from virtual class \code{\linkS4class{flint}} or
    whose type is one of the vector types or one of the non-vector types
    \code{NULL}, \code{pairlist}, \code{symbol}, and \code{language}.}
  \item{recursive}{
    a logical indicating if pairlists, lists, and expressions should be
    handled recursively.  If \code{TRUE}, then the function behaves as
    if such arguments were replaced by their terminal nodes.}
  \item{use.names}{
    a logical indicating if names should be preserved.}
  \item{deparse.level}{
    an integer (0, 1, or 2) indicating how names are chosen for rows or
    columns derived from untagged, non-matrix arguments.  0 is to use
    empty names, 2 is to deparse unevaluated arguments, and 1 (the
    default value) is to deparse unevaluated arguments only if they are
    symbols and otherwise use empty names.}
}
\value{
  If none of the arguments is a \code{flint} vector, then the internal
  default methods are dispatched.

  If at least one argument is a \code{flint} vector, then the return
  value is a \code{flint} vector, unless \code{recursive = FALSE} and
  at least one argument is a pairlist, name, call, list, or expression,
  in which case the return value is a list or expression.

  If the return value is a \code{flint} vector, then its class is the
  most specific subclass of \code{flint} whose range contains the ranges
  of the classes of the arguments.
}
\examples{
x <- slong(2:5)
c(x, 6L)
c(1L, x) # bad
c.flint(x, 6L)
c.flint(1L, x)
}
\keyword{array}
\keyword{manip}
