\name{initialize-methods}
\alias{initialize-methods}
\alias{initialize,ulong-method}
\alias{initialize,slong-method}
\alias{initialize,fmpz-method}
\alias{initialize,fmpq-method}
\alias{initialize,mag-method}
\alias{initialize,arf-method}
\alias{initialize,acf-method}
\alias{initialize,arb-method}
\alias{initialize,acb-method}
\docType{methods}
\title{Initialize Object Prototypes}
\description{
  Initialize \code{\linkS4class{flint}} vector prototype with zeros,
  with values from an atomic vector or another \code{flint} vector, or
  \dQuote{by parts}.  These methods for \code{\link{initialize}} are
  used by corresponding methods for \code{\link{coerce}} (with signature
  \code{c(from = "ANY", to = class(.Object))}) as well as by the class
  generator functions \code{\link{.flint}}, \code{\link{.flint.array}},
  etc.  Other interfaces, including
  \code{\link{new}(class(.Object), \dots)} and direct calls
  \code{\link{initialize}(.Object, \dots)}, are beyond the API of
  package \pkg{flint}.  This page is intended only for expert users
  trying to make sense of the source code.
}
\usage{
\S4method{initialize}{ulong}(.Object, x, length, dim, dimnames, names, \dots)
\S4method{initialize}{slong}(.Object, x, length, dim, dimnames, names, \dots)
\S4method{initialize}{fmpz}(.Object, x, length, dim, dimnames, names, \dots)
\S4method{initialize}{fmpq}(.Object, x, length, dim, dimnames, names, num, den, \dots)
\S4method{initialize}{mag}(.Object, x, length, dim, dimnames, names, \dots)
\S4method{initialize}{arf}(.Object, x, length, dim, dimnames, names, \dots)
\S4method{initialize}{acf}(.Object, x, length, dim, dimnames, names, real, imag, \dots)
\S4method{initialize}{arb}(.Object, x, length, dim, dimnames, names, mid, rad, \dots)
\S4method{initialize}{acb}(.Object, x, length, dim, dimnames, names, real, imag, \dots)
}
\arguments{
  \item{.Object}{
    a copy of the prototype of a \code{\linkS4class{flint}} class.}
  \item{x}{
    \code{NULL} or an atomic or \code{\linkS4class{flint}} vector.}
  \item{length}{
    \code{NULL} or a numeric vector of length equal to 1.}
  \item{dim}{
    \code{NULL} or a numeric vector of length greater than 0.}
  \item{dimnames}{
    \code{NULL} or (only if \code{dim} is not \code{NULL}) a list of
    length equal to \code{length(dim)} whose components are \code{NULL}
    or vectors of suitable length.  Non-\code{NULL} components are
    coerced to character using \code{\link{as.character.default}} or
    \code{\link{as.character.factor}}.}
  \item{names}{
    \code{NULL} or a vector of suitable length.  Non-\code{NULL} values
    are coerced to character using \code{\link{as.character.default}} or
    \code{\link{as.character.factor}}.}
  \item{num}{
    \code{NULL} or an \code{\linkS4class{fmpz}} vector, used to
    initialize numerators.}
  \item{den}{
    \code{NULL} or an \code{\linkS4class{fmpz}} vector, used to
    initialize denominators.}
  \item{mid}{
    \code{NULL} or an \code{\linkS4class{arf}} vector, used to
    initialize midpoints.}
  \item{rad}{
    \code{NULL} or a \code{\linkS4class{mag}} vector, used to
    initialize radii.}
  \item{real}{
    \code{NULL} or an \code{\linkS4class{arf}} (for \code{acf}) or
    \code{\linkS4class{arb}} (for \code{acb}) vector, used to initialize
    real parts.}
  \item{imag}{
    \code{NULL} or an \code{\linkS4class{arf}} (for \code{acf}) or
    \code{\linkS4class{arb}} (for \code{acb}) vector, used to initialize
    imaginary parts.}
  \item{\dots}{
    optional arguments, though these are currently unused.}
}
\value{
  The value of \code{.Object} after setting its \code{.xData} slot and
  (conditionally, depending on arguments after \code{.Object}) the
  \code{dim}, \code{dimnames}, and \code{names} slots.  The new
  \code{.xData} slot is an external pointer to a newly allocated array
  of the corresponding \proglang{C} type.  The array is initialized
  according to arguments after \code{.Object}.

  Note that the value of \code{.Object} is not copied, so all references
  to it are affected.
}
\keyword{methods}
\keyword{programming}
