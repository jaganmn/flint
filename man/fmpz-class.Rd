\name{fmpz-class}
\alias{fmpz-class}
\alias{initialize,fmpz,numeric-method}
\alias{coerce,numeric,fmpz-method}
\alias{coerce,fmpz,integer-method}
\alias{coerce,fmpz,double-method}
\title{Class of Vectors of Multiple Precision Integers}
\docType{class}
\description{
  Class \code{fmpz} represents vectors of multiple precision integers.
  The limb size is 32 or 64 bits, depending on the configuration of
  the linked \abbr{FLINT} library.  The number of bits is given by
  \code{flintBits()}.
}
\section{Slots}{
  \describe{
    \item{\code{length, x}}{
      inherited from \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz", x = "numeric")}:\cr
      return \code{.Object} with \code{length} slot set to a
      representation of \code{length(x)} and \code{x} slot set to the
      address of an initialized array of \code{fmpz}.  The array is
      initialized with values from \code{x}, which is truncated towards
      zero.  An error is signaled if any \code{x[i]} is \code{NaN},
      \code{-Inf}, \code{Inf}, or subnormal.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "fmpz")}:\cr
      equivalent to \code{new("fmpz", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "fmpz", to = "integer")}:\cr
      \code{signature(from = "fmpz", to = "double")}:\cr
      return an integer or double vector containing the result of
      converting the original integer type to the range of the target
      type.  The behaviour when the integer is not exactly
      representable in the target type is to introduce \code{NA} with
      a warning (\code{"integer"}) or to round without a warning
      (\code{"double"}).  Rounding is towards zero, except where the
      original number exceeds \eqn{2^1024} in absolute value, in which
      case the behaviour is to introduce \code{-Inf} or \code{Inf}
      with a warning.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
flintBits()
}
