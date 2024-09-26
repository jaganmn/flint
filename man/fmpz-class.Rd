\name{fmpz-class}
\alias{fmpz-class}
\alias{nfmpz-class}
\alias{as.vector,fmpz-method}
\alias{coerce,vector,fmpz-method}
\alias{coerce,nfmpz,fmpz-method}
\alias{coerce,fmpz,nfmpz-method}
\alias{initialize,fmpz-method}
\title{Class of Vectors of Arbitrary Precision Integers}
\docType{class}
\description{
  Class \code{fmpz} extends virtual class \code{flint}.  It represents
  vectors of arbitrary precision integers.

  Class \code{nfmpz} extends virtual class \code{\linkS4class{nflint}}
  and basic vector class \code{\linkS4class{integer}}.  It is the
  \dQuote{native} analogue of \code{fmpz}.
}
\section{Slots}{
  \code{fmpz}:
  \describe{
    \item{\code{length}, \code{x}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{nfmpz}:
  \describe{
    \item{\code{.Data}}{
      a pseudo-slot whose \emph{apparent} value is the
      \dQuote{data part} of the object: a copy of the object with S4 bit
      unset and attributes removed.  \code{NA} represents integers not
      in \eqn{(-2^{31}, 2^{31})}, the range of \R{}'s integer type.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "fmpz")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero,
      unless the element exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz", length = "numeric")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz", x = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if any \code{x[i]} is \code{NaN}, \code{-Inf},
      \code{Inf}, or subnormal.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "fmpz")}:\cr
      \code{signature(from = "nfmpz", to = "fmpz")}:\cr
      returns \code{new("fmpz", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "fmpz", to = "nfmpz")}:\cr
      returns an object of class \code{nfmpz} whose data part is
      \code{as.vector(from, "integer")}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
showClass("nfmpz")
}
