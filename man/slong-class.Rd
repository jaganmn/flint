\name{slong-class}
\alias{slong-class}
\alias{ulong-class}
\alias{initialize,slong,numeric-method}
\alias{initialize,ulong,numeric-method}
\alias{coerce,numeric,slong-method}
\alias{coerce,numeric,ulong-method}
\alias{coerce,slong,integer-method}
\alias{coerce,ulong,integer-method}
\alias{coerce,slong,double-method}
\alias{coerce,ulong,double-method}
\alias{flintBits}
\title{Classes of Vectors of Signed or Unsigned Integers}
\docType{class}
\description{
  Classes \code{slong} and \code{ulong} represent vectors of signed
  and unsigned integers.  The integer size is 32 or 64 bits, depending
  on the configuration of the linked \abbr{FLINT} library.  The number
  of bits is given by \code{flintBits()}.
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
      \code{signature(.Object = "slong", x = "numeric")}:\cr
      \code{signature(.Object = "ulong", x = "numeric")}:\cr
      return \code{.Object} with \code{length} slot set to a
      representation of \code{length(x)} and \code{x} slot set to the
      address of an initialized array of \code{slong} or \code{ulong}.
      The array is initialized with values from \code{x}, which is
      truncated towards zero.  An error is signaled if any \code{x[i]}
      is not in the range of the array type, in particular if any
      \code{x[i]} is \code{NaN}.  The range is
      \eqn{(-2^{n-1}-1, 2^{n-1})} for \code{slong} and \eqn{(-1,2^{n})}
      for \code{ulong}, where \eqn{n} is the value of \code{flintBits()}.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "slong")}:\cr
      \code{signature(from = "numeric", to = "ulong")}:\cr
      equivalent to \code{new("slong", x = from)} and
      \code{new("ulong", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "slong", to = "integer")}:\cr
      \code{signature(from = "slong", to = "double")}:\cr
      \code{signature(from = "ulong", to = "integer")}:\cr
      \code{signature(from = "ulong", to = "double")}:\cr
      return an integer or double vector containing the result of
      converting the original integer type to the range of the target
      type.  The behaviour when the integer is not exactly
      representable in the target type is to introduce \code{NA} with
      a warning (\code{"integer"}) or to round without a warning
      (\code{"double"}).  The rounding mode is implementation-defined.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} types:
  \url{https://flintlib.org/doc/flint.html}
}
\examples{
showClass("slong")
showClass("ulong")
flintBits()

new("slong", x = -1L)
new("ulong", x = c(-0.5, 0x1p+32 - 0.5))
}
