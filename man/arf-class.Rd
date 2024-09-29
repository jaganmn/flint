\name{arf-class}
\alias{arf-class}
\alias{narf-class}
\alias{as.vector,arf-method}
\alias{coerce,vector,arf-method}
\alias{coerce,narf,arf-method}
\alias{coerce,arf,narf-method}
\alias{initialize,arf-method}
\title{Class of Vectors of Arbitrary Precision Floating-Point Numbers}
\docType{class}
\description{
  Class \code{arf} extends virtual class \code{flint}.  It represents
  vectors of signed floating-point numbers with an arbitrary precision
  mantissa and an arbitrary precision exponent.

  Class \code{narf} extends virtual class \code{\linkS4class{nflint}}
  and basic vector class \code{\linkS4class{double}}.  It is the
  \dQuote{native} analogue of \code{arf}.
}
\section{Slots}{
  \code{arf}:
  \describe{
    \item{\code{length}, \code{x}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{narf}:
  \describe{
    \item{\code{.Data}}{
      a pseudo-slot whose \emph{apparent} value is the
      \dQuote{data part} of the object: a copy of the object with S4 bit
      unset and attributes removed.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "arf")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      is to the nearest representable number in the direction of zero,
      unless the element exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arf", length = "numeric")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arf", x = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "arf")}:\cr
      \code{signature(from = "narf", to = "arf")}:\cr
      returns \code{new("arf", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arf", to = "narf")}:\cr
      returns an object of class \code{narf} whose data part is
      \code{as.vector(from, "double")}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arf.html}
}
\examples{
showClass("arf")
showClass("narf")
}
