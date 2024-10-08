\name{mag-class}
\alias{mag-class}
\alias{nmag-class}
\alias{as.vector,mag-method}
\alias{coerce,vector,mag-method}
\alias{coerce,nmag,mag-method}
\alias{coerce,mag,nmag-method}
\alias{initialize,mag-method}
\title{Class of Vectors of Magnitude (Error) Bounds}
\docType{class}
\description{
  Class \code{mag} extends virtual class \code{flint}.  It represents
  vectors of unsigned floating-point numbers with a fixed precision
  mantissa and an arbitary precision exponent.  In typical usage,
  \code{mag} objects store magnitude (or error) bounds on real numbers.
  The value can be positive infinity but never not-a-number.

  Class \code{nmag} extends virtual class \code{\linkS4class{nflint}}
  and basic vector class \code{\linkS4class{double}}.  It is the
  \dQuote{native} analogue of \code{mag}.
}
\section{Slots}{
  \code{mag}:
  \describe{
    \item{\code{length}, \code{x}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{nmag}:
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
      \code{signature(x = "mag")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding in the direction of
      \code{Inf}, not always to nearest.  For a precise description of
      the conversion, consult the definition of function
      \code{mag_set_d} in the \acronym{FLINT} sources.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", length = "numeric")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", x = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{abs(x)} rounded in the
      direction of \code{Inf}, not always to nearest.  An error is
      signaled if any \code{x[i]} is \code{NaN}.  For a precise
      description of the conversion, consult the definition of function
      \code{mag_get_d} in the \acronym{FLINT} sources.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "mag")}:\cr
      \code{signature(from = "nmag", to = "mag")}:\cr
      returns \code{new("mag", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "mag", to = "nmag")}:\cr
      returns an object of class \code{nmag} whose data part is
      \code{as.vector(from, "double")}.}
    \item{\code{format}}{
      \code{signature(x = "mag")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/mag.html}
}
\examples{
showClass("mag")
showClass("nmag")
}
