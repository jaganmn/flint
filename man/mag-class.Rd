\name{mag-class}
\alias{mag-class}
\alias{nmag-class}
\alias{as.vector,mag-method}
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
  \code{mag} inherits slots \code{length} and \code{x} from
  \code{\linkS4class{flint}}.

  \code{nmag} possesses a pseudo-slot \code{.Data} (called the
  \dQuote{data part}) whose apparent value is the same double vector
  with S4 bit unset and attributes removed.
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "mag")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      is in the direction of \code{Inf}, to nearest except around the
      boundary of the range.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", length = "numeric")}:\cr
      returns \code{.Object} after initializing its \code{length} and
      \code{x} slots.  The \code{length} slot is initialized to a
      representation of the \code{length} argument, which after
      truncation towards zero must be a non-negative integer less than
      \eqn{2^{64}}.  The \code{x} slot \dQuote{address} is changed to
      the address of an array of the corresponding \proglang{C} type,
      which is newly allocated and initialized entirely with zeros.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", x = "numeric")}:\cr
      as above, except that the \code{length} slot is initialized to a
      representation of \code{length(x)} and the newly allocated array
      is initialized with values from \code{abs(x)} rounded away from
      zero.  An error is signaled if any \code{x[i]} is \code{NaN}.}
    \item{\code{coerce}}{
      \code{signature(from = "nmag", to = "mag")}:\cr
      returns \code{new("mag", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "mag", to = "nmag")}:\cr
      returns an object of class \code{nmag} whose data part is
      \code{as.vector(from, "double")}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/mag.html}
}
\examples{
showClass("mag")
showClass("nmag")
}
