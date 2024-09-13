\name{arb-class}
\alias{arb-class}
\alias{narb-class}
\alias{as.vector,arb-method}
\alias{coerce,narb,arb-method}
\alias{coerce,arb,narb-method}
\alias{initialize,arb-method}
\alias{length,narb-method}
\title{Class of Real Numbers with Error Bounds}
\docType{class}
\description{
  Class \code{arb} extends virtual class \code{flint}.  It represents
  vectors of real numbers with error bounds.  Elements are specified by
  a pair of mixed format floating-point numbers: an
  \code{\linkS4class{arf}} midpoint and a \code{\linkS4class{mag}}
  radius.

  Class \code{narb} extends virtual class \code{\linkS4class{nflint}}.
  It is the \dQuote{native} analogue of \code{arb}.
}
\section{Slots}{
  \code{arb} inherits slots \code{length} and \code{x} from
  \code{\linkS4class{flint}}.

  \code{narb} has a slot \code{mid} of class \code{\linkS4class{arf}}
  and a slot \code{rad} of class \code{\linkS4class{mag}}.  They are
  double vectors of equal length listing midpoints and radii.
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "arb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting the midpoints of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero,
      unless the midpoint exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "numeric")}:\cr
      returns \code{.Object} after initializing its \code{length} and
      \code{x} slots.  The \code{length} slot is initialized to a
      representation of the \code{length} argument, which after
      truncation towards zero must be a non-negative integer less than
      \eqn{2^{64}}.  The \code{x} slot \dQuote{address} is changed to
      the address of an array of the corresponding \proglang{C} type,
      which is newly allocated and initialized entirely with zeros
      (zero midpoint, zero radius).}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", x = "numeric")}:\cr
      as above, except that the \code{length} slot is initialized to a
      representation of \code{length(x)} and the newly allocated array
      is initialized with midpoints from \code{x} and radii equal to
      zero.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", mid = "numeric", rad = "numeric")}:\cr
      as above, except that the \code{length} slot is initialized to a
      representation of the maximum initializer length (0 if any length
      is zero) and the newly allocated array is initialized with
      midpoints from \code{mid} and radii from \code{rad}, both recycled
      as necessary.  An error is signaled if any \code{rad[i]} is
      \code{NaN}.}
    \item{\code{coerce}}{
      \code{signature(from = "narb", to = "arb")}:\cr
      returns \code{new("arb", mid = from@mid, rad = from@rad)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arb", to = "narb")}:\cr
      returns the analogous object of class \code{narb}.}
    \item{\code{length}}{
      \code{signature(x = "narb")}:\cr
      returns \code{length(x@mid)}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/arb.html}
}
\examples{
showClass("arb")
showClass("narb")
}
