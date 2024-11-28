\name{arb-class}
\alias{arb-class}
\alias{narb-class}
\alias{+,arb,missing-method}
\alias{-,arb,missing-method}
\alias{Complex,arb-method}
\alias{Math,arb-method}
\alias{Math2,arb-method}
\alias{Ops,arb,arb-method}
\alias{Summary,arb-method}
\alias{as.vector,arb-method}
\alias{coerce,vector,arb-method}
\alias{coerce,narb,arb-method}
\alias{coerce,arb,narb-method}
\alias{initialize,arb-method}
\alias{length,narb-method}
\alias{log,arb-method}
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
  \code{arb}:
  \describe{
    \item{\code{length}, \code{x}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{narb}:
  \describe{
    \item{\code{mid}}{
      an \code{\linkS4class{narf}} vector listing midpoints.}
    \item{\code{rad}}{
      an \code{\linkS4class{nmag}} vector listing radii.}
  }
  The lengths of \code{mid} and \code{rad} must be equal.
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "arb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting the midpoints of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number (with precedence to even
      significands in case of ties), unless a midpoint exceeds
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "numeric")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero midpoint, zero radius).
      \code{.Object} is not copied, so all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", x = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with midpoints from \code{x} and radii equal to zero.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", mid = "numeric", rad = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with midpoints from \code{mid} and radii from
      \code{rad}, both recycled as necessary.  The result length is the
      maximum initializer length or zero if any initializer has length
      zero.  An error is signaled if any \code{rad[i]} is \code{NaN}.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "arb")}:\cr
      returns \code{new("arb", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "narb", to = "arb")}:\cr
      returns \code{new("arb", mid = from@mid, rad = from@rad)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arb", to = "narb")}:\cr
      returns the analogous object of class \code{narb}.}
    \item{\code{length}}{
      \code{signature(x = "narb")}:\cr
      returns \code{length(x@mid)}.}
    \item{\code{format}}{
      \code{signature(x = "arb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)"} and scientific format for \code{m} and
      \code{r}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arb.html}
}
\examples{
showClass("arb")
showClass("narb")
}
