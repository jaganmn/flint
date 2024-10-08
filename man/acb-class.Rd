\name{acb-class}
\alias{acb-class}
\alias{nacb-class}
\alias{Re,acb-method}
\alias{Im,acb-method}
\alias{as.vector,acb-method}
\alias{coerce,vector,acb-method}
\alias{coerce,nacb,acb-method}
\alias{coerce,acb,nacb-method}
\alias{initialize,acb-method}
\alias{length,nacb-method}
\title{Class of Complex Numbers with Error Bounds}
\docType{class}
\description{
  Class \code{acb} extends virtual class \code{flint}.  It represents
  vectors of complex numbers with error bounds on the real and imaginary
  parts.  Elements are specified by two pairs of mixed format
  floating-point numbers: for each of the real and imaginary parts, an
  \code{\linkS4class{arf}} midpoint and a \code{\linkS4class{mag}}
  radius.

  Class \code{nacb} extends virtual class \code{\linkS4class{nflint}}.
  It is the \dQuote{native} analogue of \code{acb}.
}
\section{Slots}{
  \code{acb}:
  \describe{
    \item{\code{length}, \code{x}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{nacb}:
  \describe{
    \item{\code{real}}{
      an \code{\linkS4class{narb}} vector listing real parts with error
      bounds.}
    \item{\code{imag}}{
      an \code{\linkS4class{narb}} vector listing imaginary parts with
      error bounds.}
  }
  The lengths of \code{real} and \code{imag} must be equal.
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "acb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a complex
      vector containing the result of converting the midpoints of
      the real and imaginary parts of \code{x} to the range of double,
      rounding if the value is not exactly representable in double
      precision.  The rounding mode is to the nearest representable
      number (with precedence to even significands in case of ties),
      unless a midpoint exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", length = "numeric")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero midpoint, zero radius
      in the real and imaginary parts).  \code{.Object} is not copied,
      so all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", x = "complex")}:\cr
      \code{signature(.Object = "acb", x = "numeric")}:\cr
      as above, except that the new \code{x} slot points to an array
      initialized with midpoints from \code{x} and radii equal to zero.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", real.mid = "numeric", real.rad = "numeric", imag.mid = "numeric", imag.rad = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with midpoints from \code{*.mid} and radii from
      \code{*.rad}, all recycled as necessary.  The result length is the
      maximum initializer length or zero if any initializer has length
      zero.  An error is signaled if any \code{*.rad[i]} is \code{NaN}.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "acb")}:\cr
      returns
      \code{new("acb", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "nacb", to = "acb")}:\cr
      returns
      \code{new("acb", real.mid = from@real@mid, real.rad = from@real@rad, imag.mid = from@imag@mid, imag.rad = from@imag@rad)}.}
    \item{\code{coerce}}{
      \code{signature(from = "acb", to = "nacb")}:\cr
      returns the analogous object of class \code{nacb}.}
    \item{\code{length}}{
      \code{signature(x = "nacb")}:\cr
      returns \code{length(x@real@mid)}.}
    \item{\code{Re}, \code{Im}}{
      \code{signature(z = "acb")}:\cr
      return an \code{\linkS4class{arb}} vector containing the real and
      imaginary parts, respectively.}
    \item{\code{format}}{
      \code{signature(x = "acb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)+(m +/- r)i"} and scientific format for
      each \code{m} and \code{r}.  Optional arguments control the
      output; see \code{\link{format-methods}}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/acb.html}
}
\examples{
showClass("acb")
showClass("nacb")
}
