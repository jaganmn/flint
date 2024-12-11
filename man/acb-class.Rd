\name{acb-class}
\alias{acb-class}
\alias{nacb-class}
\alias{+,acb,missing-method}
\alias{-,acb,missing-method}
\alias{Complex,acb-method}
\alias{Math,acb-method}
\alias{Math2,acb-method}
\alias{Ops,ANY,acb-method}
\alias{Ops,acb,ANY-method}
\alias{Ops,acb,slong-method}
\alias{Ops,acb,ulong-method}
\alias{Ops,acb,fmpz-method}
\alias{Ops,acb,fmpq-method}
\alias{Ops,acb,arf-method}
\alias{Ops,acb,mag-method}
\alias{Ops,acb,arb-method}
\alias{Ops,acb,acb-method}
\alias{Summary,acb-method}
\alias{as.vector,acb-method}
\alias{coerce,ANY,acb-method}
\alias{coerce,nacb,acb-method}
\alias{coerce,acb,nacb-method}
\alias{initialize,acb-method}
\alias{length,nacb-method}
\alias{log,acb-method}
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
    \item{\code{.xData}}{
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
      vector containing the result of converting the midpoints of the
      real and imaginary parts of \code{x} to the range of double,
      rounding if the value is not exactly representable in double
      precision.  The rounding mode is to the nearest representable
      number (with precedence to even significands in case of ties),
      unless a midpoint exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "acb")}:\cr
      returns \code{new("acb", x = from)}.  An error is signaled if the
      class or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
    \item{\code{coerce}}{
      \code{signature(from = "nacb", to = "acb")}:\cr
      returns \code{new("acb", real = from@real, imag = from@imag)}.}
    \item{\code{coerce}}{
      \code{signature(from = "acb", to = "nacb")}:\cr
      returns the analogous object of class \code{nacb}.}
    \item{\code{format}}{
      \code{signature(x = "acb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)+(m +/- r)i"} and scientific format for
      each \code{m} and \code{r}.  Optional arguments control the
      output; see \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero midpoint, zero radius in the
      real and imaginary parts).  \code{.Object} is not copied, so all
      references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", real = "\var{atomic|flint}", imag = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      real and imaginary parts are initialized separately with values
      from \code{real} and \code{imag}, which are coerced to class
      \code{\linkS4class{arb}} and recycled as necessary.}
    \item{\code{length}}{
      \code{signature(x = "nacb")}:\cr
      returns \code{length(x@real@mid)}.}
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
