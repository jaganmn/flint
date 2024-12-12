\name{acb-class}
\alias{acb-class}
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
\alias{initialize,acb-method}
\alias{log,acb-method}
\alias{Real}
\alias{Imag}
\title{Class of Complex Numbers with Error Bounds}
\docType{class}
\description{
  Class \code{acb} extends virtual class \code{flint}.  It represents
  vectors of complex numbers with error bounds on the real and imaginary
  parts.  Elements are specified by two pairs of mixed format
  floating-point numbers: an \code{\linkS4class{arb}} real part and an
  \code{\linkS4class{arb}} imaginary part, each specified by an
  \code{\linkS4class{arf}} midpoint and a \code{\linkS4class{mag}}
  radius.
}
\usage{
Real(z)
Imag(z)
}
\arguments{
  \item{z}{an object of class \code{acb}.}
}
\value{
  \code{Real(z)} and \code{Imag(z)} are \code{\linkS4class{arb}}
  vectors whose elements are the real and imaginary parts of \code{z}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  Due to constraints imposed by generic functions, methods typically
  do \emph{not} provide a formal argument \code{prec} allowing for a
  precision to be indicated in the function call.  Such methods use
  the current default precision set by \code{\link{flintPrec}}.
  \describe{
    \item{\code{+}}{
      \code{signature(e1 = "acb", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "acb", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "acb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{Math}}{
      \code{signature(x = "acb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions
      \code{floor}, \code{ceiling}, \code{trunc}, \code{cummin},
      \code{cummax} are not implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "acb")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "acb")}:\cr
      \code{signature(e1 = "acb", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "acb")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{acb} vector of length 1 or 2 (\code{sum}, \code{prod}).
      Member functions \code{min}, \code{max}, \code{range} are not
      implemented.}
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
    \item{\code{log}}{
      \code{signature(x = "acb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/acb.html}
}
\examples{
showClass("acb")
showMethods(classes = "acb")
}
