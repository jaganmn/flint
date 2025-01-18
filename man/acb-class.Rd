\name{acb-class}
\alias{acb-class}
\alias{acb}
\alias{.acb}
\alias{!,acb-method}
\alias{+,acb,missing-method}
\alias{-,acb,missing-method}
\alias{Complex,acb-method}
\alias{Math,acb-method}
\alias{Math2,acb-method}
\alias{Ops,ANY,acb-method}
\alias{Ops,acb,ANY-method}
\alias{Ops,acb,ulong-method}
\alias{Ops,acb,slong-method}
\alias{Ops,acb,fmpz-method}
\alias{Ops,acb,fmpq-method}
\alias{Ops,acb,mag-method}
\alias{Ops,acb,arf-method}
\alias{Ops,acb,acf-method}
\alias{Ops,acb,arb-method}
\alias{Ops,acb,acb-method}
\alias{Summary,acb-method}
\alias{anyNA,acb-method}
\alias{as.vector,acb-method}
\alias{coerce,ANY,acb-method}
\alias{initialize,acb-method}
\alias{is.finite,acb-method}
\alias{is.infinite,acb-method}
\alias{is.na,acb-method}
\alias{is.nan,acb-method}
\alias{is.unsorted,acb-method}
\alias{log,acb-method}
\alias{mean,acb-method}
\alias{xtfrm,acb-method}
\docType{class}
\title{Arbitrary Precision Floating-Point Complex Numbers with Error Bounds}
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
## The class generator function:
.acb(\dots)

## Mode 1: initialize with zeros
## .acb(length = 0L)
##
## Mode 2: initialize with vector
## .acb(length = length(x), x)
##
## Mode 3: initialize by parts
## .acb(length = max0(length(real), length(imag)), real, imag)
##
## where max0 <- function(m, n) if (min(m, n)) max(m, n) else 0L
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator function returns \code{\link{new}("acb", \dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}, \code{names}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  Due to constraints imposed by generic functions, methods typically do
  \emph{not} provide a formal argument \code{prec} allowing for a
  precision to be indicated in the function call.  Such methods use the
  current default precision set by \code{\link{flintPrec}}.
  \describe{
    \item{\code{!}}{
      \code{signature(x = "acb")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
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
    \item{\code{anyNA}}{
      \code{signature(x = "acb")}:\cr
      returns \code{TRUE} if any element of \code{x} has real or
      imaginary part with midpoint \code{NaN}, \code{FALSE} otherwise.}
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
      introduced with a warning.  Coercion to types \code{"character"},
      \code{"symbol"} (synonym \code{"name"}), \code{"pairlist"},
      \code{"list"}, and \code{"expression"}, which are not
      \dQuote{number-like}, is handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "acb")}:\cr
      returns \code{.acb(x = from)}.  An error is signaled if the class
      or type of \code{from} is not supported by the method for
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
      \code{signature(.Object = "acb", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", length = "\var{numeric}", real = "\var{atomic|flint}", imag = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      real and imaginary parts are initialized separately with values
      from \code{real} and \code{imag}, which are coerced to class
      \code{\linkS4class{arb}} and recycled as necessary.}
    \item{\code{is.finite}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      do not have real or imaginary part with midpoint \code{NaN},
      \code{-Inf}, or \code{Inf} or radius \code{Inf}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part with midpoint \code{-Inf} or
      \code{Inf} or radius \code{Inf}.}
    \item{\code{is.na}, \code{is.nan}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part with midpoint \code{NaN}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "acb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}; see \code{xtfrm} below.}
    \item{\code{log}}{
      \code{signature(x = "acb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{mean}}{
      \code{signature(x = "acb")}:\cr
      returns the arithmetic mean.}
    \item{\code{xtfrm}}{
      \code{signature(x = "acb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}: \code{a <= b || b <= a} is is not
      \code{TRUE} for all finite \code{a} and \code{b} of class
      \code{arb}.  Thus, direct sorting of \code{acb}, which is based
      on \code{arb}, is not supported.  Users wanting to order the
      \emph{midpoints} of the real and imaginary parts should operate on
      \code{\link{Mid}(\link{Real}(x))} and
      \code{\link{Mid}(\link{Imag}(x))}.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.  Generic functions
  \code{\link{Real}} and \code{\link{Imag}} and their replacement forms
  for getting and setting real and imaginary parts.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/acb.html}

  Johansson, F. (2017).
  Arb: efficient arbitrary-precision midpoint-radius interval
  arithmetic.
  \emph{IEEE Transactions on Computers},
  \emph{66}(8), 1281-1292.
  \doi{10.1109/TC.2017.2690633}
}
\examples{
showClass("acb")
showMethods(classes = "acb")
}
\keyword{classes}
