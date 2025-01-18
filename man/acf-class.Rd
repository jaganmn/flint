\name{acf-class}
\alias{acf-class}
\alias{acf}
\alias{.acf}
\alias{!,acf-method}
\alias{+,acf,missing-method}
\alias{-,acf,missing-method}
\alias{Complex,acf-method}
\alias{Math,acf-method}
\alias{Math2,acf-method}
\alias{Ops,ANY,acf-method}
\alias{Ops,acf,ANY-method}
\alias{Ops,acf,ulong-method}
\alias{Ops,acf,slong-method}
\alias{Ops,acf,fmpz-method}
\alias{Ops,acf,fmpq-method}
\alias{Ops,acf,mag-method}
\alias{Ops,acf,arf-method}
\alias{Ops,acf,acf-method}
\alias{Ops,acf,arb-method}
\alias{Ops,acf,acb-method}
\alias{Summary,acf-method}
\alias{anyNA,acf-method}
\alias{as.vector,acf-method}
\alias{coerce,ANY,acf-method}
\alias{initialize,acf-method}
\alias{is.finite,acf-method}
\alias{is.infinite,acf-method}
\alias{is.na,acf-method}
\alias{is.nan,acf-method}
\alias{is.unsorted,acf-method}
\alias{mean,acf-method}
\alias{xtfrm,acf-method}
\docType{class}
\title{Arbitrary Precision Floating-Point Complex Numbers}
\description{
  Class \code{acf} extends virtual class \code{\linkS4class{flint}}.  It
  represents vectors of arbitrary precision floating-point complex
  numbers.  Elements have real and imaginary parts, each with arbitrary
  precision significand and exponent.  The underlying \proglang{C} type
  can represent \code{NaN}, \code{-Inf}, and \code{Inf} real and
  imaginary parts.

  Note that package \pkg{stats} exports a function
  \code{\link[=stats]{acf}}, referring to autocovariance and
  autocorrelation functions of time series.  It returns objects of
  \emph{informal} S3 class \code{acf}, for which a small number of
  \emph{informal} S3 methods are registered.  The \emph{formal} S4 class
  and methods documented here are entirely unrelated.
}
\usage{
## The class generator function:
.acf(\dots)

## Mode 1: initialize with zeros
## .acf(length = 0L)
##
## Mode 2: initialize with vector
## .acf(length = length(x), x)
##
## Mode 3: initialize by parts
## .acf(length = max0(length(real), length(imag)), real, imag)
##
## where max0 <- function(m, n) if (min(m, n)) max(m, n) else 0L
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator function returns \code{\link{new}("acf", \dots)}.
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
      \code{signature(x = "acf")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "acf", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "acf", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "acf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{Math}}{
      \code{signature(x = "acf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions
      \code{floor}, \code{ceiling}, \code{trunc}, \code{cummin},
      \code{cummax} are not implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "acf")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "acf")}:\cr
      \code{signature(e1 = "acf", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "acf")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{acf} vector of length 1 or 2 (\code{sum}, \code{prod}).
      Member functions \code{min}, \code{max}, \code{range} are not
      implemented.}
    \item{\code{anyNA}}{
      \code{signature(x = "acf")}:\cr
      returns \code{TRUE} if any element of \code{x} has real or
      imaginary part \code{NaN}, \code{FALSE} otherwise.}
    \item{\code{as.vector}}{
      \code{signature(x = "acf")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a complex
      vector containing the result of converting the real and imaginary
      parts of \code{x} to the range of double, rounding if the value is
      not exactly representable in double precision.  The rounding mode
      is to the nearest representable number (with precedence to even
      significands in case of ties), unless parts exceed
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.  Coercion
      to types \code{"character"}, \code{"symbol"} (synonym
      \code{"name"}), \code{"pairlist"}, \code{"list"}, and
      \code{"expression"}, which are not \dQuote{number-like}, is
      handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "acf")}:\cr
      returns \code{.acf(x = from)}.  An error is signaled if the class
      or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
    \item{\code{format}}{
      \code{signature(x = "acf")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"a+bi"} and scientific format for each \code{a} and
      \code{b}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acf", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acf", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "acf", length = "\var{numeric}", real = "\var{atomic|flint}", imag = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      real and imaginary parts are initialized separately with values
      from \code{real} and \code{imag}, which are coerced to class
      \code{\linkS4class{arf}} and recycled as necessary.}
    \item{\code{is.finite}}{
      \code{signature(x = "acf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      do not have real or imaginary part \code{NaN}, \code{-Inf}, or
      \code{Inf}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "acf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part \code{-Inf} or \code{Inf}.}
    \item{\code{is.na}, \code{is.nan}}{
      \code{signature(x = "acf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part \code{NaN}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "acf")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}) by real part then by
      imaginary part.}
    \item{\code{mean}}{
      \code{signature(x = "acf")}:\cr
      returns the arithmetic mean.}
    \item{\code{xtfrm}}{
      \code{signature(x = "acf")}:\cr
      returns a numeric vector that sorts in the same order as \code{x}.
      The permutation \code{order(xtfrm(x), \dots)} orders \code{x}
      first by its real part then by its imaginary part, with the caveat
      that all \code{a+NaNi} and \code{NaN+bi} have equal precedence
      (for compatibility with \pkg{base}).}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.  Generic functions
  \code{\link{Real}} and \code{\link{Imag}} and their replacement forms
  for getting and setting real and imaginary parts.
}
\examples{
showClass("acf")
showMethods(classes = "acf")
}
\keyword{classes}
