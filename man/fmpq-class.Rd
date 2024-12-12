\name{fmpq-class}
\alias{fmpq-class}
\alias{+,fmpq,missing-method}
\alias{-,fmpq,missing-method}
\alias{^,fmpq,fmpq-method}
\alias{^,fmpq,fmpz-method}
\alias{Complex,fmpq-method}
\alias{Math,fmpq-method}
\alias{Math2,fmpq-method}
\alias{Ops,ANY,fmpq-method}
\alias{Ops,fmpq,ANY-method}
\alias{Ops,fmpq,slong-method}
\alias{Ops,fmpq,ulong-method}
\alias{Ops,fmpq,fmpz-method}
\alias{Ops,fmpq,fmpq-method}
\alias{Ops,fmpq,arf-method}
\alias{Ops,fmpq,mag-method}
\alias{Ops,fmpq,arb-method}
\alias{Ops,fmpq,acb-method}
\alias{Summary,fmpq-method}
\alias{as.vector,fmpq-method}
\alias{coerce,ANY,fmpq-method}
\alias{initialize,fmpq-method}
\title{Class of Vectors of Rational Numbers}
\docType{class}
\description{
  Class \code{fmpq} extends virtual class \code{\linkS4class{flint}}.
  It represents vectors of rational numbers.  Elements are specified by
  a pair of arbitrary precision integers a numerator and a (typically
  positive, coprime) denominator.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{+}}{
      \code{signature(e1 = "fmpq", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "fmpq", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "fmpq")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math}}{
      \code{signature(x = "fmpq")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "fmpq")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "fmpq")}:\cr
      \code{signature(e1 = "fmpq", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "fmpq")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{fmpq} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{as.vector}}{
      \code{signature(x = "fmpq")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero,
      unless the element exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "fmpq")}:\cr
      returns \code{new("fmpq", x = from)}.}
    \item{\code{format}}{
      \code{signature(x = "fmpq")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(p/q)"}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpq", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero numerator, unit
      denominator).  \code{.Object} is not copied, so all references are
      affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpq", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.  An error is signaled if
      any \code{x[i]} is \code{NaN}, \code{-Inf}, or \code{Inf}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpq", num = "\var{atomic|flint}", den = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      numerators and denominators are initialized independently with
      values from \code{num} and \code{den}, which are coerced to class
      \code{\linkS4class{fmpz}} and recycled as necessary.  An error is
      signaled if any \code{num[i]} or \code{den[i]} is \code{NaN},
      \code{-Inf}, \code{Inf}, or subnormal or if any \code{den[i]} is
      zero.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpq.html}
}
\examples{
showClass("fmpq")
showMethods(classes = "fmpq")
}
