\name{fmpz-class}
\alias{fmpz-class}
\alias{nfmpz-class}
\alias{+,fmpz,missing-method}
\alias{-,fmpz,missing-method}
\alias{Complex,fmpz-method}
\alias{Math,fmpz-method}
\alias{Math2,fmpz-method}
\alias{Ops,ANY,fmpz-method}
\alias{Ops,fmpz,ANY-method}
\alias{Ops,fmpz,slong-method}
\alias{Ops,fmpz,ulong-method}
\alias{Ops,fmpz,fmpz-method}
\alias{Ops,fmpz,fmpq-method}
\alias{Ops,fmpz,arf-method}
\alias{Ops,fmpz,mag-method}
\alias{Ops,fmpz,arb-method}
\alias{Ops,fmpz,acb-method}
\alias{Summary,fmpz-method}
\alias{as.vector,fmpz-method}
\alias{as.vector,fmpz-method}
\alias{as.vector,fmpz-method}
\alias{coerce,ANY,fmpz-method}
\alias{coerce,nfmpz,fmpz-method}
\alias{coerce,fmpz,nfmpz-method}
\alias{initialize,fmpz-method}
\title{Class of Vectors of Arbitrary Precision Integers}
\docType{class}
\description{
  Class \code{fmpz} extends virtual class \code{flint}.  It represents
  vectors of arbitrary precision integers.

  Class \code{nfmpz} extends virtual class \code{\linkS4class{nflint}}
  and basic vector class \code{\linkS4class{integer}}.  It is the
  \dQuote{native} analogue of \code{fmpz}.
}
\section{Slots}{
  \code{fmpz}:
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{nfmpz}:
  \describe{
    \item{\code{.Data}}{
      a pseudo-slot whose \emph{apparent} value is the
      \dQuote{data part} of the object: a copy of the object with S4 bit
      unset and attributes removed.  \code{NA} represents integers not
      in \eqn{(-2^{31}, 2^{31})}, the range of \R{}'s integer type.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{+}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "fmpz")}:\cr
      a placeholder signaling a \dQuote{not yet implemented} error.}
    \item{\code{Math}}{
      \code{signature(x = "fmpz")}:\cr
      mathematical functions of one argument, where supported
      \emph{without} promotion to a floating-point type.}
    \item{\code{Math2}}{
      \code{signature(x = "fmpz")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "fmpz")}:\cr
      \code{signature(e1 = "fmpz", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "fmpz")}:\cr
      univariate summary statistics, returning a logical vector of
      length 1 (\code{any}, \code{all}) or an \code{fmpz} vector of
      length 1 or 2 (\code{sum}, \code{prod}, \code{min}, \code{max},
      \code{range}).}
    \item{\code{as.vector}}{
      \code{signature(x = "fmpz")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero,
      unless the element exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "fmpz")}:\cr
      \code{signature(from = "nfmpz", to = "fmpz")}:\cr
      returns \code{new("fmpz", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "fmpz", to = "nfmpz")}:\cr
      returns an object of class \code{nfmpz} whose data part is
      \code{as.vector(from, "integer")}.}
    \item{\code{format}}{
      \code{signature(x = "fmpz")}:\cr
      returns a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if any \code{x[i]} is \code{NaN}, \code{-Inf},
      \code{Inf}, or subnormal.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
showMethods(classes = "fmpz")
showClass("nfmpz")
showMethods(classes = "nfmpz")
}
