\name{fmpq-class}
\alias{fmpq-class}
\alias{fmpq}
\alias{.fmpq}
\alias{!,fmpq-method}
\alias{+,fmpq,missing-method}
\alias{-,fmpq,missing-method}
\alias{Complex,fmpq-method}
\alias{Math,fmpq-method}
\alias{Math2,fmpq-method}
\alias{Ops,ANY,fmpq-method}
\alias{Ops,fmpq,ANY-method}
\alias{Ops,fmpq,ulong-method}
\alias{Ops,fmpq,slong-method}
\alias{Ops,fmpq,fmpz-method}
\alias{Ops,fmpq,fmpq-method}
\alias{Ops,fmpq,mag-method}
\alias{Ops,fmpq,arf-method}
\alias{Ops,fmpq,acf-method}
\alias{Ops,fmpq,arb-method}
\alias{Ops,fmpq,acb-method}
\alias{Summary,fmpq-method}
\alias{anyNA,fmpq-method}
\alias{as.vector,fmpq-method}
\alias{coerce,ANY,fmpq-method}
\alias{initialize,fmpq-method}
\alias{is.finite,fmpq-method}
\alias{is.infinite,fmpq-method}
\alias{is.na,fmpq-method}
\alias{is.nan,fmpq-method}
\alias{is.unsorted,fmpq-method}
\alias{mean,fmpq-method}
\docType{class}
\title{Arbitrary Precision Rational Numbers}
\description{
  Class \code{fmpq} extends virtual class \code{\linkS4class{flint}}.
  It represents vectors of arbitrary precision rational numbers.
  Elements are specified by a pair of arbitrary precision signed
  integers: a numerator and a positive, coprime denominator.  There is
  no representation for \R{}'s missing value \code{\link{NA_integer_}}.
}
\usage{
## The class generator function:
.fmpq(\dots)

## Mode 1: initialize with zeros
## .fmpq(length = 0L)
##
## Mode 2: initialize with vector
## .fmpq(length = length(x), x)
##
## Mode 3: initialize by parts
## .fmpq(length = max0(length(num), length(den)), num, den)
##
## where max0 <- function(m, n) if (min(m, n)) max(m, n) else 0L
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator function returns \code{\link{new}("fmpq", \dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}, \code{names}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{!}}{
      \code{signature(x = "fmpq")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
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
    \item{\code{anyNA}}{
      \code{signature(x = "fmpq")}:\cr
      returns \code{FALSE}, as \code{fmpq} has no representation for
      \code{NaN}.}
    \item{\code{as.vector}}{
      \code{signature(x = "fmpq")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero,
      unless the element exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.  Coercion to types \code{"character"},
      \code{"symbol"} (synonym \code{"name"}), \code{"pairlist"},
      \code{"list"}, and \code{"expression"}, which are not
      \dQuote{number-like}, is handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "fmpq")}:\cr
      returns \code{.fmpq(x = from)}.  An error is signaled if the class
      or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
    \item{\code{format}}{
      \code{signature(x = "fmpq")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"p/q"}.  Optional arguments control the output; see
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
      \code{signature(.Object = "fmpq", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.  An error is signaled if
      elements of \code{x} are \code{NaN}, \code{-Inf}, or \code{Inf}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpq", length = "\var{numeric}", num = "\var{atomic|flint}", den = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      numerators and denominators are initialized separately with values
      from \code{num} and \code{den}, which are coerced to class
      \code{\linkS4class{fmpz}} and recycled as necessary.  An error is
      signaled if elements of \code{num} or \code{den} are \code{NaN},
      \code{-Inf}, or \code{Inf} or elements of \code{den} are zero.}
    \item{\code{is.finite}}{
      \code{signature(x = "fmpq")}:\cr
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{fmpq} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.infinite}, \code{is.na}, \code{is.nan}}{
      \code{signature(x = "fmpq")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{fmpq} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "fmpq")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{mean}}{
      \code{signature(x = "fmpq")}:\cr
      returns the arithmetic mean.  An error is signaled if the argument
      length is 0, because the return type is \code{fmpq} which cannot
      represent the result of division by 0.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.  Generic functions
  \code{\link{Num}} and \code{\link{Den}} and their replacement forms
  for getting and setting numerators and denominators.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpq.html}
}
\examples{
showClass("fmpq")
showMethods(classes = "fmpq")
}
\keyword{classes}
