\name{fmpz-class}
\alias{fmpz-class}
\alias{fmpz}
\alias{.fmpz}
\alias{!,fmpz-method}
\alias{+,fmpz,missing-method}
\alias{-,fmpz,missing-method}
\alias{Complex,fmpz-method}
\alias{Math,fmpz-method}
\alias{Math2,fmpz-method}
\alias{Ops,ANY,fmpz-method}
\alias{Ops,fmpz,ANY-method}
\alias{Ops,fmpz,ulong-method}
\alias{Ops,fmpz,slong-method}
\alias{Ops,fmpz,fmpz-method}
\alias{Ops,fmpz,fmpq-method}
\alias{Ops,fmpz,mag-method}
\alias{Ops,fmpz,arf-method}
\alias{Ops,fmpz,acf-method}
\alias{Ops,fmpz,arb-method}
\alias{Ops,fmpz,acb-method}
\alias{Summary,fmpz-method}
\alias{anyNA,fmpz-method}
\alias{as.vector,fmpz-method}
\alias{coerce,ANY,fmpz-method}
\alias{initialize,fmpz-method}
\alias{is.finite,fmpz-method}
\alias{is.infinite,fmpz-method}
\alias{is.na,fmpz-method}
\alias{is.nan,fmpz-method}
\alias{is.unsorted,fmpz-method}
\alias{mean,fmpz-method}
\docType{class}
\title{Arbitrary Precision Signed Integers}
\description{
  Class \code{fmpz} extends virtual class \code{\linkS4class{flint}}.
  It represents vectors of arbitrary precision signed integers.  There
  is no representation for \R{}'s missing value
  \code{\link{NA_integer_}}.
}
\usage{
## The class generator function:
.fmpz(\dots)

## Mode 1: initialize with zeros
## .fmpz(length = 0L)
##
## Mode 2: initialize with vector
## .fmpz(length = length(x), x)
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator function returns \code{\link{new}("fmpz", \dots)}.
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
      \code{signature(x = "fmpz")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math}}{
      \code{signature(x = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "fmpz")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "fmpz")}:\cr
      \code{signature(e1 = "fmpz", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "fmpz")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{fmpz} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{anyNA}}{
      \code{signature(x = "fmpz")}:\cr
      returns \code{FALSE}, as \code{fmpz} has no representation for
      \code{NaN}.}
    \item{\code{as.vector}}{
      \code{signature(x = "fmpz")}:\cr
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
      \code{signature(from = "ANY", to = "fmpz")}:\cr
      returns \code{.fmpz(x = from)}.  An error is signaled if the class
      or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
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
      \code{signature(.Object = "fmpz", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if elements of \code{x} are \code{NaN},
      \code{-Inf}, or \code{Inf}.}
    \item{\code{is.finite}}{
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.infinite}, \code{is.na}, \code{is.nan}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{mean}}{
      \code{signature(x = "fmpz")}:\cr
      returns the arithmetic mean.  An error is signaled if the argument
      length is 0, because the return type is \code{\linkS4class{fmpq}}
      which cannot represent the result of division by 0.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
showMethods(classes = "fmpz")
}
\keyword{classes}
