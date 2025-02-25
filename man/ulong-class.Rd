\name{ulong-class}
\alias{ulong-class}
\alias{slong-class}
\alias{ulong}
\alias{slong}
\alias{.ulong}
\alias{.slong}
\alias{!,ulong-method}
\alias{!,slong-method}
\alias{+,ulong,missing-method}
\alias{+,slong,missing-method}
\alias{-,ulong,missing-method}
\alias{-,slong,missing-method}
\alias{Complex,ulong-method}
\alias{Complex,slong-method}
\alias{Math,ulong-method}
\alias{Math,slong-method}
\alias{Math2,ulong-method}
\alias{Math2,slong-method}
\alias{Ops,ANY,ulong-method}
\alias{Ops,ANY,slong-method}
\alias{Ops,ulong,ANY-method}
\alias{Ops,slong,ANY-method}
\alias{Ops,ulong,slong-method}
\alias{Ops,slong,slong-method}
\alias{Ops,ulong,ulong-method}
\alias{Ops,slong,ulong-method}
\alias{Ops,ulong,fmpz-method}
\alias{Ops,slong,fmpz-method}
\alias{Ops,ulong,fmpq-method}
\alias{Ops,slong,fmpq-method}
\alias{Ops,ulong,mag-method}
\alias{Ops,slong,mag-method}
\alias{Ops,ulong,arf-method}
\alias{Ops,slong,arf-method}
\alias{Ops,ulong,acf-method}
\alias{Ops,slong,acf-method}
\alias{Ops,ulong,arb-method}
\alias{Ops,slong,arb-method}
\alias{Ops,ulong,acb-method}
\alias{Ops,slong,acb-method}
\alias{Summary,ulong-method}
\alias{Summary,slong-method}
\alias{anyNA,ulong-method}
\alias{anyNA,slong-method}
\alias{as.vector,ulong-method}
\alias{as.vector,slong-method}
\alias{coerce,ANY,ulong-method}
\alias{coerce,ANY,slong-method}
\alias{initialize,ulong-method}
\alias{initialize,slong-method}
\alias{is.finite,ulong-method}
\alias{is.finite,slong-method}
\alias{is.infinite,ulong-method}
\alias{is.infinite,slong-method}
\alias{is.na,ulong-method}
\alias{is.na,slong-method}
\alias{is.nan,ulong-method}
\alias{is.nan,slong-method}
\alias{is.unsorted,ulong-method}
\alias{is.unsorted,slong-method}
\alias{mean,ulong-method}
\alias{mean,slong-method}
\docType{class}
\title{Fixed Precision Unsigned and Signed Integers}
\description{
  Classes \code{ulong} and \code{slong} extend virtual class
  \code{\linkS4class{flint}}.  They represent vectors of fixed precision
  unsigned and signed integers, respectively.  The integer size is 32 or
  64 bits, depending on the ABI; see \code{\link{flintABI}}.  There is
  no representation for \R{}'s missing value \code{\link{NA_integer_}}.
}
\usage{
## The class generator functions:
.ulong(\dots)
.slong(\dots)

## Mode 1: initialize with zeros
## .ulong(length = 0L)
## .slong(length = 0L)
##
## Mode 2: initialize with vector
## .ulong(length = length(x), x)
## .slong(length = length(x), x)
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator functions return
  \code{\link{new}("ulong", \dots)} and
  \code{\link{new}("slong", \dots)}.
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
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "ulong", e2 = "missing")}:\cr
      \code{signature(e1 = "slong", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "ulong", e2 = "missing")}:\cr
      \code{signature(e1 = "slong", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "ulong")}:\cr
      \code{signature(z = "slong")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "ulong")}:\cr
      \code{signature(e1 = "ANY", e2 = "slong")}:\cr
      \code{signature(e1 = "ulong", e2 = "ANY")}:\cr
      \code{signature(e1 = "slong", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or a
      \code{ulong}, \code{slong}, or \code{\linkS4class{fmpz}} vector of
      length 1 or 2 (\code{sum}, \code{prod}, \code{min}, \code{max},
      \code{range}).}
    \item{\code{anyNA}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns \code{FALSE}, as \code{ulong} and \code{slong} have no
      representation for \code{NaN}.}
    \item{\code{as.vector}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero.
      Coercion to types \code{"character"}, \code{"symbol"} (synonym
      \code{"name"}), \code{"pairlist"}, \code{"list"}, and
      \code{"expression"}, which are not \dQuote{number-like}, is
      handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "ulong")}:\cr
      \code{signature(from = "ANY", to = "slong")}:\cr
      returns \code{.ulong(x = from)} or \code{.slong(x = from)}.  An
      error is signaled if the class or type of \code{from} is not
      supported by the methods for \code{\link{initialize}}.}
    \item{\code{format}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "ulong", length = "\var{numeric}")}:\cr
      \code{signature(.Object = "slong", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "ulong", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      \code{signature(.Object = "slong", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if elements of \code{x} are not in the range of
      the \proglang{C} type, in particular if elements are \code{NaN}.
      The range is \eqn{(-1, 2^{n})} for \code{ulong} and
      \eqn{(-2^{n-1}-1, 2^{n-1})} for \code{slong}, where \eqn{n} is the
      value of \code{\link{flintABI}()}.}
    \item{\code{is.finite}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{ulong} and \code{slong} have no representation for
      \code{NaN}, \code{-Inf}, and \code{Inf}.}
    \item{\code{is.infinite}, \code{is.na}, \code{is.nan}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{ulong} and \code{slong} have no representation for
      \code{NaN}, \code{-Inf}, and \code{Inf}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{mean}}{
      \code{signature(x = "ulong")}:\cr
      \code{signature(x = "slong")}:\cr
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
  types: \url{https://flintlib.org/doc/flint.html}
}
\examples{
showClass("ulong")
showClass("slong")
showMethods(classes = c("ulong", "slong"))
}
\keyword{classes}
