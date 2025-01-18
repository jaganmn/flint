\name{arb-class}
\alias{arb-class}
\alias{arb}
\alias{.arb}
\alias{!,arb-method}
\alias{+,arb,missing-method}
\alias{-,arb,missing-method}
\alias{Complex,arb-method}
\alias{Math,arb-method}
\alias{Math2,arb-method}
\alias{Ops,ANY,arb-method}
\alias{Ops,arb,ANY-method}
\alias{Ops,arb,ulong-method}
\alias{Ops,arb,slong-method}
\alias{Ops,arb,fmpz-method}
\alias{Ops,arb,fmpq-method}
\alias{Ops,arb,mag-method}
\alias{Ops,arb,arf-method}
\alias{Ops,arb,acf-method}
\alias{Ops,arb,arb-method}
\alias{Ops,arb,acb-method}
\alias{Summary,arb-method}
\alias{anyNA,arb-method}
\alias{as.vector,arb-method}
\alias{coerce,ANY,arb-method}
\alias{initialize,arb-method}
\alias{is.finite,arb-method}
\alias{is.infinite,arb-method}
\alias{is.na,arb-method}
\alias{is.nan,arb-method}
\alias{is.unsorted,arb-method}
\alias{log,arb-method}
\alias{mean,arb-method}
\alias{xtfrm,arb-method}
\docType{class}
\title{Arbitrary Precision Floating-Point Real Numbers with Error Bounds}
\description{
  Class \code{arb} extends virtual class \code{flint}.  It represents
  vectors of arbitrary precision floating-point real numbers with error
  bounds.  Elements are specified by a pair of mixed format
  floating-point numbers: an \code{\linkS4class{arf}} midpoint and a
  \code{\linkS4class{mag}} radius.

  Arithmetic on \code{arb} vectors is midpoint-radius interval
  arithmetic, also known as ball arithmetic, enabling computation with
  rigorous propagation of errors.  Logic and comparison involving
  \code{arb} vectors are defined as follows: unary \code{op(x)} is true
  if and only if \code{op} is true for all elements of the interval
  \code{x}, and binary \code{op(x, y)} is true if and only if \code{op}
  is true for all elements of the Cartesian product of the intervals
  \code{x} and \code{y}.  A corollary is that the operator \code{<=}
  does not define a \emph{total order} on the range of \code{arb} (that
  is, the set of intervals \eqn{[m-r,m+r]}), and a consequence is that
  methods for generic functions that necessitate a total order tend to
  signal an error.
}
\usage{
## The class generator function:
.arb(\dots)

## Mode 1: initialize with zeros
## .arb(length = 0L)
##
## Mode 2: initialize with vector
## .arb(length = length(x), x)
##
## Mode 3: initialize by parts
## .arb(length = max0(length(mid), length(rad)), mid, rad)
##
## where max0 <- function(m, n) if (min(m, n)) max(m, n) else 0L
}
\arguments{
  \item{\dots}{
    arguments passed to methods for \code{\link{initialize}}.}
}
\value{
  The class generator function returns \code{\link{new}("arb", \dots)}.
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
      \code{signature(x = "arb")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "arb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{Math}}{
      \code{signature(x = "arb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{Math2}}{
      \code{signature(x = "arb")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "arb")}:\cr
      \code{signature(e1 = "arb", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "arb")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{arb} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{anyNA}}{
      \code{signature(x = "arb")}:\cr
      returns \code{TRUE} if any element of \code{x} has midpoint
      \code{NaN}, \code{FALSE} otherwise.}
    \item{\code{as.vector}}{
      \code{signature(x = "arb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting the midpoints of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number (with precedence to even
      significands in case of ties), unless a midpoint exceeds
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.  Coercion
      to types \code{"character"}, \code{"symbol"} (synonym
      \code{"name"}), \code{"pairlist"}, \code{"list"}, and
      \code{"expression"}, which are not \dQuote{number-like}, is
      handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "arb")}:\cr
      returns \code{.arb(x = from)}.  An error is signaled if the class
      or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
    \item{\code{format}}{
      \code{signature(x = "arb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)"} and scientific format for \code{m} and
      \code{r}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero midpoint, zero radius).
      \code{.Object} is not copied, so all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "\var{numeric}", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized by the elements of \code{x}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "\var{numeric}", mid = "\var{atomic|flint}", rad = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      midpoints and radii are initialized separately with values from
      \code{mid} and \code{rad}, which are coerced to classes
      \code{\linkS4class{arf}} and \code{\linkS4class{mag}} and recycled
      as necessary.}
    \item{\code{is.finite}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      do not have midpoint \code{NaN}, \code{-Inf}, or \code{Inf} or
      radius \code{Inf}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have midpoint \code{-Inf} or \code{Inf} or radius \code{Inf}.}
    \item{\code{is.na}, \code{is.nan}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have midpoint \code{NaN}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "arb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}; see \code{xtfrm} below.}
    \item{\code{log}}{
      \code{signature(x = "arb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{mean}}{
      \code{signature(x = "arb")}:\cr
      returns the arithmetic mean.}
    \item{\code{xtfrm}}{
      \code{signature(x = "arb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}: \code{a <= b || b <= a} is is not
      \code{TRUE} for all finite \code{a} and \code{b} of class
      \code{arb}.  Thus, direct sorting of \code{arb} is not supported.
      Users wanting to order the \emph{midpoints} should operate on
      \code{\link{Mid}(x)}.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.  Generic functions
  \code{\link{Mid}} and \code{\link{Rad}} and their replacement forms
  for getting and setting midpoints and radii.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arb.html}

  Johansson, F. (2017).
  Arb: efficient arbitrary-precision midpoint-radius interval
  arithmetic.
  \emph{IEEE Transactions on Computers},
  \emph{66}(8), 1281-1292.
  \doi{10.1109/TC.2017.2690633}
}
\examples{
showClass("arb")
showMethods(classes = "arb")
}
\keyword{classes}
