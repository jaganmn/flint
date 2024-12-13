\name{mag-class}
\alias{mag-class}
\alias{!,mag-method}
\alias{+,mag,missing-method}
\alias{-,mag,missing-method}
\alias{Complex,mag-method}
\alias{Math,mag-method}
\alias{Math2,mag-method}
\alias{Ops,ANY,mag-method}
\alias{Ops,mag,ANY-method}
\alias{Ops,mag,slong-method}
\alias{Ops,mag,ulong-method}
\alias{Ops,mag,fmpz-method}
\alias{Ops,mag,fmpq-method}
\alias{Ops,mag,arf-method}
\alias{Ops,mag,mag-method}
\alias{Ops,mag,arb-method}
\alias{Ops,mag,acb-method}
\alias{Summary,mag-method}
\alias{as.vector,mag-method}
\alias{coerce,ANY,mag-method}
\alias{initialize,mag-method}
\alias{is.finite,mag-method}
\alias{is.infinite,mag-method}
\alias{is.na,mag-method}
\alias{is.nan,mag-method}
\alias{log,mag-method}
\title{Class of Vectors of Magnitude (Error) Bounds}
\docType{class}
\description{
  Class \code{mag} extends virtual class \code{flint}.  It represents
  vectors of unsigned floating-point numbers with a fixed precision
  significand and an arbitary precision exponent.  In typical usage,
  \code{mag} objects store magnitude (or error) bounds on real numbers.
  The value can be positive infinity but never not-a-number.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{!}}{
      \code{signature(x = "mag")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "mag", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "mag", e2 = "missing")}:\cr
      returns a copy of the argument, to be consistent with the binary
      operation which returns an upper bound for the absolute value of
      the difference.}
    \item{\code{Complex}}{
      \code{signature(z = "mag")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is an
      upper bound for the absolute value of the exact answer.}
    \item{\code{Math}}{
      \code{signature(x = "mag")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is an
      upper bound for the absolute value of the exact answer.  Notably,
      the (inverse) trigonometric, (inverse) hyperbolic, and
      gamma-related member functions are not yet implemented.  Users
      wanting those can (for now) operate on
      \code{as(x, "\linkS4class{arb}")}.}
    \item{\code{Math2}}{
      \code{signature(x = "mag")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      functions: \code{\link{round}}, \code{\link{signif}}.  The return
      value is an upper bound for the absolute value of the exact
      answer.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "mag")}:\cr
      \code{signature(e1 = "mag", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.  For arithmetic, the return value is a
      \code{mag} vector only if both operands are \code{mag} vectors.
      In that case, the return value is an upper bound for the absolute
      value of the exact answer.  Users wanting \dQuote{standard}
      floating-point arithmetic must ensure that at least one operand is
      not a \code{mag} vector.}
    \item{\code{Summary}}{
      \code{signature(x = "mag")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{mag} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).  For \code{sum} and
      \code{prod}, the return value is an upper bound for the absolute
      value of the exact answer.}
    \item{\code{as.vector}}{
      \code{signature(x = "mag")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding in the direction of
      \code{Inf}, not always to nearest.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "mag")}:\cr
      returns \code{new("mag", x = from)}.}
    \item{\code{format}}{
      \code{signature(x = "mag")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "mag", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with upper bounds for \code{abs(x)}.  An error is
      signaled if any \code{x[i]} is \code{NaN}.}
    \item{\code{is.na, is.nan}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{mag} has no representation for \code{NaN}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{Inf}.}
    \item{\code{is.finite}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are not \code{Inf}.}
    \item{\code{log}}{
      \code{signature(x = "mag")}:\cr
      returns an upper bound for the absolute value of the logarithm of
      the argument.  The natural logarithm is computed by default (when
      optional argument \code{base} is unset).}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/mag.html}
}
\examples{
showClass("mag")
showMethods(classes = "mag")
}
