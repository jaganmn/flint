\name{mag-class}
\alias{mag-class}
\alias{mag}
\alias{mag.array}
\alias{!,mag-method}
\alias{\%*\%,ANY,mag-method}
\alias{\%*\%,mag,ANY-method}
\alias{\%*\%,mag,ulong-method}
\alias{\%*\%,mag,slong-method}
\alias{\%*\%,mag,fmpz-method}
\alias{\%*\%,mag,fmpq-method}
\alias{\%*\%,mag,mag-method}
\alias{\%*\%,mag,arf-method}
\alias{\%*\%,mag,acf-method}
\alias{\%*\%,mag,arb-method}
\alias{\%*\%,mag,acb-method}
\alias{+,mag,missing-method}
\alias{-,mag,missing-method}
\alias{Complex,mag-method}
\alias{Math,mag-method}
\alias{Math2,mag-method}
\alias{Ops,ANY,mag-method}
\alias{Ops,mag,ANY-method}
\alias{Ops,mag,ulong-method}
\alias{Ops,mag,slong-method}
\alias{Ops,mag,fmpz-method}
\alias{Ops,mag,fmpq-method}
\alias{Ops,mag,mag-method}
\alias{Ops,mag,arf-method}
\alias{Ops,mag,acf-method}
\alias{Ops,mag,arb-method}
\alias{Ops,mag,acb-method}
\alias{Summary,mag-method}
\alias{anyNA,mag-method}
\alias{as.vector,mag-method}
\alias{backsolve,ANY,mag-method}
\alias{backsolve,mag,ANY-method}
\alias{backsolve,mag,ulong-method}
\alias{backsolve,mag,slong-method}
\alias{backsolve,mag,fmpz-method}
\alias{backsolve,mag,fmpq-method}
\alias{backsolve,mag,mag-method}
\alias{backsolve,mag,arf-method}
\alias{backsolve,mag,acf-method}
\alias{backsolve,mag,arb-method}
\alias{backsolve,mag,acb-method}
\alias{chol,mag-method}
\alias{chol2inv,mag-method}
\alias{coerce,ANY,mag-method}
\alias{colMeans,mag-method}
\alias{colSums,mag-method}
\alias{crossprod,ANY,mag-method}
\alias{crossprod,mag,ANY-method}
\alias{crossprod,mag,ulong-method}
\alias{crossprod,mag,slong-method}
\alias{crossprod,mag,fmpz-method}
\alias{crossprod,mag,fmpq-method}
\alias{crossprod,mag,mag-method}
\alias{crossprod,mag,arf-method}
\alias{crossprod,mag,acf-method}
\alias{crossprod,mag,arb-method}
\alias{crossprod,mag,acb-method}
\alias{det,mag-method}
\alias{determinant,mag-method}
\alias{is.finite,mag-method}
\alias{is.infinite,mag-method}
\alias{is.na,mag-method}
\alias{is.nan,mag-method}
\alias{is.unsorted,mag-method}
\alias{log,mag-method}
\alias{mean,mag-method}
\alias{rowMeans,mag-method}
\alias{rowSums,mag-method}
\alias{solve,ANY,mag-method}
\alias{solve,mag,ANY-method}
\alias{solve,mag,ulong-method}
\alias{solve,mag,slong-method}
\alias{solve,mag,fmpz-method}
\alias{solve,mag,fmpq-method}
\alias{solve,mag,mag-method}
\alias{solve,mag,arf-method}
\alias{solve,mag,acf-method}
\alias{solve,mag,arb-method}
\alias{solve,mag,acb-method}
\alias{tcrossprod,ANY,mag-method}
\alias{tcrossprod,mag,ANY-method}
\alias{tcrossprod,mag,ulong-method}
\alias{tcrossprod,mag,slong-method}
\alias{tcrossprod,mag,fmpz-method}
\alias{tcrossprod,mag,fmpq-method}
\alias{tcrossprod,mag,mag-method}
\alias{tcrossprod,mag,arf-method}
\alias{tcrossprod,mag,acf-method}
\alias{tcrossprod,mag,arb-method}
\alias{tcrossprod,mag,acb-method}
\docType{class}
\title{Fixed Precision Magnitude (Error) Bounds}
\description{
  Class \code{mag} extends virtual class \code{\linkS4class{flint}}.  It
  represents vectors of fixed precision error bounds.  Elements are
  unsigned floating-point numbers with a 30-bit significand and an
  arbitary precision exponent.  The underlying \proglang{C} type can
  represent \code{Inf} but not \code{NaN}.
}
\usage{
## Class generator functions

mag(x = 0, length = 0L, names = NULL)

mag.array(x = 0, dim = length(x), dimnames = NULL)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{mag}.}
  \item{length}{
    a numeric vector of length one giving the length of the return
    value.  If that exceeds the length of \code{x}, then \code{x} is
    recycled.  Non-integer values are rounded in the direction of zero.}
  \item{names}{
    the \code{names} slot of the return value, either \code{NULL} or a
    character vector of equal length.  Non-character \code{names} are
    coerced to character.}
  \item{dim}{
    the \code{dim} slot of the return value, an integer vector of
    nonzero length.  If the product exceeds the length of \code{x}, then
    \code{x} is recycled.  Non-integer numeric \code{dim} are coerced to
    integer.}
  \item{dimnames}{
    the \code{dimnames} slot of the return value, either \code{NULL} or
    a list of length equal to the length of \code{dim}.  The components
    are either \code{NULL} or character vectors of length given by
    \code{dim}.  Non-character vector components of \code{dimnames} are
    coerced to character.}
}
\value{
  A \code{mag} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has four distinct usages:
  \preformatted{mag()
mag(length=)
mag(x)
mag(x, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  Attempts to recycle \code{x} of length zero to nonzero length
  are an error.

  Usage of \code{mag.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Magnitudes of real numbers and real parts of complex numbers are
  rounded in the direction of 0 or \code{Inf} according to the default
  rounding mode set by \code{\link{flintRnd}}.  Imaginary parts of
  complex numbers are discarded.

  Character strings are converted using function \code{mpfr_strtofr}
  from the \acronym{GNU} \acronym{MPFR} library with argument
  \code{base} set to 0 and argument \code{rnd} set according to the
  default rounding mode; see
  \url{https://www.mpfr.org/mpfr-current/mpfr.html#Assignment-Functions}.

  An error is signaled if elements of \code{x} are \code{NaN}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}, \code{dim}, \code{dimnames}, \code{names}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{!}}{
      \code{signature(x = "mag")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\%*\%}, \code{crossprod}, \code{tcrossprod}}{
      \code{signature(x = "mag", y = "mag")}:\cr
      \code{signature(x = "mag", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "mag")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
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
      wanting those can (for now) operate on \code{\link{arb}(x)}.}
    \item{\code{Math2}}{
      \code{signature(x = "mag")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      functions: \code{\link{round}}, \code{\link{signif}}.  The return
      value is an upper bound for the exact answer.}
    \item{\code{Ops}}{
      \code{signature(e1 = "mag", e2 = "mag")}:\cr
      \code{signature(e1 = "mag", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "mag")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.  For arithmetic, the return value
      is a \code{mag} vector only if both operands are \code{mag}
      vectors.  In that case, the return value is an upper bound for the
      absolute value of the exact answer.  Users wanting
      \dQuote{standard} floating-point arithmetic must ensure that at
      least one operand is not a \code{mag} vector.}
    \item{\code{Summary}}{
      \code{signature(x = "mag")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{mag} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).  For \code{sum} and
      \code{prod}, the return value is an upper bound for the exact
      answer.}
    \item{\code{anyNA}}{
      \code{signature(x = "mag")}:\cr
      returns \code{FALSE}, as \code{mag} has no representation for
      \code{NaN}.}
    \item{\code{as.vector}}{
      \code{signature(x = "mag")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding in the direction of
      \code{Inf}, not always to nearest.  Coercion to types
      \code{"character"}, \code{"symbol"} (synonym \code{"name"}),
      \code{"pairlist"}, \code{"list"}, and \code{"expression"}, which
      are not \dQuote{number-like}, is handled specially.  See also
      \code{\link{asVector}}.}
    \item{\code{backsolve}}{
      \code{signature(r = "mag", x = "mag")}:\cr
      \code{signature(r = "mag", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "mag")}:\cr
      coerces the \code{mag} operand to class \code{\linkS4class{arf}},
      \code{\linkS4class{acf}}, \code{\linkS4class{arb}}, or
      \code{\linkS4class{acb}} (depending on the class of the other
      operand) and dispatches.}
    \item{\code{chol}, \code{chol2inv}}{
      \code{signature(x = "mag")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "mag")}:\cr
      returns the value of \code{mag(from)}.}
    \item{\code{colSums}, \code{colMeans}}{
      \code{signature(x = "mag")}:\cr
      returns a \code{mag} vector or array containing upper bounds on
      the column sums or means of \code{x}, defined as sums or means
      over dimensions \code{1:dims}.}
    \item{\code{det}, \code{determinant}}{
      \code{signature(x = "mag")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{format}}{
      \code{signature(x = "mag")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{is.finite}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are not \code{Inf}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{Inf}.}
    \item{\code{is.na}, \code{is.nan}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{mag} has no representation for \code{NaN}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "mag")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{log}}{
      \code{signature(x = "mag")}:\cr
      returns an upper bound for the absolute value of the logarithm of
      the argument.  The natural logarithm is computed by default (when
      optional argument \code{base} is unset).}
    \item{\code{mean}}{
      \code{signature(x = "mag")}:\cr
      returns an upper bound for the arithmetic mean.}
    \item{\code{rowSums}, \code{rowMeans}}{
      \code{signature(x = "mag")}:\cr
      returns a \code{mag} vector or array containing upper bounds on
      the row sums or means of \code{x}, defined as sums or means over
      dimensions \code{(dims+1):length(dim(x))}.}
    \item{\code{solve}}{
      \code{signature(a = "mag", b = "mag")}:\cr
      \code{signature(a = "mag", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "mag")}:\cr
      coerces the \code{mag} operand to class \code{\linkS4class{arf}},
      \code{\linkS4class{acf}}, \code{\linkS4class{arb}}, or
      \code{\linkS4class{acb}} (depending on the class of the other
      operand) and dispatches.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/mag.html}

  Johansson, F. (2017).
  Arb: efficient arbitrary-precision midpoint-radius interval
  arithmetic.
  \emph{IEEE Transactions on Computers},
  \emph{66}(8), 1281-1292.
  \doi{10.1109/TC.2017.2690633}
}
\examples{
showClass("mag")
showMethods(classes = "mag")
}
\keyword{classes}
