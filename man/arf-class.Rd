\name{arf-class}
\alias{arf-class}
\alias{arf}
\alias{arf.array}
\alias{!,arf-method}
\alias{\%*\%,ANY,arf-method}
\alias{\%*\%,arf,ANY-method}
\alias{\%*\%,arf,ulong-method}
\alias{\%*\%,arf,slong-method}
\alias{\%*\%,arf,fmpz-method}
\alias{\%*\%,arf,fmpq-method}
\alias{\%*\%,arf,mag-method}
\alias{\%*\%,arf,arf-method}
\alias{\%*\%,arf,acf-method}
\alias{\%*\%,arf,arb-method}
\alias{\%*\%,arf,acb-method}
\alias{+,arf,missing-method}
\alias{-,arf,missing-method}
\alias{Complex,arf-method}
\alias{Math,arf-method}
\alias{Math2,arf-method}
\alias{Ops,ANY,arf-method}
\alias{Ops,arf,ANY-method}
\alias{Ops,arf,ulong-method}
\alias{Ops,arf,slong-method}
\alias{Ops,arf,fmpz-method}
\alias{Ops,arf,fmpq-method}
\alias{Ops,arf,mag-method}
\alias{Ops,arf,arf-method}
\alias{Ops,arf,acf-method}
\alias{Ops,arf,arb-method}
\alias{Ops,arf,acb-method}
\alias{Summary,arf-method}
\alias{anyNA,arf-method}
\alias{as.vector,arf-method}
\alias{backsolve,ANY,arf-method}
\alias{backsolve,arf,ANY-method}
\alias{backsolve,arf,ulong-method}
\alias{backsolve,arf,slong-method}
\alias{backsolve,arf,fmpz-method}
\alias{backsolve,arf,fmpq-method}
\alias{backsolve,arf,mag-method}
\alias{backsolve,arf,arf-method}
\alias{backsolve,arf,acf-method}
\alias{backsolve,arf,arb-method}
\alias{backsolve,arf,acb-method}
\alias{chol,arf-method}
\alias{chol2inv,arf-method}
\alias{coerce,ANY,arf-method}
\alias{colMeans,arf-method}
\alias{colSums,arf-method}
\alias{crossprod,ANY,arf-method}
\alias{crossprod,arf,ANY-method}
\alias{crossprod,arf,ulong-method}
\alias{crossprod,arf,slong-method}
\alias{crossprod,arf,fmpz-method}
\alias{crossprod,arf,fmpq-method}
\alias{crossprod,arf,mag-method}
\alias{crossprod,arf,arf-method}
\alias{crossprod,arf,acf-method}
\alias{crossprod,arf,arb-method}
\alias{crossprod,arf,acb-method}
\alias{det,arf-method}
\alias{determinant,arf-method}
\alias{diff,arf-method}
\alias{diffinv,arf-method}
\alias{is.finite,arf-method}
\alias{is.infinite,arf-method}
\alias{is.na,arf-method}
\alias{is.nan,arf-method}
\alias{is.unsorted,arf-method}
\alias{log,arf-method}
\alias{mean,arf-method}
\alias{rowMeans,arf-method}
\alias{rowSums,arf-method}
\alias{solve,ANY,arf-method}
\alias{solve,arf,ANY-method}
\alias{solve,arf,ulong-method}
\alias{solve,arf,slong-method}
\alias{solve,arf,fmpz-method}
\alias{solve,arf,fmpq-method}
\alias{solve,arf,mag-method}
\alias{solve,arf,arf-method}
\alias{solve,arf,acf-method}
\alias{solve,arf,arb-method}
\alias{solve,arf,acb-method}
\alias{tcrossprod,ANY,arf-method}
\alias{tcrossprod,arf,ANY-method}
\alias{tcrossprod,arf,ulong-method}
\alias{tcrossprod,arf,slong-method}
\alias{tcrossprod,arf,fmpz-method}
\alias{tcrossprod,arf,fmpq-method}
\alias{tcrossprod,arf,mag-method}
\alias{tcrossprod,arf,arf-method}
\alias{tcrossprod,arf,acf-method}
\alias{tcrossprod,arf,arb-method}
\alias{tcrossprod,arf,acb-method}
\docType{class}
\title{Arbitrary Precision Floating-Point Real Numbers}
\description{
  Class \code{arf} extends virtual class \code{\linkS4class{flint}}.  It
  represents vectors of arbitrary precision floating-point real numbers.
  Elements have arbitrary precision significand and exponent.  The
  underlying \proglang{C} type can represent \code{NaN}, \code{-Inf},
  and \code{Inf}.
}
\usage{
## Class generator functions

arf(x = 0, length = 0L, names = NULL,
    prec = NULL, rnd = NULL)

arf.array(x = 0, dim = length(x), dimnames = NULL,
          prec = NULL, rnd = NULL)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{arf}.}
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
  \item{prec}{
    the precision used for conversion.  \code{NULL} means to convert
    exactly if possible and to round to the global default precision
    otherwise; see \code{\link{flintPrec}}.}
  \item{rnd}{
    the rounding mode used for inexact conversion.  \code{NULL} means to
    round according to the global default rounding mode; see
    \code{\link{flintRnd}}.}
}
\value{
  A \code{arf} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has four distinct usages:
  \preformatted{arf()
arf(length=)
arf(x)
arf(x, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  Attempts to recycle \code{x} of length zero to nonzero length
  are an error.

  Usage of \code{arf.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real parts of complex numbers are rounded according
  to the precision and rounding mode set by \code{prec} and \code{rnd}.
  Imaginary parts of complex numbers are discarded.

  Character strings are converted using function \code{mpfr_strtofr}
  from the \acronym{GNU} \acronym{MPFR} library with argument
  \code{base} set to 0; see
  \url{https://www.mpfr.org/mpfr-current/mpfr.html#Assignment-Functions}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}, \code{dim}, \code{dimnames}, \code{names}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  Due to constraints imposed by generic functions, methods typically do
  \emph{not} provide a formal argument \code{prec} allowing for a
  precision to be indicated in the function call.  Such methods use the
  current default precision set by \code{\link{flintPrec}}.
  \describe{
    \item{\code{\link{!}}}{
      \code{signature(x = "arf")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{tcrossprod}}}{
      \code{signature(x = "arf", y = "arf")}:\cr
      \code{signature(x = "arf", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "arf")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.  The return value
      is approximate insofar that it may not be correctly rounded.}
    \item{\code{\link{+}}}{
      \code{signature(e1 = "arf", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{\link{-}}}{
      \code{signature(e1 = "arf", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{\link[methods]{Complex}}}{
      \code{signature(z = "arf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math}}}{
      \code{signature(x = "arf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math2}}}{
      \code{signature(x = "arf")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{\link[methods]{Ops}}}{
      \code{signature(e1 = "arf", e2 = "arf")}:\cr
      \code{signature(e1 = "arf", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "arf")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
    \item{\code{\link[methods]{Summary}}}{
      \code{signature(x = "arf")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{arf} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{\link{anyNA}}}{
      \code{signature(x = "arf")}:\cr
      returns \code{TRUE} if any element of \code{x} is \code{NaN},
      \code{FALSE} otherwise.}
    \item{\code{\link{as.vector}}}{
      \code{signature(x = "arf")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number (with precedence to even
      significands in case of ties), unless the element exceeds
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.  Coercion
      to types \code{"character"}, \code{"symbol"} (synonym
      \code{"name"}), \code{"pairlist"}, \code{"list"}, and
      \code{"expression"}, which are not \dQuote{number-like}, is
      handled specially.  See also \code{\link{asVector}}.}
    \item{\code{\link{backsolve}}}{
      \code{signature(r = "arf", x = "arf")}:\cr
      \code{signature(r = "arf", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "arf")}:\cr
      solution of the triangular system \code{op2(op1(r)) \%*\% y = x},
      where \code{op1=ifelse(upper.tri, triu, tril)} and
      \code{op2=ifelse(transpose, t, identity)} and \code{upper.tri} and
      \code{transpose} are optional logical arguments with default
      values \code{TRUE} and \code{FALSE}, respectively.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{x} is missing, then
      the return value is the inverse of \code{op2(op1(r))}, as if
      \code{x} were the identity matrix.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array \code{x} are handled as \code{length(x)}-by-1 matrices.}
    \item{\code{\link{chol}}}{
      \code{signature(x = "arf")}:\cr
      returns the upper triangular Cholesky factor of the positive
      definite matrix whose upper triangular part is the upper
      triangular part of \code{x}.}
    \item{\code{\link{chol2inv}}}{
      \code{signature(x = "arf")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x}.}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "arf")}:\cr
      returns the value of \code{arf(from)}.}
    \item{\code{\link{colSums}}, \code{\link{colMeans}}}{
      \code{signature(x = "arf")}:\cr
      returns an \code{arf} vector or array containing the column sums
      or means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{\link{det}}}{
      \code{signature(x = "arf")}:\cr
      returns the determinant of \code{x} as an \code{arf} vector of
      length 1.}
    \item{\code{\link{determinant}}}{
      \code{signature(x = "arf")}:\cr
      returns a list with components \code{modulus} and \code{sign}
      specifying the determinant of \code{x}, following the documented
      behaviour of the \pkg{base} function.}
    \item{\code{\link{diff}}}{
      \code{signature(x = "arf")}:\cr
      returns a vector storing lagged differences of the elements of
      \code{x} or (if \code{x} is a matrix) a matrix storing lagged
      differences of the rows of \code{x}, following the documented
      behaviour of the S3 default method.}
    \item{\code{\link[stats]{diffinv}}}{
      \code{signature(x = "arf")}:\cr
      returns the vector or matrix \code{y} such that
      \code{x = diff(y, \dots)}, following the documented behaviour of
      the S3 default method.}
    \item{\code{\link{format}}}{
      \code{signature(x = "arf")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{\link{is.finite}}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are not \code{NaN}, \code{-Inf}, or \code{Inf}.}
    \item{\code{\link{is.infinite}}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{-Inf} or \code{Inf}.}
    \item{\code{\link{is.na}}, \code{\link{is.nan}}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{NaN}.}
    \item{\code{\link{is.unsorted}}}{
      \code{signature(x = "arf")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{\link{log}}}{
      \code{signature(x = "arf")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{\link{mean}}}{
      \code{signature(x = "arf")}:\cr
      returns the arithmetic mean.}
    \item{\code{\link{rowSums}}, \code{\link{rowMeans}}}{
      \code{signature(x = "arf")}:\cr
      returns an \code{arf} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{solve}}}{
      \code{signature(a = "arf", b = "arf")}:\cr
      \code{signature(a = "arf", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "arf")}:\cr
      solution of the general system \code{a \%*\% x = b}.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{b} is missing, then
      the return value is the inverse of \code{a}, as if \code{b} were
      the identity matrix.  Operands are promoted as necessary and must
      be conformable (have compatible dimensions).  Non-array \code{b}
      are handled as \code{length(b)}-by-1 matrices.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arf.html}

  Johansson, F. (2017).
  Arb: efficient arbitrary-precision midpoint-radius interval
  arithmetic.
  \emph{IEEE Transactions on Computers},
  \emph{66}(8), 1281-1292.
  \doi{10.1109/TC.2017.2690633}
}
\examples{
showClass("arf")
showMethods(classes = "arf")
}
\keyword{classes}
