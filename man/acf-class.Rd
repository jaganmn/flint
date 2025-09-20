\name{acf-class}
\alias{acf-class}
\alias{ACF}
\alias{acf}
\alias{ACF.array}
\alias{acf.array}
\alias{!,acf-method}
\alias{\%*\%,ANY,acf-method}
\alias{\%*\%,acf,ANY-method}
\alias{\%*\%,acf,ulong-method}
\alias{\%*\%,acf,slong-method}
\alias{\%*\%,acf,fmpz-method}
\alias{\%*\%,acf,fmpq-method}
\alias{\%*\%,acf,mag-method}
\alias{\%*\%,acf,arf-method}
\alias{\%*\%,acf,acf-method}
\alias{\%*\%,acf,arb-method}
\alias{\%*\%,acf,acb-method}
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
\alias{backsolve,ANY,acf-method}
\alias{backsolve,acf,ANY-method}
\alias{backsolve,acf,ulong-method}
\alias{backsolve,acf,slong-method}
\alias{backsolve,acf,fmpz-method}
\alias{backsolve,acf,fmpq-method}
\alias{backsolve,acf,mag-method}
\alias{backsolve,acf,arf-method}
\alias{backsolve,acf,acf-method}
\alias{backsolve,acf,arb-method}
\alias{backsolve,acf,acb-method}
\alias{chol,acf-method}
\alias{chol2inv,acf-method}
\alias{coerce,ANY,acf-method}
\alias{colMeans,acf-method}
\alias{colSums,acf-method}
\alias{crossprod,ANY,acf-method}
\alias{crossprod,acf,ANY-method}
\alias{crossprod,acf,ulong-method}
\alias{crossprod,acf,slong-method}
\alias{crossprod,acf,fmpz-method}
\alias{crossprod,acf,fmpq-method}
\alias{crossprod,acf,mag-method}
\alias{crossprod,acf,arf-method}
\alias{crossprod,acf,acf-method}
\alias{crossprod,acf,arb-method}
\alias{crossprod,acf,acb-method}
\alias{det,acf-method}
\alias{determinant,acf-method}
\alias{is.finite,acf-method}
\alias{is.infinite,acf-method}
\alias{is.na,acf-method}
\alias{is.nan,acf-method}
\alias{is.unsorted,acf-method}
\alias{mean,acf-method}
\alias{rowMeans,acf-method}
\alias{rowSums,acf-method}
\alias{solve,ANY,acf-method}
\alias{solve,acf,ANY-method}
\alias{solve,acf,ulong-method}
\alias{solve,acf,slong-method}
\alias{solve,acf,fmpz-method}
\alias{solve,acf,fmpq-method}
\alias{solve,acf,mag-method}
\alias{solve,acf,arf-method}
\alias{solve,acf,acf-method}
\alias{solve,acf,arb-method}
\alias{solve,acf,acb-method}
\alias{tcrossprod,ANY,acf-method}
\alias{tcrossprod,acf,ANY-method}
\alias{tcrossprod,acf,ulong-method}
\alias{tcrossprod,acf,slong-method}
\alias{tcrossprod,acf,fmpz-method}
\alias{tcrossprod,acf,fmpq-method}
\alias{tcrossprod,acf,mag-method}
\alias{tcrossprod,acf,arf-method}
\alias{tcrossprod,acf,acf-method}
\alias{tcrossprod,acf,arb-method}
\alias{tcrossprod,acf,acb-method}
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
  and methods documented here are unrelated.

  The class generator functions are named \code{ACF} and
  \code{ACF.array} instead of \code{acf} and \code{acf.array} because
  an exported function named \code{acf} would mask the function in
  package \pkg{stats}.
}
\usage{
## Class generator functions

ACF(x = 0i, length = 0L, names = NULL, real = 0, imag = 0)

ACF.array(x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{acf}.}
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
  \item{real, imag}{
    atomic or \code{\linkS4class{flint}} vectors containing data for
    conversion to \code{\linkS4class{arf}}.  Use these instead of
    \code{x} for initialization \dQuote{by parts} (real and imaginary).}
}
\value{
  An \code{acf} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has six distinct usages:
  \preformatted{acf.()
acf.(length=)
acf.(x)
acf.(x, length=)
acf.(real=, imag=)
acf.(real=, imag=, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  The fifth and sixth usages, in which either of \code{real} and
  \code{imag} can be missing, use \code{\link{arf}(real)} and
  \code{\link{arf}(imag)} to separately initialize the real and
  imaginary parts of the \code{acf} return value.

  Attempts to recycle \code{real}, \code{imag}, or \code{x} of length
  zero to nonzero length are an error.

  Usage of \code{acf.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real and imaginary parts of complex numbers are
  rounded according to the default precision and rounding mode set by
  \code{\link{flintPrec}} and \code{\link{flintRnd}}.

  Character strings are scanned first for a real part then for an
  imaginary part.  They can use any of three formats:
  \code{"\var{s}\var{a}"}, \code{"\var{t}\var{b}i"}, and
  \code{"\var{s}\var{a}\var{t}\var{b}i"}, where \code{\var{s}} and
  \code{\var{a}} define the sign and absolute value of the real part and
  \code{\var{t}} and \code{\var{b}} define the sign and absolute value
  of the imaginary part.  \code{\var{s}} can be empty if the real part
  is \code{NaN} or non-negative.  \code{\var{t}} can be empty if the
  imaginary part is \code{NaN} or non-negative, but only in the second
  format.

  The sequences \code{\var{s}\var{a}} and \code{\var{t}\var{b}} are
  converted using function \code{mpfr_strtofr} from the \acronym{GNU}
  \acronym{MPFR} library with argument \code{base} set to 0 and argument
  \code{rnd} set according to the default rounding mode; see
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
    \item{\code{!}}{
      \code{signature(x = "acf")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\%*\%}, \code{crossprod}, \code{tcrossprod}}{
      \code{signature(x = "acf", y = "acf")}:\cr
      \code{signature(x = "acf", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "acf")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.  The return value
      is approximate insofar that it may not be correctly rounded.}
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
      \code{signature(e1 = "acf", e2 = "acf")}:\cr
      \code{signature(e1 = "acf", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "acf")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
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
      handled specially.  See also \code{\link{asVector}}.}
    \item{\code{backsolve}}{
      \code{signature(r = "acf", x = "acf")}:\cr
      \code{signature(r = "acf", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "acf")}:\cr
      solution of the triangular system \code{op2(op1(r)) \%*\% y = x},
      where \code{op1=ifelse(upper.tri, triu, tril)} and
      \code{op2=ifelse(transpose, t, identity)} and \code{upper.tri}
      and \code{transpose} are optional logical arguments with default
      values \code{TRUE} and \code{FALSE}, respectively.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{x} is missing, then
      the return value is the inverse of \code{op2(op1(r))}, as if
      \code{x} were the identity matrix.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array \code{x} are handled as \code{length(x)}-by-1 matrices.}
    \item{\code{chol}}{
      \code{signature(x = "acf")}:\cr
      returns the upper triangular Cholesky factor of the positive
      definite matrix whose upper triangular part is the upper
      triangular part of \code{x} (discarding imaginary parts of
      diagonal entries).}
    \item{\code{chol2inv}}{
      \code{signature(x = "acf")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x} (discarding imaginary parts of diagonal entries).}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "acf")}:\cr
      returns the value of \code{acf.(from)}.}
    \item{\code{colSums}, \code{colMeans}}{
      \code{signature(x = "acf")}:\cr
      returns an \code{acf} vector or array containing the column sums
      or means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{det}}{
      \code{signature(x = "acf")}:\cr
      returns the determinant of \code{x} as an \code{acf} vector of
      length 1.}
    \item{\code{determinant}}{
      \code{signature(x = "acf")}:\cr
      returns a list with components \code{modulus} and \code{argument}
      specifying the determinant of \code{x}, following the \pkg{base}
      function (except for the use of argument instead of sign), hence
      see \code{\link{determinant}}.}
    \item{\code{format}}{
      \code{signature(x = "acf")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"a+bi"} and scientific format for each \code{a} and
      \code{b}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
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
    \item{\code{rowSums}, \code{rowMeans}}{
      \code{signature(x = "acf")}:\cr
      returns an \code{acf} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{solve}}{
      \code{signature(a = "acf", b = "acf")}:\cr
      \code{signature(a = "acf", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "acf")}:\cr
      solution of the general system \code{a \%*\% x = b}.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{b} is missing, then
      the return value is the inverse of \code{a}, as if \code{b} were
      the identity matrix.  Operands are promoted as necessary and must
      be conformable (have compatible dimensions).  Non-array \code{b}
      are handled as \code{length(b)}-by-1 matrices.}
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
