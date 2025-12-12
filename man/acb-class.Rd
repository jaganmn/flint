\name{acb-class}
\alias{acb-class}
\alias{acb}
\alias{acb.array}
\alias{!,acb-method}
\alias{\%*\%,ANY,acb-method}
\alias{\%*\%,acb,ANY-method}
\alias{\%*\%,acb,ulong-method}
\alias{\%*\%,acb,slong-method}
\alias{\%*\%,acb,fmpz-method}
\alias{\%*\%,acb,fmpq-method}
\alias{\%*\%,acb,mag-method}
\alias{\%*\%,acb,arf-method}
\alias{\%*\%,acb,acf-method}
\alias{\%*\%,acb,arb-method}
\alias{\%*\%,acb,acb-method}
\alias{+,acb,missing-method}
\alias{-,acb,missing-method}
\alias{Complex,acb-method}
\alias{Math,acb-method}
\alias{Math2,acb-method}
\alias{Ops,ANY,acb-method}
\alias{Ops,acb,ANY-method}
\alias{Ops,acb,ulong-method}
\alias{Ops,acb,slong-method}
\alias{Ops,acb,fmpz-method}
\alias{Ops,acb,fmpq-method}
\alias{Ops,acb,mag-method}
\alias{Ops,acb,arf-method}
\alias{Ops,acb,acf-method}
\alias{Ops,acb,arb-method}
\alias{Ops,acb,acb-method}
\alias{Summary,acb-method}
\alias{anyNA,acb-method}
\alias{as.vector,acb-method}
\alias{backsolve,ANY,acb-method}
\alias{backsolve,acb,ANY-method}
\alias{backsolve,acb,ulong-method}
\alias{backsolve,acb,slong-method}
\alias{backsolve,acb,fmpz-method}
\alias{backsolve,acb,fmpq-method}
\alias{backsolve,acb,mag-method}
\alias{backsolve,acb,arf-method}
\alias{backsolve,acb,acf-method}
\alias{backsolve,acb,arb-method}
\alias{backsolve,acb,acb-method}
\alias{chol,acb-method}
\alias{chol2inv,acb-method}
\alias{coerce,ANY,acb-method}
\alias{colMeans,acb-method}
\alias{colSums,acb-method}
\alias{crossprod,ANY,acb-method}
\alias{crossprod,acb,ANY-method}
\alias{crossprod,acb,ulong-method}
\alias{crossprod,acb,slong-method}
\alias{crossprod,acb,fmpz-method}
\alias{crossprod,acb,fmpq-method}
\alias{crossprod,acb,mag-method}
\alias{crossprod,acb,arf-method}
\alias{crossprod,acb,acf-method}
\alias{crossprod,acb,arb-method}
\alias{crossprod,acb,acb-method}
\alias{det,acb-method}
\alias{determinant,acb-method}
\alias{diff,acb-method}
\alias{diffinv,acb-method}
\alias{is.finite,acb-method}
\alias{is.infinite,acb-method}
\alias{is.na,acb-method}
\alias{is.nan,acb-method}
\alias{is.unsorted,acb-method}
\alias{log,acb-method}
\alias{mean,acb-method}
\alias{rowMeans,acb-method}
\alias{rowSums,acb-method}
\alias{solve,ANY,acb-method}
\alias{solve,acb,ANY-method}
\alias{solve,acb,ulong-method}
\alias{solve,acb,slong-method}
\alias{solve,acb,fmpz-method}
\alias{solve,acb,fmpq-method}
\alias{solve,acb,mag-method}
\alias{solve,acb,arf-method}
\alias{solve,acb,acf-method}
\alias{solve,acb,arb-method}
\alias{solve,acb,acb-method}
\alias{tcrossprod,ANY,acb-method}
\alias{tcrossprod,acb,ANY-method}
\alias{tcrossprod,acb,ulong-method}
\alias{tcrossprod,acb,slong-method}
\alias{tcrossprod,acb,fmpz-method}
\alias{tcrossprod,acb,fmpq-method}
\alias{tcrossprod,acb,mag-method}
\alias{tcrossprod,acb,arf-method}
\alias{tcrossprod,acb,acf-method}
\alias{tcrossprod,acb,arb-method}
\alias{tcrossprod,acb,acb-method}
\alias{xtfrm,acb-method}
\docType{class}
\title{Arbitrary Precision Floating-Point Complex Numbers with Error Bounds}
\description{
  Class \code{acb} extends virtual class \code{flint}.  It represents
  vectors of complex numbers with error bounds on the real and imaginary
  parts.  Elements are specified by two pairs of mixed format
  floating-point numbers: an \code{\linkS4class{arb}} real part and an
  \code{\linkS4class{arb}} imaginary part, each specified by an
  \code{\linkS4class{arf}} midpoint and a \code{\linkS4class{mag}}
  radius.
}
\usage{
## Class generator functions

acb(x = 0i, length = 0L, names = NULL, real = 0, imag = 0,
    prec = NULL)

acb.array(x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0,
          prec = NULL)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{acb}.}
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
    conversion to \code{\linkS4class{arb}}.  Use these for
    initialization \dQuote{by parts} (real and imaginary).}
  \item{prec}{
    the precision used for conversion of midpoints.  \code{NULL} means
    to convert exactly if possible and to round to the global default
    precision otherwise; see \code{\link{flintPrec}}.  By convention,
    rounding is always towards zero.}
}
\value{
  An \code{acb} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has six distinct usages:
  \preformatted{acb()
acb(length=)
acb(x)
acb(x, length=)
acb(real=, imag=)
acb(real=, imag=, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  The fifth and sixth usages, in which either of \code{real} and
  \code{imag} can be missing, use \code{\link{arb}(real)} and
  \code{\link{arb}(imag)} to separately initialize the real and
  imaginary parts of the \code{acb} return value.

  Attempts to recycle \code{real}, \code{imag}, or \code{x} of length
  zero to nonzero length are an error.

  Usage of \code{acb.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real and imaginary parts of complex numbers are
  rounded according to the precision set by \code{prec}, always in the
  direction of zero.  Ball midpoints are the numbers obtained by
  rounding.  Ball radii are upper bounds on the absolute errors incurred
  by rounding.

  Character strings are scanned first for a real part then for an
  imaginary part.  They can use any of three formats:
  \code{"\var{s}\var{a}"}, \code{"\var{t}\var{b}i"}, and
  \code{"\var{s}\var{a}\var{t}\var{b}i"}, where, recursively, each of
  \code{\var{a}} and \code{\var{b}} have the format
  \code{"(\var{k}\var{m}+/-\var{r})"}, defining a ball for each of the
  real and imaginary parts.  \code{\var{k}} and \code{\var{m}} define
  the sign and absolute value of the signed ball midpoints, and
  \code{\var{r}} defines the unsigned ball radii.  \code{\var{k}} can
  be empty if the ball midpoint is \code{NaN} or non-negative.
  \code{\var{s}} and \code{\var{t}} are unary or binary plus or minus
  to be reconciled with \code{\var{k}}; they are optional except in the
  third format where \code{\var{t}} is mandatory.

  The sequences \code{\var{k}\var{m}} and \code{\var{r}} are converted
  using function \code{mpfr_strtofr} from the \acronym{GNU}
  \acronym{MPFR} library with argument \code{base} set to 0; see
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
      \code{signature(x = "acb")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{tcrossprod}}}{
      \code{signature(x = "acb", y = "acb")}:\cr
      \code{signature(x = "acb", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "acb")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
    \item{\code{\link{+}}}{
      \code{signature(e1 = "acb", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{\link{-}}}{
      \code{signature(e1 = "acb", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{\link[methods]{Complex}}}{
      \code{signature(z = "acb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math}}}{
      \code{signature(x = "acb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions
      \code{floor}, \code{ceiling}, \code{trunc}, \code{cummin},
      \code{cummax} are not implemented.}
    \item{\code{\link[methods]{Math2}}}{
      \code{signature(x = "acb")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{\link[methods]{Ops}}}{
      \code{signature(e1 = "acb", e2 = "acb")}:\cr
      \code{signature(e1 = "acb", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "acb")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
    \item{\code{\link[methods]{Summary}}}{
      \code{signature(x = "acb")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{acb} vector of length 1 or 2 (\code{sum}, \code{prod}).
      Member functions \code{min}, \code{max}, \code{range} are not
      implemented.}
    \item{\code{\link{anyNA}}}{
      \code{signature(x = "acb")}:\cr
      returns \code{TRUE} if any element of \code{x} has real or
      imaginary part with midpoint \code{NaN}, \code{FALSE} otherwise.}
    \item{\code{\link{as.vector}}}{
      \code{signature(x = "acb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a complex
      vector containing the result of converting the midpoints of the
      real and imaginary parts of \code{x} to the range of double,
      rounding if the value is not exactly representable in double
      precision.  The rounding mode is to the nearest representable
      number (with precedence to even significands in case of ties),
      unless a midpoint exceeds \code{.Machine[["double.xmax"]]} in
      absolute value, in which case \code{-Inf} or \code{Inf} is
      introduced with a warning.  Coercion to types \code{"character"},
      \code{"symbol"} (synonym \code{"name"}), \code{"pairlist"},
      \code{"list"}, and \code{"expression"}, which are not
      \dQuote{number-like}, is handled specially.  See also
      \code{\link{asVector}}.}
    \item{\code{\link{backsolve}}}{
      \code{signature(r = "acb", x = "acb")}:\cr
      \code{signature(r = "acb", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "acb")}:\cr
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
    \item{\code{\link{chol}}}{
      \code{signature(x = "acb")}:\cr
      returns the upper triangular Cholesky factor of the positive
      definite matrix whose upper triangular part is the upper
      triangular part of \code{x} (discarding imaginary parts of
      diagonal entries).}
    \item{\code{\link{chol2inv}}}{
      \code{signature(x = "acb")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x} (discarding imaginary parts of diagonal entries).}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "acb")}:\cr
      returns the value of \code{acb(from)}.}
    \item{\code{\link{colSums}}, \code{\link{colMeans}}}{
      \code{signature(x = "acb")}:\cr
      returns an \code{acb} vector or array containing the column sums
      or means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{\link{det}}}{
      \code{signature(x = "arb")}:\cr
      returns the determinant of \code{x} as an \code{acb} vector of
      length 1.}
    \item{\code{\link{determinant}}}{
      \code{signature(x = "acf")}:\cr
      returns a list with components \code{modulus} and \code{argument}
      specifying the determinant of \code{x}, following the documented
      behaviour of the \pkg{base} function (except for the use of
      argument instead of sign).}
    \item{\code{\link{diff}}}{
      \code{signature(x = "acb")}:\cr
      returns a vector storing lagged differences of the elements of
      \code{x} or (if \code{x} is a matrix) a matrix storing lagged
      differences of the rows of \code{x}, following the documented
      behaviour of the S3 default method.}
    \item{\code{\link[stats]{diffinv}}}{
      \code{signature(x = "acb")}:\cr
      returns the vector or matrix \code{y} such that
      \code{x = diff(y, \dots)}, following the documented behaviour of
      the S3 default method.}
    \item{\code{\link{format}}}{
      \code{signature(x = "acb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)+(m +/- r)i"} and scientific format for
      each \code{m} and \code{r}.  Optional arguments control the
      output; see \code{\link{format-methods}}.}
    \item{\code{\link{is.finite}}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      do not have real or imaginary part with midpoint \code{NaN},
      \code{-Inf}, or \code{Inf} or radius \code{Inf}.}
    \item{\code{\link{is.infinite}}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part with midpoint \code{-Inf} or
      \code{Inf} or radius \code{Inf}.}
    \item{\code{\link{is.na}}, \code{\link{is.nan}}}{
      \code{signature(x = "acb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have real or imaginary part with midpoint \code{NaN}.}
    \item{\code{\link{is.unsorted}}}{
      \code{signature(x = "acb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}; see \code{xtfrm} below.}
    \item{\code{\link{log}}}{
      \code{signature(x = "acb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{\link{mean}}}{
      \code{signature(x = "acb")}:\cr
      returns the arithmetic mean.}
    \item{\code{\link{rowSums}}, \code{\link{rowMeans}}}{
      \code{signature(x = "acb")}:\cr
      returns an \code{acb} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{solve}}}{
      \code{signature(a = "acb", b = "acb")}:\cr
      \code{signature(a = "acb", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "acb")}:\cr
      solution of the general system \code{a \%*\% x = b}.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{b} is missing, then
      the return value is the inverse of \code{a}, as if \code{b} were
      the identity matrix.  Operands are promoted as necessary and must
      be conformable (have compatible dimensions).  Non-array \code{b}
      are handled as \code{length(b)}-by-1 matrices.}
    \item{\code{\link{xtfrm}}}{
      \code{signature(x = "acb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}: \code{a <= b || b <= a} is is not
      \code{TRUE} for all finite \code{a} and \code{b} of class
      \code{arb}.  Thus, direct sorting of \code{acb}, which is based
      on \code{arb}, is not supported.  Users wanting to order the
      \emph{midpoints} of the real and imaginary parts should operate on
      \code{\link{Mid}(\link{Real}(x))} and
      \code{\link{Mid}(\link{Imag}(x))}.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.  Generic functions
  \code{\link{Real}} and \code{\link{Imag}} and their replacement forms
  for getting and setting real and imaginary parts.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/acb.html}

  Johansson, F. (2017).
  Arb: efficient arbitrary-precision midpoint-radius interval
  arithmetic.
  \emph{IEEE Transactions on Computers},
  \emph{66}(8), 1281-1292.
  \doi{10.1109/TC.2017.2690633}
}
\examples{
showClass("acb")
showMethods(classes = "acb")
}
\keyword{classes}
