\name{arb-class}
\alias{arb-class}
\alias{arb}
\alias{arb.array}
\alias{!,arb-method}
\alias{\%*\%,ANY,arb-method}
\alias{\%*\%,arb,ANY-method}
\alias{\%*\%,arb,ulong-method}
\alias{\%*\%,arb,slong-method}
\alias{\%*\%,arb,fmpz-method}
\alias{\%*\%,arb,fmpq-method}
\alias{\%*\%,arb,mag-method}
\alias{\%*\%,arb,arf-method}
\alias{\%*\%,arb,acf-method}
\alias{\%*\%,arb,arb-method}
\alias{\%*\%,arb,acb-method}
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
\alias{backsolve,ANY,arb-method}
\alias{backsolve,arb,ANY-method}
\alias{backsolve,arb,ulong-method}
\alias{backsolve,arb,slong-method}
\alias{backsolve,arb,fmpz-method}
\alias{backsolve,arb,fmpq-method}
\alias{backsolve,arb,mag-method}
\alias{backsolve,arb,arf-method}
\alias{backsolve,arb,acf-method}
\alias{backsolve,arb,arb-method}
\alias{backsolve,arb,acb-method}
\alias{chol,arb-method}
\alias{chol2inv,arb-method}
\alias{coerce,ANY,arb-method}
\alias{colMeans,arb-method}
\alias{colSums,arb-method}
\alias{crossprod,ANY,arb-method}
\alias{crossprod,arb,ANY-method}
\alias{crossprod,arb,ulong-method}
\alias{crossprod,arb,slong-method}
\alias{crossprod,arb,fmpz-method}
\alias{crossprod,arb,fmpq-method}
\alias{crossprod,arb,mag-method}
\alias{crossprod,arb,arf-method}
\alias{crossprod,arb,acf-method}
\alias{crossprod,arb,arb-method}
\alias{crossprod,arb,acb-method}
\alias{det,arb-method}
\alias{determinant,arb-method}
\alias{diff,arb-method}
\alias{diffinv,arb-method}
\alias{is.finite,arb-method}
\alias{is.infinite,arb-method}
\alias{is.na,arb-method}
\alias{is.nan,arb-method}
\alias{is.unsorted,arb-method}
\alias{log,arb-method}
\alias{mean,arb-method}
\alias{rowMeans,arb-method}
\alias{rowSums,arb-method}
\alias{solve,ANY,arb-method}
\alias{solve,arb,ANY-method}
\alias{solve,arb,ulong-method}
\alias{solve,arb,slong-method}
\alias{solve,arb,fmpz-method}
\alias{solve,arb,fmpq-method}
\alias{solve,arb,mag-method}
\alias{solve,arb,arf-method}
\alias{solve,arb,acf-method}
\alias{solve,arb,arb-method}
\alias{solve,arb,acb-method}
\alias{tcrossprod,ANY,arb-method}
\alias{tcrossprod,arb,ANY-method}
\alias{tcrossprod,arb,ulong-method}
\alias{tcrossprod,arb,slong-method}
\alias{tcrossprod,arb,fmpz-method}
\alias{tcrossprod,arb,fmpq-method}
\alias{tcrossprod,arb,mag-method}
\alias{tcrossprod,arb,arf-method}
\alias{tcrossprod,arb,acf-method}
\alias{tcrossprod,arb,arb-method}
\alias{tcrossprod,arb,acb-method}
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
## Class generator functions

arb(x = 0, length = 0L, names = NULL, mid = 0, rad = 0,
    prec = NULL)

arb.array(x = 0, dim = length(x), dimnames = NULL, mid = 0, rad = 0,
          prec = NULL)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{arb}.}
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
  \item{mid, rad}{
    atomic or \code{\linkS4class{flint}} vectors containing data for
    conversion to \code{\linkS4class{arf}} and \code{\linkS4class{mag}},
    respectively.  Use these for initialization \dQuote{by parts}
    (midpoint and radius).}
  \item{prec}{
    the precision used for conversion of midpoints.  \code{NULL} means
    to convert exactly if possible and to round to the global default
    precision otherwise; see \code{\link{flintPrec}}.}
}
\value{
  An \code{arb} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has six distinct usages:
  \preformatted{arb()
arb(length=)
arb(x)
arb(x, length=)
arb(mid=, mid=)
arb(mid=, mid=, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  The fifth and sixth usages, in which either of \code{mid} and
  \code{rad} can be missing, use \code{\link{arf}(mid)} and
  \code{\link{mag}(rad)} to separately initialize the midpoints and
  radii of the \code{arb} return value.

  Attempts to recycle \code{mid}, \code{rad}, or \code{x} of length
  zero to nonzero length are an error.

  Usage of \code{arb.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real parts of complex numbers are rounded according
  to the precision set by \code{prec}, always in the direction of zero.
  Ball midpoints are the numbers obtained by rounding.  Ball radii are
  upper bounds on the absolute errors incurred by rounding.  Imaginary
  parts of complex numbers are discarded.

  Character strings are scanned for format
  \code{"\var{s}(\var{k}\var{m}+/-\var{r})"}, where \code{\var{k}} and
  \code{\var{m}} define the sign and absolute value of the signed ball
  midpoint, and \code{\var{r}} defines the unsigned ball radius.
  \code{\var{k}} can be empty if the ball midpoint is \code{NaN} or
  non-negative.  \code{\var{s}} is an optional unary plus or minus to be
  reconciled with \code{\var{k}}.

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
      \code{signature(x = "arb")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{tcrossprod}}}{
      \code{signature(x = "arb", y = "arb")}:\cr
      \code{signature(x = "arb", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "arb")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
    \item{\code{\link{+}}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{\link{-}}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{\link[methods]{Complex}}}{
      \code{signature(z = "arb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math}}}{
      \code{signature(x = "arb")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math2}}}{
      \code{signature(x = "arb")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{\link[methods]{Ops}}}{
      \code{signature(e1 = "arb", e2 = "arb")}:\cr
      \code{signature(e1 = "arb", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "arb")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
    \item{\code{\link[methods]{Summary}}}{
      \code{signature(x = "arb")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{arb} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{\link{anyNA}}}{
      \code{signature(x = "arb")}:\cr
      returns \code{TRUE} if any element of \code{x} has midpoint
      \code{NaN}, \code{FALSE} otherwise.}
    \item{\code{\link{as.vector}}}{
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
      handled specially.  See also \code{\link{asVector}}.}
    \item{\code{\link{backsolve}}}{
      \code{signature(r = "arb", x = "arb")}:\cr
      \code{signature(r = "arb", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "arb")}:\cr
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
      \code{signature(x = "arb")}:\cr
      returns the upper triangular Cholesky factor of the positive
      definite matrix whose upper triangular part is the upper
      triangular part of \code{x}.}
    \item{\code{\link{chol2inv}}}{
      \code{signature(x = "arb")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x}.}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "arb")}:\cr
      returns the value of \code{arb(from)}.}
    \item{\code{\link{colSums}}, \code{\link{colMeans}}}{
      \code{signature(x = "arb")}:\cr
      returns an \code{arb} vector or array containing the column sums
      or means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{\link{det}}}{
      \code{signature(x = "arb")}:\cr
      returns the determinant of \code{x} as an \code{arb} vector of
      length 1.}
    \item{\code{\link{determinant}}}{
      \code{signature(x = "arb")}:\cr
      returns a list with components \code{modulus} and \code{sign}
      specifying the determinant of \code{x}, following the documented
      behaviour of the \pkg{base} function.  The sign is \code{NA} if
      the interval computed by \code{det(x)} contains both negative
      numbers and positive numbers.}
    \item{\code{\link{diff}}}{
      \code{signature(x = "arb")}:\cr
      returns a vector storing lagged differences of the elements of
      \code{x} or (if \code{x} is a matrix) a matrix storing lagged
      differences of the rows of \code{x}, following the documented
      behaviour of the S3 default method.}
    \item{\code{\link[stats]{diffinv}}}{
      \code{signature(x = "arb")}:\cr
      returns the vector or matrix \code{y} such that
      \code{x = diff(y, \dots)}, following the documented behaviour of
      the S3 default method.}
    \item{\code{\link{format}}}{
      \code{signature(x = "arb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)"} and scientific format for \code{m} and
      \code{r}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{\link{is.finite}}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      do not have midpoint \code{NaN}, \code{-Inf}, or \code{Inf} or
      radius \code{Inf}.}
    \item{\code{\link{is.infinite}}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have midpoint \code{-Inf} or \code{Inf} or radius \code{Inf}.}
    \item{\code{\link{is.na}}, \code{\link{is.nan}}}{
      \code{signature(x = "arb")}:\cr
      returns a logical vector indicating which elements of \code{x}
      have midpoint \code{NaN}.}
    \item{\code{\link{is.unsorted}}}{
      \code{signature(x = "arb")}:\cr
      signals an error indicating that \code{<=} is not a total order
      on the range of \code{arb}; see \code{xtfrm} below.}
    \item{\code{\link{log}}}{
      \code{signature(x = "arb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{\link{mean}}}{
      \code{signature(x = "arb")}:\cr
      returns the arithmetic mean.}
    \item{\code{\link{rowSums}}, \code{\link{rowMeans}}}{
      \code{signature(x = "arb")}:\cr
      returns an \code{arb} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{solve}}}{
      \code{signature(a = "arb", b = "arb")}:\cr
      \code{signature(a = "arb", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "arb")}:\cr
      solution of the general system \code{a \%*\% x = b}.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{b} is missing, then
      the return value is the inverse of \code{a}, as if \code{b} were
      the identity matrix.  Operands are promoted as necessary and must
      be conformable (have compatible dimensions).  Non-array \code{b}
      are handled as \code{length(b)}-by-1 matrices.}
    \item{\code{\link{xtfrm}}}{
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
