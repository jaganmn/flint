\name{fmpq-class}
\alias{fmpq-class}
\alias{fmpq}
\alias{fmpq.array}
\alias{!,fmpq-method}
\alias{\%*\%,ANY,fmpq-method}
\alias{\%*\%,fmpq,ANY-method}
\alias{\%*\%,fmpq,ulong-method}
\alias{\%*\%,fmpq,slong-method}
\alias{\%*\%,fmpq,fmpz-method}
\alias{\%*\%,fmpq,fmpq-method}
\alias{\%*\%,fmpq,mag-method}
\alias{\%*\%,fmpq,arf-method}
\alias{\%*\%,fmpq,acf-method}
\alias{\%*\%,fmpq,arb-method}
\alias{\%*\%,fmpq,acb-method}
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
\alias{backsolve,ANY,fmpq-method}
\alias{backsolve,fmpq,ANY-method}
\alias{backsolve,fmpq,ulong-method}
\alias{backsolve,fmpq,slong-method}
\alias{backsolve,fmpq,fmpz-method}
\alias{backsolve,fmpq,fmpq-method}
\alias{backsolve,fmpq,mag-method}
\alias{backsolve,fmpq,arf-method}
\alias{backsolve,fmpq,acf-method}
\alias{backsolve,fmpq,arb-method}
\alias{backsolve,fmpq,acb-method}
\alias{chol,fmpq-method}
\alias{chol2inv,fmpq-method}
\alias{coerce,ANY,fmpq-method}
\alias{colMeans,fmpq-method}
\alias{colSums,fmpq-method}
\alias{crossprod,ANY,fmpq-method}
\alias{crossprod,fmpq,ANY-method}
\alias{crossprod,fmpq,ulong-method}
\alias{crossprod,fmpq,slong-method}
\alias{crossprod,fmpq,fmpz-method}
\alias{crossprod,fmpq,fmpq-method}
\alias{crossprod,fmpq,mag-method}
\alias{crossprod,fmpq,arf-method}
\alias{crossprod,fmpq,acf-method}
\alias{crossprod,fmpq,arb-method}
\alias{crossprod,fmpq,acb-method}
\alias{det,fmpq-method}
\alias{determinant,fmpq-method}
\alias{is.finite,fmpq-method}
\alias{is.infinite,fmpq-method}
\alias{is.na,fmpq-method}
\alias{is.nan,fmpq-method}
\alias{is.unsorted,fmpq-method}
\alias{mean,fmpq-method}
\alias{rowMeans,fmpq-method}
\alias{rowSums,fmpq-method}
\alias{solve,ANY,fmpq-method}
\alias{solve,fmpq,ANY-method}
\alias{solve,fmpq,ulong-method}
\alias{solve,fmpq,slong-method}
\alias{solve,fmpq,fmpz-method}
\alias{solve,fmpq,fmpq-method}
\alias{solve,fmpq,mag-method}
\alias{solve,fmpq,arf-method}
\alias{solve,fmpq,acf-method}
\alias{solve,fmpq,arb-method}
\alias{solve,fmpq,acb-method}
\alias{tcrossprod,ANY,fmpq-method}
\alias{tcrossprod,fmpq,ANY-method}
\alias{tcrossprod,fmpq,ulong-method}
\alias{tcrossprod,fmpq,slong-method}
\alias{tcrossprod,fmpq,fmpz-method}
\alias{tcrossprod,fmpq,fmpq-method}
\alias{tcrossprod,fmpq,mag-method}
\alias{tcrossprod,fmpq,arf-method}
\alias{tcrossprod,fmpq,acf-method}
\alias{tcrossprod,fmpq,arb-method}
\alias{tcrossprod,fmpq,acb-method}
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
## Class generator functions

fmpq(x = 0L, length = 0L, names = NULL, num = 0L, den = 1L)

fmpq.array(x = 0L, dim = length(x), dimnames = NULL, num = 0L, den = 1L)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{fmpq}.}
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
  \item{num, den}{
    atomic or \code{\linkS4class{flint}} vectors containing data for
    conversion to \code{\linkS4class{fmpz}}.  Use these instead of
    \code{x} for initialization \dQuote{by parts} (numerator and
    denominator).}
}
\value{
  An \code{fmpq} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has six distinct usages:
  \preformatted{fmpq()
fmpq(length=)
fmpq(x)
fmpq(x, length=)
fmpq(num=, den=)
fmpq(num=, den=, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  The fifth and sixth usages, in which either of \code{num} and
  \code{den} can be missing, use \code{\link{fmpz}(num)} and
  \code{\link{fmpz}(den)} to separately initialize the numerators and
  denominators of the \code{fmpq} return value.

  Attempts to recycle \code{num}, \code{den}, or \code{x} of length zero
  to nonzero length are an error.

  Usage of \code{fmpq.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real parts of complex numbers are converted exactly,
  as floating-point numbers are rational by definition.  Imaginary parts
  of complex numbers are discarded.

  Character strings are converted using function \code{mpq_set_str} from
  the \acronym{GNU} \acronym{MP} library with argument \code{base} set
  to 0; see \url{https://gmplib.org/manual/Initializing-Rationals}.

  An error is signaled if elements of \code{num}, \code{den}, or
  \code{x} are \code{NaN}, \code{-Inf}, or \code{Inf} or if elements of
  \code{den} are 0.
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
      \code{signature(x = "fmpq")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\%*\%}, \code{crossprod}, \code{tcrossprod}}{
      \code{signature(x = "fmpq", y = "fmpq")}:\cr
      \code{signature(x = "fmpq", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "fmpq")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
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
      \code{signature(e1 = "fmpq", e2 = "fmpq")}:\cr
      \code{signature(e1 = "fmpq", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "fmpq")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
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
      \dQuote{number-like}, is handled specially.  See also
      \code{\link{asVector}}.}
    \item{\code{backsolve}}{
      \code{signature(r = "fmpq", x = "fmpq")}:\cr
      \code{signature(r = "fmpq", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "fmpq")}:\cr
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
      Non-array \code{x} are handled as \code{length(x)}-by-1 matrices.
      If \code{r} and (if not missing) \code{x} are both formally
      rational, then the solution is exact and the return value is an
      \code{\linkS4class{fmpq}} matrix.}
    \item{\code{chol}}{
      \code{signature(x = "fmpq")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{chol2inv}}{
      \code{signature(x = "fmpq")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x}.  The return value is the exact inverse, being computed
      as \code{tcrossprod(backsolve(x))}.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "fmpq")}:\cr
      returns the value of \code{fmpq(from)}.}
    \item{\code{colSums}, \code{colMeans}}{
      \code{signature(x = "fmpq")}:\cr
      returns an \code{fmpq} vector or array containing the column sums
      or means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{colSums}}{
      \code{signature(x = "fmpq")}:\cr
      returns an \code{fmpq} vector or array containing the column sums
      of \code{x}, defined as sums over dimensions \code{1:dims}.}
    \item{\code{colMeans}}{
      \code{signature(x = "fmpq")}:\cr
      returns an \code{\linkS4class{fmpq}} vector or array containing
      the column means of \code{x}, defined as means over dimensions
      \code{1:dims}.}
    \item{\code{det}}{
      \code{signature(x = "fmpq")}:\cr
      returns the determinant of \code{x} as an \code{fmpq} vector of
      length 1.}
    \item{\code{determinant}}{
      \code{signature(x = "fmpq")}:\cr
      returns a list with components \code{modulus} and \code{sign}
      specifying the determinant of \code{x}, following the \pkg{base}
      function, hence see \code{\link{determinant}}.  Note that
      \code{det(x)} and \code{determinant(x, logarithm = FALSE)} are
      exact, but \code{determinant(x)} is not in general due to
      rounding.}
    \item{\code{format}}{
      \code{signature(x = "fmpq")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"p/q"}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
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
    \item{\code{rowSums}, \code{rowMeans}}{
      \code{signature(x = "fmpq")}:\cr
      returns an \code{fmpq} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{solve}}{
      \code{signature(a = "fmpq", b = "fmpq")}:\cr
      \code{signature(a = "fmpq", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "fmpq")}:\cr
      solution of the general system \code{a \%*\% x = b}.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  If \code{b} is missing, then
      the return value is the inverse of \code{a}, as if \code{b} were
      the identity matrix.  Operands are promoted as necessary and must
      be conformable (have compatible dimensions).  Non-array \code{b}
      are handled as \code{length(b)}-by-1 matrices.  If \code{a} and
      (if not missing) \code{b} are both formally rational, then the
      solution is exact and the return value is an
      \code{\linkS4class{fmpq}} matrix.}
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
