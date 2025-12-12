\name{fmpz-class}
\alias{fmpz-class}
\alias{fmpz}
\alias{fmpz.array}
\alias{!,fmpz-method}
\alias{\%*\%,ANY,fmpz-method}
\alias{\%*\%,fmpz,ANY-method}
\alias{\%*\%,fmpz,ulong-method}
\alias{\%*\%,fmpz,slong-method}
\alias{\%*\%,fmpz,fmpz-method}
\alias{\%*\%,fmpz,fmpq-method}
\alias{\%*\%,fmpz,mag-method}
\alias{\%*\%,fmpz,arf-method}
\alias{\%*\%,fmpz,acf-method}
\alias{\%*\%,fmpz,arb-method}
\alias{\%*\%,fmpz,acb-method}
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
\alias{backsolve,ANY,fmpz-method}
\alias{backsolve,fmpz,ANY-method}
\alias{backsolve,fmpz,ulong-method}
\alias{backsolve,fmpz,slong-method}
\alias{backsolve,fmpz,fmpz-method}
\alias{backsolve,fmpz,fmpq-method}
\alias{backsolve,fmpz,mag-method}
\alias{backsolve,fmpz,arf-method}
\alias{backsolve,fmpz,acf-method}
\alias{backsolve,fmpz,arb-method}
\alias{backsolve,fmpz,acb-method}
\alias{chol,fmpz-method}
\alias{chol2inv,fmpz-method}
\alias{coerce,ANY,fmpz-method}
\alias{colMeans,fmpz-method}
\alias{colSums,fmpz-method}
\alias{crossprod,ANY,fmpz-method}
\alias{crossprod,fmpz,ANY-method}
\alias{crossprod,fmpz,ulong-method}
\alias{crossprod,fmpz,slong-method}
\alias{crossprod,fmpz,fmpz-method}
\alias{crossprod,fmpz,fmpq-method}
\alias{crossprod,fmpz,mag-method}
\alias{crossprod,fmpz,arf-method}
\alias{crossprod,fmpz,acf-method}
\alias{crossprod,fmpz,arb-method}
\alias{crossprod,fmpz,acb-method}
\alias{det,fmpz-method}
\alias{determinant,fmpz-method}
\alias{diff,fmpz-method}
\alias{diffinv,fmpz-method}
\alias{is.finite,fmpz-method}
\alias{is.infinite,fmpz-method}
\alias{is.na,fmpz-method}
\alias{is.nan,fmpz-method}
\alias{is.unsorted,fmpz-method}
\alias{mean,fmpz-method}
\alias{rowMeans,fmpz-method}
\alias{rowSums,fmpz-method}
\alias{solve,ANY,fmpz-method}
\alias{solve,fmpz,ANY-method}
\alias{solve,fmpz,ulong-method}
\alias{solve,fmpz,slong-method}
\alias{solve,fmpz,fmpz-method}
\alias{solve,fmpz,fmpq-method}
\alias{solve,fmpz,mag-method}
\alias{solve,fmpz,arf-method}
\alias{solve,fmpz,acf-method}
\alias{solve,fmpz,arb-method}
\alias{solve,fmpz,acb-method}
\alias{tcrossprod,ANY,fmpz-method}
\alias{tcrossprod,fmpz,ANY-method}
\alias{tcrossprod,fmpz,ulong-method}
\alias{tcrossprod,fmpz,slong-method}
\alias{tcrossprod,fmpz,fmpz-method}
\alias{tcrossprod,fmpz,fmpq-method}
\alias{tcrossprod,fmpz,mag-method}
\alias{tcrossprod,fmpz,arf-method}
\alias{tcrossprod,fmpz,acf-method}
\alias{tcrossprod,fmpz,arb-method}
\alias{tcrossprod,fmpz,acb-method}
\docType{class}
\title{Arbitrary Precision Signed Integers}
\description{
  Class \code{fmpz} extends virtual class \code{\linkS4class{flint}}.
  It represents vectors of arbitrary precision signed integers.  There
  is no representation for \R{}'s missing value
  \code{\link{NA_integer_}}.
}
\usage{
## Class generator functions

fmpz(x = 0L, length = 0L, names = NULL)

fmpz.array(x = 0L, dim = length(x), dimnames = NULL)
}
\arguments{
  \item{x}{
    an atomic or \code{\linkS4class{flint}} vector containing data for
    conversion to \code{fmpz}.}
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
  An \code{fmpz} vector, possibly an array; see \sQuote{Details}.
}
\details{
  The class generator function has four distinct usages:
  \preformatted{fmpz()
fmpz(length=)
fmpz(x)
fmpz(x, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  Attempts to recycle \code{x} of length zero to nonzero length
  are an error.

  Usage of \code{fmpz.array} is modelled after \code{\link{array}}.
}
\section{Conversion}{
  Real numbers and real parts of complex numbers are rounded in the
  direction of 0.  Imaginary parts of complex numbers are discarded.

  Character strings are converted using function \code{mpz_set_str} from
  the \acronym{GNU} \acronym{MP} library with argument \code{base} set
  to 0; see \url{https://gmplib.org/manual/Assigning-Integers}.

  An error is signaled if elements of \code{x} are \code{NaN},
  \code{-Inf}, or \code{Inf}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}, \code{dim}, \code{dimnames}, \code{names}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{!}}}{
      \code{signature(x = "fmpz")}:\cr
      equivalent to (but faster than) \code{x == 0L}.}
    \item{\code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{tcrossprod}}}{
      \code{signature(x = "fmpz", y = "fmpz")}:\cr
      \code{signature(x = "fmpz", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "fmpz")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
    \item{\code{\link{+}}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{\link{-}}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{\link[methods]{Complex}}}{
      \code{signature(z = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{\link[methods]{Math}}}{
      \code{signature(x = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{\link[methods]{Math2}}}{
      \code{signature(x = "fmpz")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{\link[methods]{Ops}}}{
      \code{signature(e1 = "fmpz", e2 = "fmpz")}:\cr
      \code{signature(e1 = "fmpz", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "fmpz")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
    \item{\code{\link[methods]{Summary}}}{
      \code{signature(x = "fmpz")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{fmpz} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{\link{anyNA}}}{
      \code{signature(x = "fmpz")}:\cr
      returns \code{FALSE}, as \code{fmpz} has no representation for
      \code{NaN}.}
    \item{\code{\link{as.vector}}}{
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
      \dQuote{number-like}, is handled specially.  See also
      \code{\link{asVector}}.}
    \item{\code{\link{backsolve}}}{
      \code{signature(r = "fmpz", x = "fmpz")}:\cr
      \code{signature(r = "fmpz", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "fmpz")}:\cr
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
    \item{\code{\link{chol}}}{
      \code{signature(x = "fmpz")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{\link{chol2inv}}}{
      \code{signature(x = "fmpz")}:\cr
      returns the inverse of the positive definite matrix whose upper
      triangular Cholesky factor is the upper triangular part of
      \code{x}.  The return value is the exact inverse, being computed
      as \code{tcrossprod(backsolve(x))}.}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "fmpz")}:\cr
      returns the value of \code{fmpz(from)}.}
    \item{\code{\link{colSums}}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{fmpz} vector or array containing the column sums
      of \code{x}, defined as sums over dimensions \code{1:dims}.}
    \item{\code{\link{colMeans}}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{\linkS4class{fmpq}} vector or array containing
      the column means of \code{x}, defined as means over dimensions
      \code{1:dims}.}
    \item{\code{\link{det}}}{
      \code{signature(x = "fmpz")}:\cr
      returns the determinant of \code{x} as an \code{fmpz} vector of
      length 1.}
    \item{\code{\link{determinant}}}{
      \code{signature(x = "fmpz")}:\cr
      returns a list with components \code{modulus} and \code{sign}
      specifying the determinant of \code{x}, following the documented
      behaviour of the \pkg{base} function.  Note that \code{det(x)} and
      \code{determinant(x, logarithm = FALSE)} are exact, but
      \code{determinant(x)} is not in general due to rounding.}
    \item{\code{\link{diff}}}{
      \code{signature(x = "fmpz")}:\cr
      returns a vector storing lagged differences of the elements of
      \code{x} or (if \code{x} is a matrix) a matrix storing lagged
      differences of the rows of \code{x}, following the documented
      behaviour of the S3 default method.}
    \item{\code{\link[stats]{diffinv}}}{
      \code{signature(x = "fmpz")}:\cr
      returns the vector or matrix \code{y} such that
      \code{x = diff(y, \dots)}, following the documented behaviour of
      the S3 default method.}
    \item{\code{\link{format}}}{
      \code{signature(x = "fmpz")}:\cr
      returns a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
    \item{\code{\link{is.finite}}}{
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{\link{is.infinite}}, \code{\link{is.na}}, \code{\link{is.nan}}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{\link{is.unsorted}}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{\link{mean}}}{
      \code{signature(x = "fmpz")}:\cr
      returns the arithmetic mean.  An error is signaled if the argument
      length is 0, because the return type is \code{\linkS4class{fmpq}}
      which cannot represent the result of division by 0.}
    \item{\code{\link{rowSums}}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{fmpz} vector or array containing the row sums of
      \code{x}, defined as sums over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{rowMeans}}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{\linkS4class{fmpq}} vector or array containing
      the row means of \code{x}, defined as means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{solve}}}{
      \code{signature(a = "fmpz", b = "fmpz")}:\cr
      \code{signature(a = "fmpz", b = "ANY")}:\cr
      \code{signature(a = "ANY", b = "fmpz")}:\cr
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
  Virtual class \code{\linkS4class{flint}} and its nonvirtual subclass
  \code{\linkS4class{fmpq}}, the latter representing rational numbers as
  constrained pairs of \code{fmpz}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
showMethods(classes = "fmpz")

    fmpz(0x1p+1023)  # with(.Machine, 2L^(double.max.exp-1L))
try(fmpz(0x1p+1024)) # no representation for Inf after
                     # floating-point overflow

    fmpz(2L)^1024L  # powers are rational in general (signed exponent)
Num(fmpz(2L)^1024L) # get the numerator

## Allocation in bytes as a function of the most significant bit (B)
##     B <= M - 2: a limb
##     B >  M - 2: a limb, an 'mpz' struct, and N more limbs
## where M := flintABI(), N := floor((B - 1)/M) + 1
B <- seq_len(1024L)
rle(vapply(as.list(Num(fmpz(2L)^(B - 1L))), flintSize, 0))

## Conversion of decimal format strings
fmpz(       "1234567890"        )
fmpz(strrep("1234567890", 1L:6L))

## Conversion of hexadecimal format strings
hs <- paste0("-0x1", strrep("0", 256L))
hz <- fmpz(hs)
stopifnot(identical(hz, Num(-fmpz(2L)^1024L)),
          identical(format(hz, base = 16L), sub("0x", "", hs)))

## Exact 'sqrt' preserves class, hence requires perfect squares
    sqrt(fmpz(81L))  # ok
try(sqrt(fmpz(80L))) # error
sqrt(arf(fmpz(80L))) # ok, thanks to coercion to floating-point type

## Quotients are formally rational
(J <- fmpz.array(0L:11L, c(6L, 2L), list(NULL, col = c("aa", "bb"))))
J/2L
J/1L
try(J/0L) # NaN, -Inf, and Inf have no representation
rowMeans(J)
colMeans(J)
summary(J)
summary(J, quantile.type = 6L) # types 1 through 9 are all implemented

## Floored integer division
p <- fmpz(-127L)
q <- c(-(3L:1L), 1L:3L)
stopifnot(identical(p \%/\% q * q + p \%\% q, rep(p, 6L)))

## Exact rational solution of linear systems with integer coefficients
(A3 <- diag(c(0x1p+53, 1, 1)))
try(solve(A3)) # system is computationally singular
(A3 <- diag(Num(fmpz(2L)^c(53L, 0L, 0L))))
(A3inv <- solve(A3))
(I3 <- diag(1L, 3L, 3L))
(b3 <- -1L:1L)
stopifnot(identical(solve(A3, I3), A3inv),
          identical(solve(A3, b3), A3inv \%*\% b3),
          identical(solve(A3inv), fmpq(A3)),
          identical(A3 \%*\% A3inv, fmpq(I3)))

## Conversion to "double" rounds towards zero
(z <- Num(fmpz(2L)^54L))
(off <- fmpz(0L:8L))
(offZ <- 4L * (off \%/\% 4L))
stopifnot(identical(fmpz(as.double(z + off)) - z, offZ))

## Conversion to "arf" is exact *by default*
stopifnot(identical(fmpz(arf(z + off)) - z, off),
          identical(fmpz(arf(z + off, prec = 53L, rnd = "Z")) - z, offZ))
}
\keyword{classes}
