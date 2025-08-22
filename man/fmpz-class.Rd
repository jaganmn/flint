\name{fmpz-class}
\alias{fmpz-class}
\alias{fmpz}
\alias{.fmpz}
\alias{.fmpz.array}
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
\alias{is.finite,fmpz-method}
\alias{is.infinite,fmpz-method}
\alias{is.na,fmpz-method}
\alias{is.nan,fmpz-method}
\alias{is.unsorted,fmpz-method}
\alias{mean,fmpz-method}
\alias{rowMeans,fmpz-method}
\alias{rowSums,fmpz-method}
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

.fmpz(x = 0L, length = 0L, names = NULL)

.fmpz.array(x = 0L, dim = length(x), dimnames = NULL)
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
  There are four usages of \code{.fmpz}: \preformatted{.fmpz()
.fmpz(length=)
.fmpz(x)
.fmpz(x, length=)}
  The first usage generates an empty vector.  The second usage generates
  a zero vector of the indicated length.  The third usage converts
  \code{x}, preserving dimensions, dimension names, and names.  The
  fourth usage converts \code{x}, recycling its elements to the
  indicated length and discarding its dimensions, dimension names, and
  names.  Attempts to recycle \code{x} of length zero to nonzero length
  are an error.

  Usage of \code{.fmpz.array} is modelled after \code{\link{array}}.
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
    \item{\code{!}}{
      \code{signature(x = "fmpz")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\%*\%}, \code{crossprod}, \code{tcrossprod}}{
      \code{signature(x = "ANY", y = "fmpz")}:\cr
      \code{signature(x = "fmpz", y = "ANY")}:\cr
      matrix products.  The \dQuote{other} operand must be atomic or
      inherit from virtual class \code{\linkS4class{flint}}.
      \code{crossprod} and \code{tcrossprod} behave as if \code{y = x}
      when \code{y} is missing or \code{NULL}.  Operands are promoted as
      necessary and must be conformable (have compatible dimensions).
      Non-array operands of length \code{k} are handled as 1-by-\code{k}
      or \code{k}-by-1 matrices depending on the call.}
    \item{\code{+}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "fmpz", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math}}{
      \code{signature(x = "fmpz")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Member functions requiring
      promotion to a floating-point type may not be implemented.}
    \item{\code{Math2}}{
      \code{signature(x = "fmpz")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "fmpz")}:\cr
      \code{signature(e1 = "fmpz", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.}
    \item{\code{Summary}}{
      \code{signature(x = "fmpz")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{fmpz} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{anyNA}}{
      \code{signature(x = "fmpz")}:\cr
      returns \code{FALSE}, as \code{fmpz} has no representation for
      \code{NaN}.}
    \item{\code{as.vector}}{
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
      \dQuote{number-like}, is handled specially.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "fmpz")}:\cr
      returns the value of \code{.fmpz(from)}.}
    \item{\code{colSums}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{fmpz} vector or array containing the column sums
      of \code{x}, defined as sums over dimensions \code{1:dims}.}
    \item{\code{colMeans}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{\linkS4class{fmpq}} vector or array containing
      the column means of \code{x}, defined as means over dimensions
      \code{1:dims}.}
    \item{\code{format}}{
      \code{signature(x = "fmpz")}:\cr
      returns a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpz")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot and
      (conditionally depending on arguments other than \code{.Object})
      its \code{dim}, \code{dimnames}, and \code{names} slots; see
      see \code{\link{initialize-methods}}.}
    \item{\code{is.finite}}{
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.infinite}, \code{is.na}, \code{is.nan}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{fmpz} has no representation for \code{NaN}, \code{-Inf}, and
      \code{Inf}.}
    \item{\code{is.unsorted}}{
      \code{signature(x = "fmpz")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{mean}}{
      \code{signature(x = "fmpz")}:\cr
      returns the arithmetic mean.  An error is signaled if the argument
      length is 0, because the return type is \code{\linkS4class{fmpq}}
      which cannot represent the result of division by 0.}
    \item{\code{rowSums}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{fmpz} vector or array containing the row sums of
      \code{x}, defined as sums over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{rowMeans}}{
      \code{signature(x = "fmpz")}:\cr
      returns an \code{\linkS4class{fmpq}} vector or array containing
      the row means of \code{x}, defined as means over dimensions
      \code{(dims+1):length(dim(x))}.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/fmpz.html}
}
\examples{
showClass("fmpz")
showMethods(classes = "fmpz")
}
\keyword{classes}
