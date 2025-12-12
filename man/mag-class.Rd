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
\alias{diff,mag-method}
\alias{diffinv,mag-method}
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

mag(x = 0, length = 0L, names = NULL, rnd.mag = NULL)

mag.array(x = 0, dim = length(x), dimnames = NULL, rnd.mag = NULL)
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
  \item{rnd.mag}{
    the rounding mode used for inexact conversion.  \code{NULL} means to
    round according to the global default rounding mode; see
    \code{\link{flintRndMag}}.}
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
  rounded towards or away from zero according to the rounding mode set
  by \code{rnd.mag}.  Imaginary parts of complex numbers are discarded.

  It is guaranteed that the result of conversion is a lower or upper
  bound on the converted value.  It is not guaranteed that the bound is
  optimal; in particular, the result of conversion can be inexact even
  if the converted value is exactly representable.  Indeed, the computed
  bound and the optimal bound can differ by several ulps.  If that seems
  unusual, then note that \code{mag} exists primarily to represent the
  radii of \code{\linkS4class{arb}} and \code{\linkS4class{acb}}, and
  arithmetic involving \code{arb} or \code{acb} benefits from fast and
  \dQuote{precise enough} operations on the radii.

  Character strings are converted using function \code{mpfr_strtofr}
  from the \acronym{GNU} \acronym{MPFR} library with argument
  \code{base} set to 0; see
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
  Methods that return a \code{mag} vector are below marked by an
  asterisk \samp{*} after \code{signature(\dots)}.  Where the generic
  function is defined as evaluating a real-valued mathematical function
  \eqn{F}{F(x)}, these methods compute lower or upper bounds on
  \eqn{|F|}{abs(F(x))}.  In this sense, the marked methods can be seen
  as violating the generic function's \dQuote{contract}.  For
  contract-adhering behaviour, dispatch methods for \code{arf}, e.g.,
  do \code{log(arf(x))} instead of \code{log(x)} for \code{x} of class
  \code{mag}.  The bounds computed by the marked methods are not optimal
  in general; see \sQuote{Conversion}.  Whether lower (as opposed to
  upper) bounds are computed depends on the global default rounding
  mode; see \code{\link{flintRndMag}}.
  \describe{
    \item{\code{\link{!}}}{
      \code{signature(x = "mag")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{\link{\%*\%}}, \code{\link{crossprod}}, \code{\link{tcrossprod}}}{
      \code{signature(x = "mag", y = "mag")}:\cr
      \code{signature(x = "mag", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "mag")}:\cr
      coerces the \code{mag} operand to class \code{\linkS4class{arf}},
      \code{\linkS4class{acf}}, \code{\linkS4class{arb}}, or
      \code{\linkS4class{acb}} (depending on the class of the other
      operand) and dispatches.}
    \item{\code{\link{+}}, \code{\link{-}}}{
      \code{signature(e1 = "mag", e2 = "missing")*}:\cr
      returns a copy of the argument.}
    \item{\code{\link[methods]{Complex}}}{
      \code{signature(z = "mag")*}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math}}}{
      \code{signature(x = "mag")*}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{\link[methods]{Math2}}}{
      \code{signature(x = "mag")*}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{\link[methods]{Ops}}}{
      \code{signature(e1 = "mag", e2 = "mag")*}:\cr
      \code{signature(e1 = "mag", e2 = "ANY")}:\cr
      \code{signature(e1 = "ANY", e2 = "mag")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  Operands are promoted as necessary.
      Array operands must be conformable (have identical dimensions).
      Non-array operands are recycled.  For arithmetic, the return value
      is a \code{mag} vector if and only if both operands are \code{mag}
      vectors.}
    \item{\code{\link[methods]{Summary}}}{
      \code{signature(x = "mag")*}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or a
      \code{mag} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{\link{anyNA}}}{
      \code{signature(x = "mag")}:\cr
      returns \code{FALSE}, as \code{mag} has no representation for
      \code{NaN}.}
    \item{\code{\link{as.vector}}}{
      \code{signature(x = "mag")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding away from zero though
      not always to the nearest greater number.  Coercion to types
      \code{"character"}, \code{"symbol"} (synonym \code{"name"}),
      \code{"pairlist"}, \code{"list"}, and \code{"expression"}, which
      are not \dQuote{number-like}, is handled specially.  See also
      \code{\link{asVector}}.}
    \item{\code{\link{backsolve}}}{
      \code{signature(r = "mag", x = "mag")}:\cr
      \code{signature(r = "mag", x = "ANY")}:\cr
      \code{signature(r = "ANY", x = "mag")}:\cr
      coerces the \code{mag} operand to class \code{\linkS4class{arf}},
      \code{\linkS4class{acf}}, \code{\linkS4class{arb}}, or
      \code{\linkS4class{acb}} (depending on the class of the other
      operand) and dispatches.}
    \item{\code{\link{chol}}, \code{\link{chol2inv}}}{
      \code{signature(x = "mag")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "mag")*}:\cr
      returns the value of \code{mag(from)}.}
    \item{\code{\link{colSums}}, \code{\link{colMeans}}}{
      \code{signature(x = "mag")*}:\cr
      returns a \code{mag} vector or array containing the column sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{1:dims}.}
    \item{\code{\link{det}}, \code{\link{determinant}}, \code{\link{diff}}, \code{\link[stats]{diffinv}}}{
      \code{signature(x = "mag")}:\cr
      coerces \code{x} to class \code{\linkS4class{arf}} and
      dispatches.}
    \item{\code{\link{format}}}{
      \code{signature(x = "mag")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{\link{is.finite}}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are not \code{Inf}.}
    \item{\code{\link{is.infinite}}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{Inf}.}
    \item{\code{\link{is.na}}, \code{\link{is.nan}}}{
      \code{signature(x = "mag")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{mag} has no representation for \code{NaN}.}
    \item{\code{\link{is.unsorted}}}{
      \code{signature(x = "mag")}:\cr
      returns a logical indicating if \code{x} is not sorted in
      nondecreasing order (increasing order if optional argument
      \code{strictly} is set to \code{TRUE}).}
    \item{\code{\link{log}}}{
      \code{signature(x = "mag")*}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
    \item{\code{\link{mean}}}{
      \code{signature(x = "mag")*}:\cr
      returns the arithmetic mean.}
    \item{\code{\link{rowSums}}, \code{\link{rowMeans}}}{
      \code{signature(x = "mag")*}:\cr
      returns a \code{mag} vector or array containing the row sums or
      means of \code{x}, defined as sums or means over dimensions
      \code{(dims+1):length(dim(x))}.}
    \item{\code{\link{solve}}}{
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

(ornd <- flintRndMag()) # getting the original rounding mode

## Output gives 4 significant digits by default
(magpi <- mag(pi))

## Number of reliable digits is 8 == floor((30 - 1) * log10(2))
as.character(magpi)

## Integers in range of 30-bit unsigned type are converted exactly
(x0 <- -1L:1L * (0x1p+30L - 1L))
mag(x0) == abs(x0) #     all TRUE
(x1 <- -1L:1L * (0x1p+30L - 0L))
mag(x1) == abs(x1) # not all TRUE

## Conversion of most other input is influenced by the rounding mode
flintRndMag("A")
mag( 1) >  1
mag(pi) > pi
flintRndMag("Z")
mag( 1) <  1
mag(pi) < pi

## Computing bounds on composite functions needs care;
## e.g., for an upper bound on abs(tan(2)) = sin(2)/abs(cos(2)):
tt <- mag(2L) # exact
flintRndMag("Z") # some quantities must be rounded towards 0
cos2 <- cos(tt)
flintRndMag("A") # others must be rounded away from 0
sin2 <- sin(tt)
tan2 <- sin2/cos2
tan2    - abs(tan(2))
tan(tt) - abs(tan(2)) # direct hence sharper

flintRndMag(ornd) # resetting the original rounding mode
}
\keyword{classes}
