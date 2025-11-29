\name{flint-package}
\alias{flint-package}
\alias{flintABI}
\alias{flintBits}
\alias{flintBitsAccurate}
\alias{flintClass}
\alias{flintLength}
\alias{flintPrec}
\alias{flintRnd}
\alias{flintRndMag}
\alias{flintSize}
\alias{flintTriple}
\alias{flintVersion}
\alias{.initForeign}
\docType{package}
\title{\R{} Package \pkg{flint}}
\description{
  An \R{} interface to \acronym{FLINT}, a \proglang{C} library for
  number theory.
}
\usage{
flintABI()
flintBits(object)
flintBitsAccurate(object)
flintClass(object)
flintLength(object, exact = TRUE)
flintPrec(prec = NULL)
flintRnd(rnd = NULL)
flintRndMag(rnd.mag = NULL)
flintSize(object)
flintTriple(object)
flintVersion()

.initForeign(which = c("Rmpfr", "gmp"),
             where = topenv(parent.frame()))
}
\arguments{
  \item{object}{
    an \R{} object, typically inheriting from virtual class
    \code{\linkS4class{flint}}.}
  \item{exact}{
    a logical indicating if the length should be represented exactly as
    an object of class \code{\linkS4class{ulong}}.}
  \item{prec}{
    a new default value for the precision of inexact floating-point
    operations, if non-\code{NULL}.  The value should be a positive
    integer indicating a number of bits.}
  \item{rnd}{
    a new default value for the rounding mode of inexact floating-point
    operations, if non-\code{NULL}.  The value should be a character
    string with exactly one character indicating a rounding direction.
    Valid characters are \samp{[Dd]} (towards negative infinity),
    \samp{[Uu]} (towards positive infinity), \samp{[Zz]} (towards zero),
    \samp{[Aa]} (away from zero), and \samp{[Nn]} (to nearest, with
    precedence to even significands).}
  \item{rnd.mag}{
    as \code{rnd}, but used exclusively for operations returning
    \code{\linkS4class{mag}}.  Note that since \code{mag} is unsigned,
    \samp{[Dd]} and \samp{[Uu]} are equivalent to \samp{[Zz]} and
    \samp{[Aa]}, respectively.  \samp{[Nn]} is invalid as (by design)
    \code{mag} does not support rounding to nearest.}
  \item{which}{
    a character vector listing package names.}
  \item{where}{
    an environment for storing methods, by default the \pkg{flint}
    namespace.}
}
\details{
  To report a bug or request a feature, use
  \code{\link{bug.report}(package = "flint")}.

  To render the change log, use
  \code{\link{news}(package = "flint")}.

  To render the index, use
  \code{\link{help}(package = "flint")}

  To render a list of help topics for S4 classes, use
  \code{\link{help.search}(package = "flint", keyword = "classes")}

  To render a list of help topics for special mathematical functions,
  use
  \code{\link{help.search}(package = "flint", keyword = "math")}
}
\value{
  \code{flintABI} returns the size in bits of \proglang{C} type
  \code{long int}, either \code{32} or \code{64}.  The value is
  determined when package \pkg{flint} is configured.  It is checked at
  configure time and at load time that linked \proglang{C} libraries
  were configured for the same ABI.

  \code{flintClass} returns a character string naming the direct
  nonvirtual subclass of virtual class \code{flint} from which
  \code{object} inherits.  (Hence a possible value is \code{"ulong"}
  but not the name of any subclass of \code{\linkS4class{ulong}}.)
  If \code{object} does not inherit from virtual class \code{flint},
  then the return value is \code{\link{NA_character_}}.

  \code{flintLength} returns a representation of the length of
  \code{object}.  If \code{exact = TRUE}, then the return value is an
  object of class \code{\linkS4class{ulong}} representing the length
  exactly.  Otherwise, if the length is less than or equal to
  \code{.Machine[["integer.max"]]}, then the return value is a
  traditional integer vector representing the length exactly.
  Otherwise, the return value is a traditional double vector
  representing the length exactly if and only if
  \eqn{n \le 2^d-1}{n <= 2^d-1} or
  \eqn{2^{d+p} \le n < 2^{d+p+1}}{2^(d+p) <= n < 2^(d+p+1)} and \eqn{n}
  is divisible by \eqn{2^{p+1}}{2^(p+1)}, where \eqn{n} is the length,
  \eqn{d} is \code{.Machine[["double.digits"]]}, and
  \eqn{p = 0,1,\ldots}.  Lengths not exactly representable in double
  precision are rounded to the next representable number in the
  direction of zero.  Return values not representing the length exactly
  have an attribute \code{off} preserving the rounding error (an integer
  in \eqn{1,\ldots,2^p}).  If \code{object} does not inherit from
  virtual class \code{flint}, then the return value is
  \code{\link{NA_integer_}}.

  \code{flintPrec} returns the previous default precision, where the
  so-called factory setting is \code{\link{.Machine}$double.digits}.

  \code{flintRnd} and \code{flintRndMag} return the previous default
  rounding modes, where the so-called factory settings are \code{"N"}
  and \code{"A"}, respectively.

  \code{flintSize} returns an upper bound for the number of bytes used
  by \code{object}, as an object of class \code{object_size} (following
  function \code{object.size} in package \pkg{utils}).  If no members of
  the recursive structure share memory, then the upper bound is exact.
  Recursion starts at the address stored by the \R{} object, not at the
  address of the object itself.  A corollary is that
  \code{flintSize(object)} is zero for \code{object} of length zero.
  Another corollary is that the bytes counted by \code{flintSize} and
  the bytes counted by \code{\link{object.size}} are disjoint.  If
  \code{object} does not inherit from virtual class \code{flint}, then
  the return value is \code{\link{NA_real_}} (beneath the class).

  \code{flintTriple} returns a character vector of length 3 containing
  the class of \code{object}, the length of \code{object}, and the
  address stored by \code{object}.  If \code{object} does not inherit
  from virtual class \code{flint}, then all of the elements are
  \code{NA}.

  \code{flintVersion} returns a named list of numeric versions with
  elements:

  \item{package}{the \R{} package version.}
  \item{flint.h}{the \acronym{FLINT} header version.}
  \item{libflint}{the \acronym{FLINT} library version.}
  \item{mpfr.h}{the \acronym{GNU} \acronym{MPFR} header version.}
  \item{libmpfr}{the \acronym{GNU} \acronym{MPFR} library version.}
  \item{gmp.h}{the \acronym{GNU} \acronym{MP} header version.}
  \item{libgmp}{the \acronym{GNU} \acronym{MP} library version.}

  Header versions are determined at compile time.  Library versions are
  determined at compile time (static linking) or at load time (dynamic
  linking).

  \code{.initForeign} defines methods for \code{\link{coerce}} enabling
  coercion between classes in \pkg{flint} and analogous classes in the
  packages named by \code{which}.  Packages are loaded before methods
  are defined to ensure that \code{\link{setAs}} is able to find class
  definitions.  A corollary is that an error is signaled if packages are
  not installed in the library search path.  Supported signatures:
\preformatted{which="Rmpfr":
  from="mpfr", to="arf"
  from="arf", to="mpfr"
which="gmp":
  from="bigz", to="fmpz"
  from="fmpz", to="bigz"
  from="bigq", to="fmpq"
  from="fmpq", to="bigq"
}
}
\note{
  Whether and how the global default precision and rounding modes
  (set by \code{flintPrec}, \code{flintRnd}, and \code{flintRndMag}) are
  actually used depends on conventions defined in the floating-point
  class documentation, hence see \code{\linkS4class{mag}},
  \code{\linkS4class{arf}}, \code{\linkS4class{acf}},
  \code{\linkS4class{arb}}, and \code{\linkS4class{acb}}.  These
  conventions are partly inherited from the \proglang{C} library.
}
\author{
  Mikael Jagan
  \email{jaganmn@mcmaster.ca}
}
\references{
  FLINT Team (2025).
  FLINT: Fast Library for Number Theory.
  \url{https://flintlib.org/}
}
\examples{
flintABI()

oprec <- flintPrec()
nprec <- 100L
stopifnot(identical(flintPrec(nprec), oprec),
          identical(flintPrec(), nprec),
          identical(flintPrec(oprec), nprec),
          identical(flintPrec(), oprec))

ornd <- flintRnd()
nrnd <- "Z"
stopifnot(identical(flintRnd(nrnd), ornd),
          identical(flintRnd(), nrnd),
          identical(flintRnd(ornd), nrnd),
          identical(flintRnd(), ornd))

flintVersion()
}
\keyword{package}
\keyword{utilities}
