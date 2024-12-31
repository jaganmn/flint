\name{flint-package}
\alias{flint}
\alias{flint-package}
\alias{flintABI}
\alias{flintIdentical}
\alias{flintLength}
\alias{flintPrec}
\alias{flintRnd}
\alias{flintSize}
\alias{flintVersion}
\title{\R{} Package \pkg{flint}}
\docType{package}
\description{
  An \R{} interface to the \acronym{FLINT} (Fast Library for Number
  Theory) \proglang{C} library, an extension of \acronym{GNU}
  \acronym{MFPR} (Multiple Precision Floating-Point Reliable Library)
  and \acronym{GNU} \acronym{MP} (Multiple Precision Arithmetic
  Library).
}
\usage{
flintABI()
flintIdentical(object, reference)
flintLength(object, exact = TRUE)
flintPrec(prec = NULL)
flintRnd(rnd = NULL)
flintSize(object)
flintVersion()
}
\arguments{
  \item{object, reference}{
    objects inheriting from virtual class \code{\linkS4class{flint}}.}
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
    string indicating a rounding mode for signed floating types.
    Valid characters are \samp{[Uu]} (towards positive infinity),
    \samp{[Dd]} (towards negative infinity), \samp{[Zz]} (towards zero),
    \samp{[Aa]} (away from zero), and \samp{[Nn]} (to nearest, with
    precedence to even significands).}
}
\details{
  To render a list of available help topics, use
  \code{\link{help}(package = "flint")}.

  To report a bug or request a change, use
  \code{\link{bug.report}(package = "flint")}.
}
\value{
  \code{flintABI} returns the size in bits of \proglang{C} type
  \code{long int}, either \code{32} or \code{64}.  The value is
  determined when package \pkg{flint} is configured.  It is checked at
  configure time and at load time that linked \proglang{C} libraries
  were configured for the same ABI.

  \code{flintIdentical} tests whether its arguments inherit from the
  same nonvirtual subclass of \code{\linkS4class{flint}} and have equal
  length and equal (in the bitwise sense) elements.  If the elements
  are recursive structures, then they are compared recursively.

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
  in \eqn{1,\ldots,2^p}).

  \code{flintPrec} returns the previous default precision.

  \code{flintRnd} returns the previous default rounding mode.

  \code{flintSize} returns an upper bound for the number of bytes used
  by \code{object}, as an object of class \code{object_size} (following
  function \code{object.size} in package \pkg{utils}).  If no members of
  the recursive structure share memory, then the upper bound is exact.
  Recursion starts at the address stored by the \R{} object, not at the
  address of the object itself.  A corollary is that
  \code{flintSize(object)} is zero for \code{object} of zero length.
  Another corollary is that the bytes counted by \code{flintSize} and
  the bytes counted by \code{\link{object.size}} are disjoint.

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
}
\author{
  Mikael Jagan
  \email{jaganmn@mcmaster.ca}
}
\examples{
flintABI()
stopifnot(identical(flintABI(), 8L * .Machine[["sizeof.long"]]))

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
