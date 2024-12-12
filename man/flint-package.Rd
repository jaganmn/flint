\name{flint-package}
\alias{flint}
\alias{flint-package}
\alias{flintBits}
\alias{flintIdentical}
\alias{flintPrec}
\alias{flintRnd}
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
flintBits()
flintIdentical(object, reference)
flintPrec(prec = NULL)
flintRnd(rnd = NULL)
flintVersion()
}
\arguments{
  \item{object, reference}{
    objects inheriting from virtual class \code{\linkS4class{flint}}.}
  \item{prec}{
    a new default value for the precision of inexact floating point
    operations, if non-\code{NULL}.  The value should be a positive
    integer indicating a number of bits.}
  \item{rnd}{
    a new default value for the rounding mode of inexact floating point
    operations, if non-\code{NULL}.  The value should be a character
    vector of length 2 with names \code{c("signed", "unsigned")}
    indicating rounding modes for signed and unsigned floating types.
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
  \code{flintBits} returns the current limb size as a number of bits,
  either \code{32L} or \code{64L}.  The value depends on the
  configuration of the linked \acronym{FLINT} library.

  \code{flintIdentical} tests whether its arguments inherit from the
  same nonvirtual subclass of \code{\linkS4class{flint}} and have equal
  length and equal (in the bitwise sense) elements.  If the elements
  are recursive structures, then they are compared recursively.

  \code{flintPrec} returns the previous default precision.

  \code{flintRnd} returns the previous default rounding mode.

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
flintBits()
flintVersion()

oprec <- flintPrec()
nprec <- 100L
stopifnot(identical(flintPrec(nprec), oprec),
          identical(flintPrec(), nprec),
          identical(flintPrec(oprec), nprec),
          identical(flintPrec(), oprec))

ornd <- flintRnd()
nrnd <- c(signed = "Z", unsigned = "Z")
stopifnot(identical(flintRnd(nrnd), ornd),
          identical(flintRnd(), nrnd),
          identical(flintRnd(ornd), nrnd),
          identical(flintRnd(), ornd))
}
