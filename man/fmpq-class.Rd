\name{fmpq-class}
\alias{fmpq-class}
\alias{initialize,fmpq-method}
\alias{coerce,numeric,fmpq-method}
\alias{coerce,fmpq,integer-method}
\alias{coerce,fmpq,double-method}
\title{Class of Vectors of Rational Numbers}
\docType{class}
\description{
  Class \code{fmpq} represents vectors of rational numbers.  The
  numerator and (positive, coprime) denominator are each stored as
  multiple precision integers.  The limb size is 32 or 64 bits,
  depending on the configuration of the linked \abbr{FLINT} library.
  The number of bits is given by \code{flintBits()}.
}
\section{Slots}{
  \describe{
    \item{\code{length, x}}{
      inherited from \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{initialize}}{
      \code{signature(.Object = "fmpq", p = "numeric", q = "numeric")}:\cr
      return \code{.Object} with \code{length} slot set to a
      representation of \code{max(length(p), length(q))} (0 if either
      length is 0) and \code{x} slot set to the address of an
      initialized array of \code{fmpq}.  The array is initialized with
      numerators and denominator values from \code{p} and \code{q},
      which are both truncated towards zero and recycled.  An error is
      signaled if any \code{p[i]} or \code{q[i]} is \code{NaN},
      \code{-Inf}, \code{Inf}, or subnormal.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "fmpq")}:\cr
      equivalent to \code{new("fmpq", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "fmpq", to = "integer")}:\cr
      returns an integer vector containing the result of truncating
      the rational numbers towards zero.  \code{NA} is introduced,
      with a warning, where the result is not in the range of integer.}
    \item{\code{coerce}}{
      \code{signature(from = "fmpq", to = "double")}:\cr
      returns an double vector containing the result of rounding
      the rational numbers to the nearest representable floating-point
      numbers in the direction of zero, except where the original
      number exceeds \eqn{2^1024} in absolute value, in which case the
      behaviour is to introduce \code{-Inf} or \code{Inf} with a
      warning.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/fmpq.html}
}
\examples{
showClass("fmpq")
flintBits()
}
