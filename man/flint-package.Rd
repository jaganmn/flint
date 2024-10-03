\name{flint-package}
\alias{flint}
\alias{flint-package}
\alias{flintVersion}
\title{\R{} Package \pkg{flint}}
\description{
  An \R{} interface to the \acronym{FLINT} (Fast Library for Number
  Theory) \proglang{C} library, an extension of \acronym{GNU}
  \acronym{MFPR} (Multiple Precision Floating-Point Reliable Library)
  and \acronym{GNU} \acronym{MP} (Multiple Precision Arithmetic
  Library).
}
\usage{
flintVersion()
}
\details{
  To render a list of available help topics, use
  \code{\link{help}(package = "flint")}.

  To report a bug or request a change, use
  \code{\link{bug.report}(package = "flint")}.
}
\value{
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
\docType{package}
