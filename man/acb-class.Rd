\name{acb-class}
\alias{acb-class}
\alias{initialize,acb,numeric,numeric-method}
\alias{coerce,numeric,acb-method}
\alias{coerce,complex,acb-method}
\alias{coerce,acb,list-method}
\title{Class of Balls over the Complex Numbers}
\docType{class}
\description{TODO}
\section{Slots}{
  \describe{
    \item{\code{length, x}}{
      inherited from \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{initialize}}{
      \code{signature(.Object = "acb", r = "numeric", i = "numeric")}:\cr TODO.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "acb")}:\cr
      equivalent to \code{new("acb", r = from, i = 0)}.}
    \item{\code{coerce}}{
      \code{signature(from = "complex", to = "acb")}:\cr
      equivalent to \code{new("acb", r = Re(from), i = Im(from))}.}
    \item{\code{coerce}}{
      \code{signature(from = "acb", to = "double")}:\cr TODO.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/acb.html}
}
\examples{
showClass("acb")
}
