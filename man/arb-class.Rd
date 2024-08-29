\name{arb-class}
\alias{arb-class}
\alias{initialize,arb-method}
\alias{coerce,numeric,arb-method}
\alias{coerce,arb,list-method}
\title{Class of Balls over the Real Numbers}
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
      \code{signature(.Object = "arb", x = "numeric")}:\cr TODO.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "arb")}:\cr
      equivalent to \code{new("arb", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arb", to = "list")}:\cr TODO.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/arb.html}
}
\examples{
showClass("arb")
}
