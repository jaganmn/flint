\alias{arf-class}
\alias{initialize,arf,numeric-method}
\alias{coerce,numeric,arf-method}
\alias{coerce,arf,double-method}
\title{Class of Vectors of Arbitrary Precision Floating-Point Numbers}
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
      \code{signature(.Object = "arf", x = "numeric")}:\cr TODO.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "arf")}:\cr
      equivalent to \code{new("arf", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arf", to = "double")}:\cr TODO.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/arf.html}
}
\examples{
showClass("arf")
}
