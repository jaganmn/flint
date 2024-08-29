\alias{mag-class}
\alias{initialize,mag,numeric-method}
\alias{coerce,numeric,mag-method}
\alias{coerce,mag,double-method}
\title{Class of Vectors of Magnitude Bounds}
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
      \code{signature(.Object = "mag", x = "numeric")}:\cr TODO.}
    \item{\code{coerce}}{
      \code{signature(from = "numeric", to = "mag")}:\cr
      equivalent to \code{new("mag", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "mag", to = "double")}:\cr TODO.}
  }
}
\seealso{
  Virtual superclass \code{\linkS4class{flint}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} type:
  \url{https://flintlib.org/doc/mag.html}
}
\examples{
showClass("mag")
}
