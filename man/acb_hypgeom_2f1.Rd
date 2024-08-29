\name{acb_hypgeom_2f1}
\alias{acb_hypgeom_2f1}
\alias{acb_hypgeom_2f1_continuation}
\title{Gauss Hypergeometric Function}
\description{TODO}
\usage{
acb_hypgeom_2f1(a, b, c, z, flags = 0L, prec = 53L)
acb_hypgeom_2f1_continuation(a, b, c, z0, z1, f0, f1, prec = 53L)
}
\arguments{
  \item{a, b, c, z, z0, z1, f0, f1}{numeric, complex, or
    \code{\linkS4class{acb}} vectors.}
  \item{flags}{an integer vector of length 1.  The bits of the data
    flag special cases for which an alternate algorithm may be used.
    For expert use only.}
  \item{prec}{a numeric or \code{\linkS4class{slong}} vector
    indicating the desired precision as a number of bits.}
}
\value{
  An \code{\linkS4class{acb}} vector storing function values with error
  bounds.  The length is the maximum of the lengths of the argument, or
  0 if any argument has zero length.

  Coerce to list with \code{as(., "list")} to get a tractable \R{}
  object.  (Eventually, we will implement methods to obviate such
  coercions.)
}
\seealso{
  Class \code{\linkS4class{acb}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} functions:
  \url{https://flintlib.org/doc/acb_hypgeom.html}
}
\examples{
## TODO
}
