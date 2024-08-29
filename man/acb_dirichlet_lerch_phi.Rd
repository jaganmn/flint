\name{acb_dirichlet_lerch_phi}
\alias{acb_dirichlet_lerch_phi}
\title{Lerch Transcendent}
\description{TODO}
\usage{
acb_dirichlet_lerch_phi(z, s, a, prec = 53L)
}
\arguments{
  \item{z, s, a}{numeric, complex, or \code{\linkS4class{acb}} vectors.}
  \item{prec}{a numeric or \code{\linkS4class{slong}} vector.}
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
  The \abbr{FLINT} documentation of the underlying \proglang{C} function:
  \url{https://flintlib.org/doc/acb_dirichlet.html}
}
\examples{
## TODO
}
