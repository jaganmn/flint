\name{arb_hypgeom_bessel_j}
\alias{arb_hypgeom_bessel_j}
\alias{acb_hypgeom_bessel_j}
\alias{arb_hypgeom_bessel_i}
\alias{acb_hypgeom_bessel_i}
\alias{arb_hypgeom_bessel_y}
\alias{acb_hypgeom_bessel_y}
\alias{arb_hypgeom_bessel_k}
\alias{acb_hypgeom_bessel_k}
\title{Bessel and Related Functions}
\description{
  Compute the (modified) Bessel functions of the first and second kind.
  %% TODO: define the functions
}
\usage{
arb_hypgeom_bessel_j(nu, x, prec = flintPrec())
acb_hypgeom_bessel_j(nu, z, prec = flintPrec())

arb_hypgeom_bessel_i(nu, x, prec = flintPrec())
acb_hypgeom_bessel_i(nu, z, prec = flintPrec())

arb_hypgeom_bessel_y(nu, x, prec = flintPrec())
acb_hypgeom_bessel_y(nu, z, prec = flintPrec())

arb_hypgeom_bessel_k(nu, x, prec = flintPrec())
acb_hypgeom_bessel_k(nu, z, prec = flintPrec())
}
\arguments{
  \item{nu, x, z}{
    numeric, complex, \code{\linkS4class{arb}}, or
    \code{\linkS4class{acb}} vectors.}
  \item{prec}{
    a numeric or \code{\linkS4class{slong}} vector indicating the
    desired precision as a number of bits.}
}
\value{
  An \code{\linkS4class{arb}} or \code{\linkS4class{acb}} vector
  storing function values with error bounds.  Its length is the maximum
  of the lengths of the arguments or zero (zero if any argument has
  length zero).  The arguments are recycled as necessary.
}
\seealso{
  Classes \code{\linkS4class{arb}} and \code{\linkS4class{acb}};
  \code{\link{arb_hypgeom_gamma_lower}} and
  \code{\link{arb_hypgeom_beta_lower}} for the \dQuote{incomplete} gamma
  and beta functions.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/arb_hypgeom.html},
  \url{https://flintlib.org/doc/acb_hypgeom.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/10}
}
\examples{
## TODO
}
\keyword{math}
