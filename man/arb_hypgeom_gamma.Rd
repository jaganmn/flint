\name{arb_hypgeom_gamma}
\alias{arb_hypgeom_gamma}
\alias{acb_hypgeom_gamma}
\alias{arb_hypgeom_rgamma}
\alias{acb_hypgeom_rgamma}
\alias{arb_hypgeom_lgamma}
\alias{acb_hypgeom_lgamma}
\alias{arb_hypgeom_polygamma}
\alias{acb_hypgeom_polygamma}
\title{Gamma and Related Functions}
\description{
  Compute the gamma function, the reciprocal gamma function, the
  logarithm of the absolute value of the gamma function, or the
  polygamma function.  The gamma function \eqn{\Gamma(z)} is defined by
  \deqn{\int_{0}^{\infty} t^{z - 1} e^{-t} \text{d}t}{integral_0^Inf t^(z - 1) exp(-t) dt}
  for \eqn{\Re(z) > 0}{Re(z) > 0} and by analytic continuation
  elsewhere in the \eqn{z}-plane, excluding poles at
  \eqn{z = 0, -1, \ldots}.
}
\usage{
arb_hypgeom_gamma(x, prec = flintPrec())
acb_hypgeom_gamma(z, prec = flintPrec())

arb_hypgeom_rgamma(x, prec = flintPrec())
acb_hypgeom_rgamma(z, prec = flintPrec())

arb_hypgeom_lgamma(x, prec = flintPrec())
acb_hypgeom_lgamma(z, prec = flintPrec())

## arb_hypgeom_polygamma(s = 0, z, prec = flintPrec())
   acb_hypgeom_polygamma(s = 0, z, prec = flintPrec())
}
\arguments{
  \item{x, z, s}{
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
\details{
  \code{acb_hypgeom_polygamma(s, z)} evaluates the polygamma function of
  order \code{s} at \code{z}.  The order \code{s} can be any complex
  number.  For nonnegative integers \code{m}, \code{s = m} corresponds
  to the derivative of order \code{m} of the digamma function
  \eqn{\psi(z) = \Gamma'(z)/\Gamma(z)}.  Use
  \code{acb_hypgeom_polygamma(0, z)} to evaluate the digamma function at
  \code{z}.
}
\seealso{
  Classes \code{\linkS4class{arb}} and \code{\linkS4class{acb}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/arb_hypgeom.html},
  \url{https://flintlib.org/doc/acb_hypgeom.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/5}
}
\examples{
## TODO
}
\keyword{math}
