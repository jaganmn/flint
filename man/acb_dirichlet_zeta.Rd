\name{acb_dirichlet_zeta}
\alias{acb_dirichlet_zeta}
\alias{acb_dirichlet_hurwitz}
\alias{acb_dirichlet_lerch_phi}
\title{Zeta and Related Functions}
\description{
  Computes the Riemann zeta function, the Hurwitz zeta function,
  or Lerch's transcendent.  Lerch's transcendent \eqn{\Phi(z, s, a)}
  is defined by
  \deqn{\sum_{k = 0}^{\infty} \frac{z^{k}}{(k + a)^{s}}}{sum_{k = 0, 1, \ldots} z^k/(k + a)^s}
  for \eqn{|z| < 1}{Mod(z) < 1} and by analytic continuation elsewhere
  in the \eqn{z}-plane.  The Riemann and Hurwitz zeta functions are the
  special cases \eqn{\zeta(s) = \Phi(1, s, 1)} and
  \eqn{\zeta(s, a) = \Phi(1, s, a)}, respectively.  See the references
  for restrictions on \eqn{s} and \eqn{a} in these cases.
}
\usage{
acb_dirichlet_zeta(s, prec = 53L)
acb_dirichlet_hurwitz(s, a = 1, prec = 53L)
acb_dirichlet_lerch_phi(z = 1, s, a = 1, prec = 53L)
}
\arguments{
  \item{z, s, a}{
    numeric, complex, or \code{\linkS4class{acb}} vectors.}
  \item{prec}{
    a numeric or \code{\linkS4class{slong}} vector indicating the
    desired precision as a number of bits.}
}
\value{
  An \code{\linkS4class{acb}} vector storing function values with error
  bounds.  Its length is the maximum of the lengths of the arguments or
  zero (zero if any argument has length zero).  The arguments are
  recycled as necessary.
}
\seealso{
  Class \code{\linkS4class{acb}}.
}
\references{
  The \abbr{FLINT} documentation of the underlying \proglang{C} functions:
  \url{https://flintlib.org/doc/acb_dirichlet.html}

  \abbr{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/25}
}
\examples{
## TODO
}
