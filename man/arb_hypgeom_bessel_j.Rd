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
  Compute the principal branches of the (modified) Bessel functions of
  the first and second kind.  The Bessel functions of the first and
  second kind solve Bessel's equation
  \deqn{z^{2} \dfrac{\text{d}^{2} w}{\text{d} z^{2}} + z \dfrac{\text{d} w}{\text{d} z} + (z^{2} - \nu^{2}) w = 0}{z^2 w''(z) + z w'(z) + (z^2 - nu^2) w(z) = 0}
  and are given by
  \deqn{\begin{aligned} J_{\nu}(z) &= (\tfrac{1}{2} z)^{\nu} \sum_{k = 0}^{\infty} (-1)^{k} \frac{(\frac{1}{4} z^{2})^{k}}{k! \Gamma(\nu + k + 1)} \\ Y_{\nu}(z) &= \frac{Y_{\nu}(z) \cos(\nu \pi) - J_{-\nu}(z)}{\sin(\nu \pi)} \end{aligned}}{J(z; nu) = (z/2)^nu sum_{k = 0, 1, \ldots} (-1)^k (z^2/4)^k/(k! Gamma(nu + k + 1))
Y(z; nu) = (J(z; nu) cos(nu pi) - J(z; -nu))/sin(nu pi)}
  The modified Bessel functions of the first and second kind solve the
  modified Bessel's equation
  \deqn{z^{2} \dfrac{\text{d}^{2} w}{\text{d} z^{2}} + z \dfrac{\text{d} w}{\text{d} z} - (z^{2} + \nu^{2}) w = 0}{z^2 w''(z) + z w'(z) - (z^2 + nu^2) w(z) = 0}
  and are given by
  \deqn{\begin{aligned} I_{\nu}(z) &= (\tfrac{1}{2} z)^{\nu} \sum_{k = 0}^{\infty} \frac{(\frac{1}{4} z^{2})^{k}}{k! \Gamma(\nu + k + 1)} \\ K_{\nu}(z) &= \frac{\pi}{2} \frac{I_{-\nu}(z) - I_{\nu}(z)}{\sin(\nu \pi)} \end{aligned}}{I(z; nu) = (z/2)^nu sum_{k = 0, 1, \ldots} (z^2/4)^k/(k! Gamma(nu + k + 1))
K(z; nu) = (pi/2) (I(z; -nu) - I(z; nu))/sin(nu pi)}
}
\usage{
arb_hypgeom_bessel_j(nu, x, prec = flintPrec())
acb_hypgeom_bessel_j(nu, z, prec = flintPrec())

arb_hypgeom_bessel_y(nu, x, prec = flintPrec())
acb_hypgeom_bessel_y(nu, z, prec = flintPrec())

arb_hypgeom_bessel_i(nu, x, prec = flintPrec())
acb_hypgeom_bessel_i(nu, z, prec = flintPrec())

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
