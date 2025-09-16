\name{arb_hypgeom_2f1}
\alias{arb_hypgeom_2f1}
\alias{acb_hypgeom_2f1}
\title{Hypergeometric Functions}
\description{
  Computes the principal branch of the hypergeometric function
  \eqn{{}_{2}F_{1}(a, b, c, z)}{F(a, b, c, z)}, defined by
  \deqn{\sum_{k = 0}^{\infty} \frac{(a)_{k} (b)_{k}}{(c)_{k}} \frac{z^{k}}{k!}}{sum_{k = 0, 1, \ldots} ((a)_k * (b)_k/(c)_k) * (z^k/k!)}
  for \eqn{|z| < 1}{Mod(z) < 1} and by analytic continuation elsewhere
  in the \eqn{z}-plane, or the principal branch of the
  \emph{regularized} hypergeometric function
  \eqn{{}_{2}F_{1}(a, b, c, z)/\Gamma(c)}{F(a, b, c, z)/Gamma(c)}.
}
\usage{
arb_hypgeom_2f1(a, b, c, x, flags = 0L, prec = flintPrec())
acb_hypgeom_2f1(a, b, c, z, flags = 0L, prec = flintPrec())
}
\arguments{
  \item{a, b, c, x, z}{
    numeric, complex, \code{\linkS4class{arb}}, or
    \code{\linkS4class{acb}} vectors.}
  \item{flags}{
    an integer vector.  The lowest bit of the integer element(s)
    indicates whether to regularize.  Later bits indicate special cases
    for which an alternate algorithm may be used.  Non-experts should
    use \code{flags = 0L} or \code{1L}, leaving the later bits unset.}
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
  Classes \code{\linkS4class{arb}} and \code{\linkS4class{acb}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/arb_hypgeom.html},
  \url{https://flintlib.org/doc/acb_hypgeom.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/15}
}
\examples{
h2f1 <- acb_hypgeom_2f1

set.seed(0xbcdeL)
r <- 10L
eps <- 0x1p-4
z.l1 <- flint:::complex.runif(r, modulus = c(    0, 1-eps))
z.g1 <- flint:::complex.runif(r, modulus = c(1+eps, 1/eps))
z <- acb(x = c(z.l1, z.g1))

## Elementary special cases from http://dlmf.nist.gov/15.4 :
stopifnot(all.equal(h2f1(1.0, 1.0, 2.0,  z  ),
                    -log(1 - z)/z),
          all.equal(h2f1(0.5, 1.0, 1.5,  z^2),
                    0.5 * (log(1 + z) - log(1 - z))/z),
          all.equal(h2f1(0.5, 1.0, 1.5, -z^2),
                    atan(z)/z))
## [ see more in ../tests/hypgeom.R ]
}
\keyword{math}
