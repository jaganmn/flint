\name{arb_dirichlet_zeta}
\alias{arb_dirichlet_zeta}
\alias{acb_dirichlet_zeta}
\alias{arb_dirichlet_hurwitz}
\alias{acb_dirichlet_hurwitz}
\alias{arb_dirichlet_lerch_phi}
\alias{acb_dirichlet_lerch_phi}
\title{Zeta and Related Functions}
\description{
  Compute the Riemann zeta function, the Hurwitz zeta function,
  or Lerch's transcendent.  Lerch's transcendent \eqn{\Phi(z, s, a)}
  is defined by
  \deqn{\sum_{k = 0}^{\infty} \frac{z^{k}}{(k + a)^{s}}}{sum_{k = 0, 1, \ldots} z^k/(k + a)^s}
  for \eqn{|z| < 1}{Mod(z) < 1} and by analytic continuation elsewhere
  in the \eqn{z}-plane.  The Riemann and Hurwitz zeta functions are the
  special cases \eqn{\zeta(s) = \Phi(1, s, 1)} and
  \eqn{\zeta(s, a) = \Phi(1, s, a)}, respectively.  See the references
  for restrictions on \eqn{s} and \eqn{a}.
}
\usage{
arb_dirichlet_zeta(s, prec = flintPrec())
acb_dirichlet_zeta(s, prec = flintPrec())

arb_dirichlet_hurwitz(s, a = 1, prec = flintPrec())
acb_dirichlet_hurwitz(s, a = 1, prec = flintPrec())

## arb_dirichlet_lerch_phi(z = 1, s, a = 1, prec = flintPrec())
   acb_dirichlet_lerch_phi(z = 1, s, a = 1, prec = flintPrec())
}
\arguments{
  \item{z, s, a}{
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
  Classes \code{\linkS4class{arb}} and \code{\linkS4class{acb}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/acb_dirichlet.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/25}
}
\examples{
dzet <- acb_dirichlet_zeta
dhur <- acb_dirichlet_hurwitz
dler <- acb_dirichlet_lerch_phi

## Somewhat famous particular values :
debugging <- tolower(Sys.getenv("R_FLINT_CHECK_EXTRA")) == "true"
     s <- acb(x = c(   -1,    0,      2,       4))
zeta.s <- acb(x = c(-1/12, -1/2, pi^2/6, pi^4/90))
stopifnot(all.equal(dzet(   s   ), zeta.s),
          all.equal(dhur(   s, 1), zeta.s),
          !debugging ||
          {
          print(cbind(as.complex(dler(1, s, 1)), as.complex(zeta.s)))
          all.equal(dler(1, s, 1), zeta.s) # FLINT bug, report this
          })

set.seed(0xabcdL)
r <- 10L
eps <- 0x1p-4
a    <- flint:::complex.runif(r, modulus = c(    0, 1/eps))
z.l1 <- flint:::complex.runif(r, modulus = c(    0, 1-eps))
z.g1 <- flint:::complex.runif(r, modulus = c(1+eps, 1/eps))
z <- acb(x = c(z.l1, z.g1))

## A relation with the hypergeometric function from
##   http://dlmf.nist.gov/25.14.E3_3 :
h2f1 <- acb_hypgeom_2f1
stopifnot(all.equal(dler(z.l1, 1, a), h2f1(a, 1, a + 1, z.l1)/a))

## TODO: test values also for z[Mod(z) > 1] ...
}
\keyword{math}
