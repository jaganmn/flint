\name{arb_hypgeom_gamma_lower}
\alias{arb_hypgeom_gamma_lower}
\alias{acb_hypgeom_gamma_lower}
\alias{arb_hypgeom_gamma_upper}
\alias{acb_hypgeom_gamma_upper}
\alias{arb_hypgeom_beta_lower}
\alias{acb_hypgeom_beta_lower}
\title{Incomplete Gamma and Related Functions}
\description{
  Compute the principal branch of the (optionally, regularized)
  incomplete gamma and beta functions.  The lower incomplete gamma
  function \eqn{\gamma(s, z)} is defined by
  \deqn{\int_{0}^{z} t^{s - 1} e^{-t} \text{d}t}{integral_0^z t^(s - 1) exp(-t) dt}
  for \eqn{\Re(s) > 0}{Re(s) > 0} and by analytic continuation elsewhere
  in the \eqn{s}-plane, excluding poles at \eqn{s = 0, -1, \ldots}.  The
  upper incomplete gamma function \eqn{\Gamma(s, z)} is defined by
  \deqn{\int_{z}^{\infty} t^{s - 1} e^{-t} \text{d}t}{integral_z^Inf t^(s - 1) exp(-t) dt}
  for \eqn{\Re(s) > 0}{Re(s) > 0} and by analytic continuation elsewhere
  in the \eqn{s}-plane except at \eqn{z = 0}.  The incomplete beta
  function \eqn{B(a, b, z)} is defined by
  \deqn{\int_{0}^{z} t^{a - 1} (1 - t)^{b - 1} \text{d}t}{integral_0^z t^(a - 1) (1 - t)^(b - 1) dt}
  for \eqn{\Re(a), \Re(b) > 0}{Re(a), Re(b) > 0} and by analytic
  continuation to all other \eqn{(a, b)}.  It coincides with the beta
  function at \eqn{z = 1}.  The regularized functions are
  \eqn{\gamma(s, z)/\Gamma(s)}, \eqn{\Gamma(s, z)/\Gamma(s)}, and
  \eqn{B(a, b, z)/B(a, b)}.
}
\usage{
arb_hypgeom_gamma_lower(s, x, flags = 0L, prec = flintPrec())
acb_hypgeom_gamma_lower(s, z, flags = 0L, prec = flintPrec())

arb_hypgeom_gamma_upper(s, x, flags = 0L, prec = flintPrec())
acb_hypgeom_gamma_upper(s, z, flags = 0L, prec = flintPrec())

arb_hypgeom_beta_lower(a, b, x, flags = 0L, prec = flintPrec())
acb_hypgeom_beta_lower(a, b, z, flags = 0L, prec = flintPrec())
}
\arguments{
  \item{x, z, s, a, b}{
    numeric, complex, \code{\linkS4class{arb}}, or
    \code{\linkS4class{acb}} vectors.}
  \item{flags}{
    an integer vector with elements 0, 1, or 2 indicating unregularized,
    regularized, or \dQuote{alternately} regularized; see the
    \acronym{FLINT} documentation.}
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
  \code{\link{arb_hypgeom_gamma}} and \code{\link{arb_hypgeom_beta}} for
  the \dQuote{complete} gamma and beta functions.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/arb_hypgeom.html},
  \url{https://flintlib.org/doc/acb_hypgeom.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/8}
}
\examples{
hg  <- acb_hypgeom_gamma
hgl <- acb_hypgeom_gamma_lower
hgu <- acb_hypgeom_gamma_upper

hb  <- acb_hypgeom_beta
hbl <- acb_hypgeom_beta_lower

set.seed(0xcdefL)
r <- 10L
eps <- 0x1p-4
a <- flint:::complex.runif(r, modulus = c(  0, 1/eps))
b <- flint:::complex.runif(r, modulus = c(  0, 1/eps))
z <- flint:::complex.runif(r, modulus = c(eps, 1/eps))

## Some trivial identities
stopifnot(# http://dlmf.nist.gov/8.2.E3
          all.equal(hgl(a, z) + hgu(a, z), hg(a), tolerance = 1e-5),
          # https://dlmf.nist.gov/8.4.E5
          all.equal(hgu(1, z), exp(-z), check.class = FALSE))

## Regularization
stopifnot(all.equal(hgl(a,    z, flags = 1L), hgl(a,    z)/hg(a   )),
          all.equal(hgu(a,    z, flags = 1L), hgu(a,    z)/hg(a   )),
          all.equal(hbl(a, b, z, flags = 1L), hbl(a, b, z)/hb(a, b)))

## A relation with the hypergeometric function from
##   https://dlmf.nist.gov/8.17.E7 :
h2f1 <- acb_hypgeom_2f1
stopifnot(all.equal(hbl(a, b, z), z^a * h2f1(a, 1 - b, a + 1, z)/a))
}
\keyword{math}
