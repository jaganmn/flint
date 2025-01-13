\name{arb_lambertw}
\alias{arb_lambertw}
\alias{acb_lambertw}
\title{Lambert W function}
\description{
  Computes any branch \eqn{W_{k}}{W_k} of the multiple-valued Lambert
  \eqn{W} function.  \eqn{W(z)} is the set of solutions \eqn{w} of the
  equation \eqn{w e^{w} = z}{w * exp(w) = z}.
}
\usage{
arb_lambertw(x,         flags = 0L, prec = flintPrec())
acb_lambertw(z, k = 0L, flags = 0L, prec = flintPrec())
}
\arguments{
  \item{x, z}{
    numeric, complex, \code{\linkS4class{arb}}, or
    \code{\linkS4class{acb}} vectors.}
  \item{k}{
    an integer or \code{\linkS4class{fmpz}} vector listing indices
    of branches of the function.  \code{0} indicates the principal
    branch.}
  \item{flags}{
    for \code{arb_lambertw}:\cr
    an integer vector indicating which of the index 0 and index -1
    branches is computed (\code{0} means index 0, \code{1} means index
    -1).\cr
    \cr
    for \code{acb_lambertw}:\cr
    an integer vector indicating how branch cuts are defined.
    Nonzero values are nonstandard; see the first reference.}
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
  functions: \url{https://flintlib.org/doc/arb.html},
  \url{https://flintlib.org/doc/acb.html}

  \acronym{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/4.13}
}
\examples{
## TODO
}
\keyword{math}
