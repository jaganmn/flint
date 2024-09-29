\name{acb_lambertw}
\alias{acb_lambertw}
\title{Lambert W function}
\description{
  Computes any branch \eqn{W_{k}}{W_k} of the multiple-valued Lambert
  \eqn{W} function.  \eqn{W(z)} is the set of solutions \eqn{w} of the
  equation \eqn{w e^{w} = z}{w * exp(w) = z}.
}
\usage{
acb_lambertw(z, k = 0L, flags = 0L, prec = 53L)
}
\arguments{
  \item{z}{
    a numeric, complex, or \code{\linkS4class{acb}} vector.}
  \item{k}{
    an integer or \code{\linkS4class{fmpz}} vector listing indices
    of branches of the function.  \code{0} indicates the principal
    branch.}
  \item{flags}{
    an integer vector indicating how branch cuts are defined.
    Non-zero values are non-standard; see the first reference.}
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
  The \abbr{FLINT} documentation of the underlying \proglang{C} function:
  \url{https://flintlib.org/doc/acb.html}

  \abbr{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/4.13}
}
\examples{
## TODO
}
