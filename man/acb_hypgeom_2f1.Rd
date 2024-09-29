\name{acb_hypgeom_2f1}
\alias{acb_hypgeom_2f1}
\title{Hypergeometric Functions}
\description{
  Computes the principal branch of the hypergeometric function
  \eqn{{}_{2}F_{1}(a, b, c, z)}{F(a, b, c, z)}, defined by
  \deqn{\sum_{k = 0}^{\infty} \frac{(a)_{k} (b)_{k}}{(c)_{k}} \frac{z^{k}}{k!}}{sum_{k = 0, 1, \ldots} ((a)_k * (b)_k / (c)_k) * (z^k / k!)}
  for \eqn{|z| < 1}{Mod(z) < 1} and by analytic continuation elsewhere
  in the \eqn{z}-plane, or the principal branch of the
  \emph{regularized} hypergeometric function
  \eqn{{}_{2}F_{1}(a, b, c, z) / \Gamma(c)}{F(a, b, c, z)/Gamma(c)}.
}
\usage{
acb_hypgeom_2f1(a, b, c, z, flags = 0L, prec = 53L)
}
\arguments{
  \item{a, b, c, z}{
    numeric, complex, or \code{\linkS4class{acb}} vectors.}
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
  \url{https://flintlib.org/doc/acb_hypgeom.html}

  \abbr{NIST} Digital Library of Mathematical Functions:
  \url{https://dlmf.nist.gov/15}
}
\examples{
## TODO
}
