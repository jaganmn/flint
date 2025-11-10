\name{arb_integrate}
\alias{arb_integrate}
\alias{acb_integrate}
\title{Integration of Functions of One Variable}
\description{
  Compute an enclosure of the definite integral
  \deqn{\int_{a}^{b} f(z) \text{d}z}{integral_a^b f(z) dz}
  taking as the path of integration the line segment from \eqn{a} to
  \eqn{b}.
}
\usage{
arb_integrate(func, a, b, param = NULL, rtol = NULL, atol = NULL,
              control = NULL, prec = flintPrec())
acb_integrate(func, a, b, param = NULL, rtol = NULL, atol = NULL,
              control = NULL, prec = flintPrec())
}
\arguments{
  \item{func}{
    a function of the form \code{function (z, param, order, prec)}
    specifying the integrand.  Unused trailing arguments can omitted.}
  \item{a, b}{
    real or complex numbers or enclosures indicating finite limits of
    integration.}
  \item{param}{
    an \R{} object typically specifying parameters of the integrand,
    passed to \code{func}.}
  \item{rtol}{
    a positive real number less than 1 that the relative error in any
    subinterval should not exceed.  \code{2^-prec} by default.}
  \item{atol}{
    a positive real number that the absolute error in any subinterval
    should not exceed.  The value 0 indicates that convergence should
    account only for relative error.  \code{2^-prec} by default.}
  \item{control}{
    a named list of options for integration.}
  \item{prec}{
    a positive integer indicating the working precision as a number of
    bits, passed to \code{func}.}
}
\value{
  An \code{\linkS4class{arb}} or \code{\linkS4class{acb}} vector of
  length 1 giving an enclosure of the definite integral.
}
\details{
  \code{func(z, param, order, prec)} computes an enclosure for the
  integrand on \code{z}, where \code{z} is (and the return value of
  \code{func} must be) an \code{\linkS4class{arb}} or
  \code{\linkS4class{acb}} vector of length 1.  If the integer
  \code{order} is nonzero, then \code{func} must give a nonfinite result
  if the integrand is not holomorphic on \code{z}, in particular if the
  integrand composes functions that are bounded on \code{z} with branch
  cuts whose intersection with \code{z} is nonempty.

  The list \code{control} admits components \code{deg.limit},
  \code{eval.limit}, \code{depth.limit}, \code{use.heap}, and
  \code{verbose}.  These correspond to so-named members of the
  \proglang{C} struct \code{acb_calc_integrate_opt_struct}; see the
  \acronym{FLINT} documentation for details.
}
\seealso{
  Classes \code{\linkS4class{arb}} and \code{\linkS4class{acb}};
  function \code{\link{integrate}} in \pkg{base}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  function: \url{https://flintlib.org/doc/arb_calc.html}
}
\examples{
## TODO
}
\keyword{math}
