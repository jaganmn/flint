\name{Constants}
\alias{Constants}
\alias{arb_const_pi}
\alias{arb_const_log2}
\alias{arb_const_log10}
\alias{arb_const_e}
\title{Mathematical Constants Represented to Arbitrary Precision}
\description{
  Compute standard mathematical constants to arbitrary precision.
}
\usage{
arb_const_pi(prec = flintPrec())
arb_const_log2(prec = flintPrec())
arb_const_log10(prec = flintPrec())
arb_const_e(prec = flintPrec())
}
\arguments{
  \item{prec}{
    a numeric or \code{\linkS4class{slong}} vector indicating the
    desired precision as a number of bits.}
}
\value{
  An \code{\linkS4class{arb}} vector storing function values with error
  bounds.  Its length is the length of \code{prec}, typically 1.
}
\seealso{
  Class \code{\linkS4class{arb}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  functions: \url{https://flintlib.org/doc/arb.html}
}
\examples{
arb_const_pi(2^(0:15))
}
\keyword{math}
