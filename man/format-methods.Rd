\name{format-methods}
\alias{format-methods}
\alias{format,ulong-method}
\alias{format,slong-method}
\alias{format,fmpz-method}
\alias{format,fmpq-method}
\alias{format,mag-method}
\alias{format,arf-method}
\alias{format,acf-method}
\alias{format,arb-method}
\alias{format,acb-method}
\docType{methods}
\title{Format FLINT-type Numbers as Strings}
\description{
  Format a \code{\linkS4class{flint}} vector for pretty printing.
}
\usage{
\S4method{format}{ulong}(x, base = 10L, \dots)
\S4method{format}{slong}(x, base = 10L, \dots)
\S4method{format}{fmpz}(x, base = 10L, \dots)
\S4method{format}{fmpq}(x, base = 10L, \dots)
\S4method{format}{mag}(x, base = 10L, digits = NULL, sep = NULL,
       rnd = flintRnd(), \dots)
\S4method{format}{arf}(x, base = 10L, digits = NULL, sep = NULL,
       rnd = flintRnd(), \dots)
\S4method{format}{acf}(x, base = 10L, digits = NULL, sep = NULL,
       rnd = flintRnd(), \dots)
\S4method{format}{arb}(x, base = 10L, digits = NULL, sep = NULL,
       rnd = flintRnd(), \dots)
\S4method{format}{acb}(x, base = 10L, digits = NULL, sep = NULL,
       rnd = flintRnd(), \dots)
}
\arguments{
  \item{x}{
    a \code{\linkS4class{flint}} vector.}
  \item{base}{
    an integer from 2 to 62 indicating a base for output.  Values 2, 10,
    and 16 correspond to binary, decimal, and hexadecimal output.
    Digits are represented by characters \samp{[0-9A-Za-z]}, in that
    significance order, hence the maximum 10+26+26=62.}
  \item{digits}{
    an integer indicating how many digits of the significand are
    reported when formatting floating type vectors.  When more than one
    digit is printed, a radix point inserted after the first digit.
    Value 0 is equivalent to the minimum integer \code{d} such that all
    elements of \code{x} are represented exactly by \code{d} digits in
    the specified base.  The default value \code{NULL} is equivalent to
    \code{getOption("digits", 99999L)}.}
  \item{sep}{
    a nonempty character string used to separate the significand from
    the exponent.  The default value \code{NULL} is a equivalent to
    \code{"e"} for \code{base} equal to 10 and to \code{"@"} for all
    other bases.}
  \item{rnd}{
    a nonempty character string whose first character indicates a
    rounding mode.  Methods for \code{\linkS4class{arb}} and
    \code{\linkS4class{acb}} require \code{rnd} of length 2, specifying
    rounding modes separately for midpoints and radii.  See
    \code{\link{flintRnd}} for information about valid character
    strings.}
  \item{\dots}{
    further optional arguments, though these are currently unused.}
}
\value{
  A character vector containing \acronym{ASCII} strings of equal length.
}
\examples{
q <- .fmpq(num = c(-1L, 1L) * 0:5, den = 1:6)
for (b in 2:8) {
    cat("base = ", b, ":\n", sep = "")
    print(format(q, base = b), quote = FALSE, width = 12L)
}

z <- .acb(real = .arb(mid = pi, rad = 0.5 * pi))
format(z)
format(z, base = 62L, sep = "*[62]^")
strsplit(format(Re(z), digits = 80L), "[( )]")[[1L]][c(FALSE, TRUE)]
}
\keyword{character}
\keyword{methods}
\keyword{print}
