\name{Part}
\alias{Part}
\alias{Num}
\alias{Num,fmpq-method}
\alias{Num<-}
\alias{Num<-,fmpq-method}
\alias{Den}
\alias{Den,fmpq-method}
\alias{Den<-}
\alias{Den<-,fmpq-method}
\alias{Mid}
\alias{Mid,arb-method}
\alias{Mid,acb-method}
\alias{Mid<-}
\alias{Mid<-,arb-method}
\alias{Mid<-,acb-method}
\alias{Rad}
\alias{Rad,arb-method}
\alias{Rad<-}
\alias{Rad<-,arb-method}
\alias{Real}
\alias{Real,acf-method}
\alias{Real,acb-method}
\alias{Real<-}
\alias{Real<-,acf-method}
\alias{Real<-,acb-method}
\alias{Imag}
\alias{Imag,acf-method}
\alias{Imag,acb-method}
\alias{Imag<-}
\alias{Imag<-,acf-method}
\alias{Imag<-,acb-method}
\docType{methods}
\title{Get or Set One Part of a Vector}
\description{
  The subclasses of virtual class \code{\linkS4class{flint}} are
  interfaces to \proglang{C} types in the \acronym{FLINT} \proglang{C}
  library.  For types implemented recursively as \proglang{C} structs,
  it is often very natural to get and set the struct members.  The
  functions documented here provide support for this common operation;
  they are all S4 generic.
}
\usage{
Num(q)
Num(q) <- value
Den(q)
Den(q) <- value

Mid(x)
Mid(x) <- value
Rad(x)
Rad(x) <- value

Real(z)
Real(z) <- value
Imag(z)
Imag(z) <- value
}
\arguments{
  \item{q}{
    a vector-like \R{} object with elements representing quotients of
    numbers.  Package \pkg{flint} provides methods for class
    \code{\linkS4class{fmpq}}.}
  \item{x}{
    a vector-like \R{} object with elements representing balls in a
    metric space.  Package \pkg{flint} provides methods for class
    \code{\linkS4class{arb}}.}
  \item{z}{
    a vector-like \R{} object with elements representing complex
    numbers.  Package \pkg{flint} provides methods for classes
    \code{\linkS4class{acf}} and \code{\linkS4class{acb}}.}
  \item{value}{
    a vector-like \R{} object; the replacement value.  Methods in
    package \pkg{flint} support atomic vectors and vectors inheriting
    from virtual class \code{\linkS4class{flint}}, of length equal to
    1 or the length of the argument.}
}
\value{
  \code{Num}, \code{Den}, \code{Mid}, \code{Rad}, \code{Real}, and
  \code{Imag} and their replacement forms return a vector-like \R{}
  object preserving the length, dimensions, dimension names, and names
  of the argument.  See \sQuote{Details} for behaviour specific to
  methods in package \pkg{flint}.
}
\details{
  \code{Num} and \code{Den} extract \code{\linkS4class{fmpz}} numerators
  and denominators from \code{\linkS4class{fmpq}} \code{q}.  The
  replacement form of \code{Num} constructs a new \code{fmpq} vector
  from \code{value} (coerced to \code{fmpz}) and \code{Den(q)}.  The
  replacement form of \code{Den} constructs a new \code{fmpq} vector
  from \code{Num(q)} and \code{value} (coerced to \code{fmpz}).

  \code{Mid} and \code{Rad} extract \code{\linkS4class{arf}} midpoints
  and \code{\linkS4class{mag}} radii from \code{\linkS4class{arb}}
  \code{x}.  The replacement form of \code{Mid} constructs a new
  \code{arb} vector from \code{value} (coerced to \code{arf}) and
  \code{Rad(x)}.  The replacement form of \code{Rad} constructs a new
  \code{arb} vector from \code{Mid(x)} and \code{value} (coerced to
  \code{mag}).

  \code{Real} and \code{Imag} extract \code{\linkS4class{arf}} real and
  imaginary parts from \code{\linkS4class{acf}} \code{z} and
  \code{\linkS4class{arb}} real and imaginary parts from
  \code{\linkS4class{acb}} \code{z}.  The replacement form of
  \code{Real} constructs a new \code{acf} or \code{acb} vector from
  \code{value} (coerced to \code{arf} or \code{arb}) and \code{Imag(z)}.
  The replacement form of \code{Imag} constructs a new \code{acf} or
  \code{acb} vector from \code{Real(z)} and \code{value} (coerced to
  \code{arf} or \code{arb}).

  For convenience, \code{Mid} and its replacement form also work for
  \code{\linkS4class{acb}} \code{x}, getting and setting the
  complex midpoint defined by the midpoints of the real and imaginary
  parts of \code{x}.
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\examples{
(q <- q. <- fmpq(num = 1:10, den = 2L))
Num(q)
Den(q)
Num(q) <- Den(q)
q
(m <- Num(q))
(n <- Den(q))
stopifnot(m == 1L, n == 1L, q == 1L)
}
\keyword{methods}
