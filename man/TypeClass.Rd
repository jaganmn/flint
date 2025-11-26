\name{TypeClass}
\alias{TypeClass}
\alias{isSigned}
\alias{isSigned,ANY-method}
\alias{isSigned,ulong-method}
\alias{isSigned,slong-method}
\alias{isSigned,fmpz-method}
\alias{isSigned,fmpq-method}
\alias{isSigned,mag-method}
\alias{isSigned,arf-method}
\alias{isSigned,acf-method}
\alias{isSigned,arb-method}
\alias{isSigned,acb-method}
\alias{isComplex}
\alias{isComplex,ANY-method}
\alias{isComplex,ulong-method}
\alias{isComplex,slong-method}
\alias{isComplex,fmpz-method}
\alias{isComplex,fmpq-method}
\alias{isComplex,mag-method}
\alias{isComplex,arf-method}
\alias{isComplex,acf-method}
\alias{isComplex,arb-method}
\alias{isComplex,acb-method}
\alias{isComplex,fmpq-method}
\alias{isFloating}
\alias{isFloating,ANY-method}
\alias{isFloating,ulong-method}
\alias{isFloating,slong-method}
\alias{isFloating,fmpz-method}
\alias{isFloating,fmpq-method}
\alias{isFloating,mag-method}
\alias{isFloating,arf-method}
\alias{isFloating,acf-method}
\alias{isFloating,arb-method}
\alias{isFloating,acb-method}
\docType{methods}
\title{Test What is Representable by a Type or Class}
\description{
  \code{isSigned} tests if the type or class of the argument can
  represent nonzero numbers with sign (value divided by modulus) not
  equal to 1.

  \code{isComplex} tests if the type or class of the argument can
  represent complex numbers with nonzero imaginary part.

  \code{isFloating} tests if the type or class of the argument
  represents floating-point real or complex numbers.
}
\usage{
isSigned(x)
isComplex(x)
isFloating(x)
}
\arguments{
  \item{x}{
    a vector-like \R{} object representing numbers.}
}
\value{
  A logical, either \code{TRUE} or \code{FALSE}.
}
\details{
  \code{isSigned(x)} equal to \code{FALSE} implies that
  \code{\link{sign}(x)} is all 0 or 1.  The converse is not true in
  general.

  \code{isComplex(x)} equal to \code{FALSE} implies that
  \code{\link{Imag}(x)} (equivalently \code{\link{Im}(x)}) is all 0.
  The converse is not true in general.

  For \code{x} of class \code{\linkS4class{arb}} or
  \code{\linkS4class{acb}}, methods inherit behaviour from the class of
  the midpoint, returning the value of \code{is*(\link{Mid}(x))}.
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\examples{
try(isSigned(NULL)) # an error if 'x' does not represent numbers

L <- sapply(c(if (getRversion() >= "4.5") "raw", # for as(NULL, "raw")
              "logical", "integer", "double", "complex",
              "ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb"),
            as, object = NULL, simplify = FALSE)
F <- function (x) c(isSigned   = isSigned  (x),
                    isComplex  = isComplex (x),
                    isFloating = isFloating(x))
t(vapply(L, F, logical(3L)))
}
\keyword{methods}
\keyword{programming}
