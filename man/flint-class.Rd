\name{flint-class}
\alias{flint-class}
\title{Class of \abbr{FLINT}-Type Vectors}
\docType{class}
\description{
  Class \code{flint} is a virtual class representing vectors of any
  \abbr{FLINT} type.  The type is determined by the class attribute.
}
\section{Slots}{
  These slots are common to all objects inheriting from class
  \code{flint}.  They are not part of the API, so users should not
  access them directly, and the details are subject to change without
  notice.
  \describe{
    \item{\code{length}}{
      an integer vector of length 2 storing the lower and upper bits
      of the 64-bit length.}
    \item{\code{x}}{
      an external pointer to allocated memory of size equal to the
      length times the size of the \abbr{FLINT} type.  A finalizer
      ensures that the memory is freed before the \R{} object is
      garbage collected.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{length}}{
      \code{signature(x = "flint")}:\cr
      an integer vector of length 1 giving the object length.  If the
      length exceeds \code{.Machine[["integer.max"]]}, then a double
      vector of length 1 is returned instead.  If the length exceeds
      \code{2^.Machine[["double.digits"]]} and is not exactly
      representable, then a warning is signaled to indicate that the
      result is an implementation-defined rounded length.}
  }
}
\seealso{
  The nonvirtual subclasses:
  \code{\linkS4class{slong}}, \code{\linkS4class{ulong}},
  \code{\linkS4class{fmpz}}, \code{\linkS4class{fmpq}},
  \code{\linkS4class{mag}}, \code{\linkS4class{arf}},
  \code{\linkS4class{arb}}, and \code{\linkS4class{acb}}.
}
\examples{
showClass("flint")
}
