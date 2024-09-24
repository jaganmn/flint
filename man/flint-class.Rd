\name{flint-class}
\alias{flint-class}
\alias{as.raw,flint-method}
\alias{as.logical,flint-method}
\alias{as.integer,flint-method}
\alias{as.double,flint-method} % stub
\alias{as.numeric,flint-method}
\alias{as.complex,flint-method}
\alias{as.list,flint-method}
\alias{as.expression,flint-method}
\alias{length,flint-method}
\title{Class of FLINT-Type Vectors}
\docType{class}
\description{
  Class \code{flint} is a virtual class representing vectors of any
  \abbr{FLINT} \proglang{C} type.  The \proglang{C} type is determined
  by the class attribute and interfaced exactly using \R{}'s external
  pointer type.
}
\section{Slots}{
  \describe{
    \item{\code{length}}{
      an integer vector of length 2 storing the lower and upper bits of
      the 64-bit object length.}
    \item{\code{x}}{
      an external pointer to allocated memory of size equal to the
      object length times the size of the \abbr{FLINT} type.  Typically,
      a finalizer is set to ensure that this memory is freed before the
      external pointer is itself freed by the garbage collector.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{as.raw}, \code{as.logical}, \code{as.integer}, \code{as.numeric}, \code{as.complex}, \code{as.list}, \code{as.expression}}{
      \code{signature(x = "flint")}:\cr
      return the value of \code{as.vector(x, mode = *)}.  Methods for
      \code{as.vector} must be defined for subclasses of \code{flint}.
      Note that \code{as.double} dispatches internally the method for
      \code{as.numeric}, so there is no method for \code{as.double};
      see also \code{\link{as.numeric}}, section \sQuote{S4 Methods}.}
    \item{\code{length}}{
      \code{signature(x = "flint")}:\cr
      return an integer vector of length 1 giving the object length.  If
      the length exceeds \code{.Machine[["integer.max"]]}, then a double
      vector of length 1 is returned instead.  If the length exceeds
      \code{2^.Machine[["double.digits"]]-1} and is not exactly
      representable, then a warning is signaled to indicate that the
      return value is an implementation-defined rounded length.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{nflint}}.

  The nonvirtual subclasses:
  \code{\linkS4class{slong}}, \code{\linkS4class{ulong}},
  \code{\linkS4class{fmpz}}, \code{\linkS4class{fmpq}},
  \code{\linkS4class{arf}}, \code{\linkS4class{mag}},
  \code{\linkS4class{arb}}, and \code{\linkS4class{acb}}.
}
\examples{
showClass("flint")
}
