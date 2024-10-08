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
\alias{print,flint-method}
\alias{show,flint-method}
\title{Class of FLINT-Type Vectors}
\docType{class}
\description{
  Class \code{flint} is a virtual class representing vectors of any
  \acronym{FLINT} \proglang{C} type.  The \proglang{C} type is
  determined by the class attribute and interfaced exactly using \R{}'s
  external pointer type.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      an external pointer.  The protected field is an integer vector
      of length 2 storing the lower and upper bits of the 64-bit object
      length.  The pointer field contains the address of a block of
      allocated memory of size greater than or equal to the object
      length times the size of the \acronym{FLINT} type.  It is a null
      pointer if and only if the object length is zero.\cr
      \cr
      Methods for \code{\link{initialize}} set a finalizer on
      \code{.xData} (see \code{\link{reg.finalizer}}) to ensure that
      allocated memory is freed before \code{.xData} is itself freed by
      the garbage collector.
    }
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
    \item{\code{print}}{
      \code{signature(x = "flint")}:\cr
      prints \code{\link{format}(x)} and returns \code{x} invisibly.
      For more options, consider \code{print(format(x, \dots), \dots)}
      instead of \code{print(x, \dots)}.}
    \item{\code{show}}{
      \code{signature(object = "flint")}:\cr
      prints \code{\link{format}(object)} and returns \code{NULL}
      invisibly.}
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
