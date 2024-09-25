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
      an external pointer.  The pointer field contains the address of a
      block of allocated memory of size greater than or equal to the
      object length times the size of the \abbr{FLINT} type.  It is a
      null pointer if and only if the object length is zero.  The
      protected field is the \code{length} slot.}
  }
  Methods for \code{\link{initialize}} set a finalizer on \code{x} to
  ensure that allocated memory is freed before \code{x} is itself freed
  by the garbage collector.  The requirement that the protected field of
  \code{x} is \code{length} ensures that the finalizer has a means to
  determine the object length from \code{x}.

  There is no mechanism in the package API for replacing or modifying
  \code{length} or \code{x} once initialized.  Users tempted to do so
  are encouraged to use \code{\link{new}} and API functions and methods,
  as those will allocate new objects with new (valid, consistent,
  \ldots) slots \code{length} and \code{x}.

  Calls to \code{\link{initialize}} outside of \code{\link{new}} and
  calls to \code{attr<-}, \code{\link{slot<-}}, \code{\link{@<-}}, and
  similar in user code are not safe and not supported.
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
