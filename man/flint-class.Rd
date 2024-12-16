\name{flint-class}
\alias{flint-class}
\alias{[,flint,ANY,missing,missing-method}
\alias{[<-,flint,ANY,missing-method}
\alias{[[,flint,ANY,missing-method}
\alias{[[<-,flint,ANY,missing-method}
\alias{as.raw,flint-method}
\alias{as.logical,flint-method}
\alias{as.integer,flint-method}
\alias{as.double,flint-method} % stub
\alias{as.numeric,flint-method}
\alias{as.complex,flint-method}
\alias{as.character,flint-method}
\alias{as.list,flint-method}
\alias{as.expression,flint-method}
\alias{length,flint-method}
\alias{length<-,flint-method}
\alias{print,flint-method}
\alias{rep,flint-method}
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
    \item{\code{[}}{
      \code{signature(x = "flint", i = "ANY", j = "missing", drop = "missing")}:\cr
      returns a \code{flint} vector containing the elements of \code{x}
      indexed by \code{i} (the \dQuote{subscript}).  The subscript can
      be logical, integer, or double or missing or \code{NULL}.  It is
      an error to supply \code{NA} or out of bounds subscripts: the
      underlying \proglang{C} types have no representation for missing
      values.}
    \item{\code{[<-}}{
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      returns a \code{flint} vector identical to \code{x} but with
      elements indexed by \code{i} (the \dQuote{subscript}) set to
      values from \code{value} (which is coerced to the class of
      \code{x}).  The subscript can be logical, integer, or double or
      missing or \code{NULL}.  It is an error to supply \code{NA} or out
      of bounds subscripts: the underlying \proglang{C} types have no
      representation for missing values.}
    \item{\code{[[}}{
      \code{signature(x = "flint", i = "ANY", j = "missing")}:\cr
      equivalent to code \code{[} with the restriction that \code{i}
      must index exactly one element.}
    \item{\code{[[<-}}{
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      equivalent to code \code{[<-} with the restriction that \code{i}
      must index exactly one element.}
    \item{\code{as.raw}, \code{as.logical}, \code{as.integer}, \code{as.numeric}, \code{as.complex}, \code{as.list}, \code{as.expression}}{
      \code{signature(x = "flint")}:\cr
      returns the value of \code{as.vector(x, mode = *)}.  Methods for
      \code{as.vector} must be defined for subclasses of \code{flint}.
      Note that \code{as.double} dispatches internally the method for
      \code{as.numeric}, so there is no method for \code{as.double};
      see also \code{\link{as.numeric}}, section \sQuote{S4 Methods}.}
    \item{\code{as.character}}{
      \code{signature(x = "flint")}:\cr
      returns the value of
      \code{\link[=format-methods]{format}(x, digits = 15L, rnd = "N")}.
      where 15 is chosen to match the internal default method; see
      \code{\link{as.character}}.}
    \item{\code{length}}{
      \code{signature(x = "flint")}:\cr
      returns an integer vector of length 1 giving the object length.
      If the length exceeds \code{.Machine[["integer.max"]]}, then the
      method returns a double vector of length 1 instead.  If the length
      exceeds \code{2^.Machine[["double.digits"]]-1} and is not exactly
      representable, then the return value is the next representable
      number in the direction of zero and has an attribute \code{off}
      preserving the rounding error.}
    \item{\code{length<-}}{
      \code{signature(x = "flint")}:\cr
      returns a \code{flint} vector of length given by the second
      argument \code{value}.  The first \code{min(length(x), value)}
      elements are copied from \code{x} and the remaining elements are
      initialized to zero.  Note that \code{value} is coerced to
      \code{\linkS4class{ulong}} and so is constrained to its range.
      (Ideally, \code{value} would have 64-bit range even for 32-bit
      \code{ulong}.  If this \dQuote{ideal} matters to you, then please
      submit a feature request using
      \code{\link{bug.report}(package = "flint")}.)}
    \item{\code{print}}{
      \code{signature(x = "flint")}:\cr
      prints \code{\link{format}(x)} and returns \code{x} invisibly.
      For more options, consider \code{print(format(x, \dots), \dots)}
      instead of \code{print(x, \dots)}.}
    \item{\code{rep}}{
      \code{signature(x = "flint")}:\cr
      repeats \code{x} (or elements of \code{x}) according to optional
      arguments \code{times}, \code{length.out}, and \code{each}.  The
      behaviour is parallel to that of the internal default method,
      hence see \code{\link{rep}}.  One difference is that
      \code{rep(\var{0-length}, length.out=\var{nonzero})} signals an
      error, because the underlying \proglang{C} types have no
      representation for missing values.  Note that the optional
      arguments are coerced to \code{\linkS4class{ulong}} and so are
      constrained to its range; see the parenthetical comment under
      \code{length<-} above.}
    \item{\code{show}}{
      \code{signature(object = "flint")}:\cr
      prints \code{\link{format}(object)} and returns \code{NULL}
      invisibly.}
  }
}
\seealso{
  The nonvirtual subclasses:
  \code{\linkS4class{slong}}, \code{\linkS4class{ulong}},
  \code{\linkS4class{fmpz}}, \code{\linkS4class{fmpq}},
  \code{\linkS4class{mag}},
  \code{\linkS4class{arf}}, \code{\linkS4class{acf}},
  \code{\linkS4class{arb}}, and \code{\linkS4class{acb}}.
}
\examples{
showClass("flint")
showMethods(classes = "flint")
}
