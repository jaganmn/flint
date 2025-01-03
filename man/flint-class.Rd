\name{flint-class}
\alias{flint-class}
\alias{[,flint,ANY,missing,missing-method}
\alias{[<-,flint,ANY,missing-method}
\alias{[[,flint,ANY,missing-method}
\alias{[[<-,flint,ANY,missing-method}
\alias{anyDuplicated,flint-method}
\alias{as.raw,flint-method}
\alias{as.logical,flint-method}
\alias{as.integer,flint-method}
\alias{as.double,flint-method} % stub
\alias{as.numeric,flint-method}
\alias{as.complex,flint-method}
\alias{as.character,flint-method}
\alias{as.list,flint-method}
\alias{as.expression,flint-method}
\alias{as.Date,flint-method}
\alias{as.POSIXct,flint-method}
\alias{as.POSIXlt,flint-method}
\alias{as.data.frame,flint-method}
\alias{all.equal,ANY,flint-method}
\alias{all.equal,flint,ANY-method}
\alias{all.equal,flint,flint-method}
\alias{c,flint-method}
\alias{coerce,ANY,flint-method}
\alias{duplicated,flint-method}
\alias{is.na<-,flint-method}
\alias{length,flint-method}
\alias{length<-,flint-method}
\alias{mtfrm,flint-method}
\alias{names,flint-method}
\alias{names<-,flint,NULL-method}
\alias{names<-,flint,character-method}
\alias{print,flint-method}
\alias{quantile,flint-method}
\alias{rep,flint-method}
\alias{rep.int,flint-method}
\alias{rep_len,flint-method}
\alias{show,flint-method}
\alias{summary,flint-method}
\alias{unique,flint-method}
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
      an external pointer.  The protected field is an integer vector of
      length 1 or 2 storing the object length whose size is 32 or 64
      bits depending on the ABI; see \code{\link{flintABI}}.  The
      pointer field contains the address of a block of allocated memory
      of size greater than or equal to the object length times the size
      of the \acronym{FLINT} \proglang{C} type.  It is a null pointer if
      and only if the object length is zero.\cr
      \cr
      Methods for \code{\link{initialize}} set a finalizer on
      \code{.xData} (see \code{\link{reg.finalizer}}) to ensure that
      allocated memory is freed before \code{.xData} is itself freed by
      the garbage collector.}
    \item{\code{names}}{
      a character vector of length 0, indicating that the object is
      unnamed, or of length equal to the object length.  A corollary is
      that objects whose length exceeds the maximum length of a
      character vector cannot have names.}
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
      values.  The class of the return value is a superclass of (if not
      exactly) the class of \code{x}.}
    \item{\code{[<-}}{
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      returns the \code{flint} vector obtained by replacing the elements
      of \code{x} indexed by \code{i} (the \dQuote{subscript}) with
      elements of \code{value}, which are recycled as necessary.  The
      subscript can be logical, integer, or double or missing or
      \code{NULL}.  It is an error to supply \code{NA} or out of bounds
      subscripts: the underlying \proglang{C} types have no
      representation for missing values.  The class of the return value
      is the most specific subclass of \code{flint} whose range contains
      the ranges of the classes of \code{x} and \code{value}.}
    \item{\code{[[}}{
      \code{signature(x = "flint", i = "ANY", j = "missing")}:\cr
      equivalent to code \code{[} with the restriction that \code{i}
      must index exactly one element.}
    \item{\code{[[<-}}{
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      equivalent to code \code{[<-} with the restriction that \code{i}
      must index exactly one element.}
    \item{\code{all.equal}}{
      \code{signature(x = "ANY", y = "flint")}:\cr
      \code{signature(x = "flint", y = "ANY")}:\cr
      \code{signature(x = "flint", y = "flint")}:\cr
      returns either \code{TRUE}, indicating that there is no meaningful
      difference between \code{x} and \code{y}, or a character vector
      describing differences.  The implementation (including optional
      arguments) is adapted from \code{\link{all.equal.numeric}}, hence
      see \emph{its} documentation.  Notably, comparison of objects
      inheriting from different subclasses of virtual class \code{flint}
      and comparison with objects (typically atomic vectors) coercible
      to virtual class \code{flint} are supported with
      \code{check.class = FALSE}.  See \code{\link{flintIdentical}} for
      much more strict comparison objects inheriting from \code{flint}.}
    \item{\code{anyDuplicated}}{
      \code{signature(x = "flint")}:\cr
      returns \code{anyDuplicated(mtfrm(x), \dots)}.}
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
    \item{\code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}}{
      \code{signature(x = "flint")}:\cr
      coerces the argument with \code{as.vector} and dispatches.}
    \item{\code{as.data.frame}}{
      \code{signature(x = "flint")}:\cr
      this method is a copy of \code{\link{as.data.frame.vector}}.}
    \item{\code{c}}{
      \code{signature(x = "flint")}:\cr
      returns \code{\link{c.flint}(x, \dots)}, the concatenation of the
      arguments.  Function \code{c.flint} is exported to work around the
      fact that \code{\link{c}(x, \dots)} dispatches only on \code{x}.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "flint")}:\cr
      coerces atomic (except character) vectors \code{from} to the most
      specific subclass of \code{flint} whose range contains the range
      of \code{typeof(from)}.}
    \item{\code{duplicated}}{
      \code{signature(x = "flint")}:\cr
      returns \code{duplicated(mtfrm(x), \dots)}.}
    \item{\code{is.na<-}}{
      \code{signature(x = "flint")}:\cr
      returns the value of \code{x} after \code{x[value] <- \var{na}},
      where \code{\var{na}} is an \code{\link{NA}} of integer, double,
      or complex type, depending on the class of \code{x}.}
    \item{\code{length}}{
      \code{signature(x = "flint")}:\cr
      returns \code{\link{flintLength}(x, exact = FALSE)}.}
    \item{\code{length<-}}{
      \code{signature(x = "flint")}:\cr
      returns a \code{flint} vector of length given by the second
      argument \code{value}.  The first \code{min(length(x), value)}
      elements are copied from \code{x} and the remaining elements are
      initialized to zero.}
    \item{\code{mtfrm}}{
      \code{signature(x = "flint")}:\cr
      returns \code{format(x, base = 62L, digits = 0L)}, a character
      vector representing the elements of \code{x} exactly in base 62
      (chosen over smaller bases to reduce the number of characters in
      the output); see also \code{\link{format-methods}}.}
    \item{\code{names}}{
      \code{signature(x = "flint")}:\cr
      returns the value of the \code{names} slot or \code{NULL},
      \code{NULL} if the \code{names} slot has zero length.}
    \item{\code{names<-}}{
      \code{signature(x = "flint", value = "NULL")}:\cr
      returns \code{x} with \code{names} slot set to a character vector
      of length zero.}
    \item{\code{names<-}}{
      \code{signature(x = "flint", value = "character")}:\cr
      returns \code{x} with \code{names} slot set to \code{value}.
      Attributes of \code{value} are stripped.
      \code{\link{NA_character_}} are appended to \code{value} if its
      length is less than the length of \code{x}.  An error is signaled
      if its length is greater.}
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
      representation for missing values.}
    \item{\code{rep.int}, \code{rep_len}}{
      \code{signature(x = "flint")}:\cr
      analogues of \code{rep(x, times=)} and
      \code{rep(x, length.out=)} not preserving names, faster than
      \code{rep} when \code{x} has names.}
    \item{\code{show}}{
      \code{signature(object = "flint")}:\cr
      prints \code{\link{format}(object)} and returns \code{NULL}
      invisibly.}
    \item{\code{unique}}{
      \code{signature(x = "flint")}:\cr
      returns \code{x[!duplicated(x, \dots)]}.}
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
