\name{flint-class}
\alias{flint-class}
\alias{[,ANY,flint,missing,missing-method}
\alias{[,flint,ANY,missing,missing-method}
\alias{[,flint,flint,missing,missing-method}
\alias{[<-,ANY,ANY,missing,flint-method}
\alias{[<-,ANY,flint,missing,ANY-method}
\alias{[<-,ANY,flint,missing,flint-method}
\alias{[<-,flint,ANY,missing,ANY-method}
\alias{[<-,flint,ANY,missing,flint-method}
\alias{[<-,flint,flint,missing,ANY-method}
\alias{[<-,flint,flint,missing,flint-method}
\alias{[[,ANY,flint,missing-method}
\alias{[[,flint,ANY,missing-method}
\alias{[[,flint,flint,missing-method}
\alias{[[<-,ANY,ANY,missing,flint-method}
\alias{[[<-,ANY,flint,missing,ANY-method}
\alias{[[<-,ANY,flint,missing,flint-method}
\alias{[[<-,flint,ANY,missing,ANY-method}
\alias{[[<-,flint,ANY,missing,flint-method}
\alias{[[<-,flint,flint,missing,ANY-method}
\alias{[[<-,flint,flint,missing,flint-method}
\alias{anyDuplicated,flint-method}
\alias{as.raw,flint-method}
\alias{as.logical,flint-method}
\alias{as.integer,flint-method}
\alias{as.double,flint-method} % stub
\alias{as.numeric,flint-method}
\alias{as.complex,flint-method}
\alias{as.matrix,flint-method}
\alias{as.array,flint-method}
\alias{as.data.frame,flint-method}
\alias{as.Date,flint-method}
\alias{as.POSIXct,flint-method}
\alias{as.POSIXlt,flint-method}
\alias{all.equal,ANY,flint-method}
\alias{all.equal,flint,ANY-method}
\alias{all.equal,flint,flint-method}
\alias{c,flint-method}
\alias{coerce,ANY,flint-method}
\alias{cut,flint-method}
\alias{duplicated,flint-method}
\alias{findInterval,flint-method}
\alias{is.na<-,flint-method}
\alias{length,flint-method}
\alias{length<-,flint-method}
\alias{match,ANY,flint-method}
\alias{match,flint,ANY-method}
\alias{match,flint,flint-method}
\alias{mtfrm,flint-method}
\alias{names,flint-method}
\alias{names<-,flint,NULL-method}
\alias{names<-,flint,character-method}
\alias{print,flint-method}
\alias{quantile,flint-method}
\alias{rep,flint-method}
\alias{rep.int,flint-method}
\alias{rep_len,flint-method}
\alias{seq,flint-method}
\alias{sequence,flint-method}
\alias{show,flint-method}
\alias{summary,flint-method}
\alias{unique,flint-method}
\docType{class}
\title{Class of FLINT-Type Vectors}
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
      \code{signature(x = "ANY", i = "flint", j = "missing", drop = "missing")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing", drop = "missing")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing", drop = "missing")}:\cr
      return a traditional vector or a \code{flint} vector containing
      the elements of \code{x} indexed by \code{i} (the
      \dQuote{subscript}).  The subscript can be missing, \code{NULL},
      logical, integer, double, character, \code{\linkS4class{ulong}},
      \code{\linkS4class{slong}}, \code{\linkS4class{fmpz}}, or
      \code{\linkS4class{fmpq}}.  Methods for signatures with
      \code{x = "flint"} signal an error for \code{NA} and out of bounds
      subscripts, as the \proglang{C} types interfaced by \code{flint}
      vectors have no representation for missing values.  Note that
      \code{\link{[}} does not perform S4 dispatch if its first
      positional argument is not an S4 object.  If it is known that
      \code{i} is a \code{flint} vector and not known whether \code{x}
      is a \code{flint} vector, then consider programming defensively
      by calling \code{\link{[}} as \code{`[`(i = i, x = x)} rather than
      as \code{x[i]}.}
    \item{\code{[<-}}{
      \code{signature(x = "ANY", i = "ANY", j = "missing", value = "flint")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "missing", value = "flint")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "flint")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing", value = "flint")}:\cr
      return the traditional vector or \code{flint} vector obtained by
      replacing the elements of \code{x} indexed by \code{i} (the
      \dQuote{subscript}) with elements of \code{value}, which are
      recycled as necessary.  The subscript can be missing, \code{NULL},
      logical, integer, double, character, \code{\linkS4class{ulong}},
      \code{\linkS4class{slong}}, \code{\linkS4class{fmpz}}, or
      \code{\linkS4class{fmpq}}.  The class of the return value is
      determined following strict rules from the classes of \code{x} and
      \code{value}, which are promoted to the value class as necessary.
      If the value class is a subclass of \code{flint}, then an error is
      signaled for \code{NA} and out of bounds subscripts, as the
      \proglang{C} types interfaced by \code{flint} vectors have no
      representation for missing values.  Note that \code{\link{[<-}}
      does not perform S4 dispatch if its first positional argument is
      not an S4 object.  If it is known that \code{i} is a \code{flint}
      vector and not known whether \code{x} is a \code{flint} vector,
      then consider calling \code{\link{[<-}} as
      \code{`[`(i = i, x = x) <- value} rather than as
      \code{x[i] <- value}.  If it known that \code{value} is a
      \code{flint} vector and not known whether \code{x} or \code{i} is
      a \code{flint} vector, then consider doing something like
      \code{x <- `[<-`(value = value, x = x, i = i)}.}
    \item{\code{[[}}{
      \code{signature(x = "ANY", i = "flint", j = "missing")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing")}:\cr
      similar to \code{[}, with differences as documented in
      \code{\link{Extract}}.}
    \item{\code{[[<-}}{
      \code{signature(x = "ANY", i = "ANY", j = "missing", value = "flint")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "missing", value = "flint")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "flint", i = "ANY", j = "missing", value = "flint")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing", value = "ANY")}:\cr
      \code{signature(x = "flint", i = "flint", j = "missing", value = "flint")}:\cr
      similar to \code{[<-}, with differences as documented in
      \code{\link{Extract}}.}
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
      much stricter comparison of objects inheriting from \code{flint}.}
    \item{\code{anyDuplicated}}{
      \code{signature(x = "flint")}:\cr
      returns \code{anyDuplicated(mtfrm(x), \dots)}.}
    \item{\code{as.raw}, \code{as.logical}, \code{as.integer}, \code{as.numeric}, \code{as.complex}}{
      \code{signature(x = "flint")}:\cr
      returns the value of \code{as.vector(x, mode = *)}.  Methods for
      \code{as.vector} must be defined for subclasses of \code{flint}.
      Note that \code{as.double} dispatches internally the method for
      \code{as.numeric}, so there is no method for \code{as.double};
      see also \code{\link{as.numeric}}, section \sQuote{S4 Methods}.}
    \item{\code{as.matrix}, \code{as.array}, \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}}{
      \code{signature(x = "flint")}:\cr
      coerces the argument with \code{as.vector} and dispatches.}
    \item{\code{as.data.frame}}{
      \code{signature(x = "flint")}:\cr
      this method is a copy of \code{\link{as.data.frame.vector}}.  It
      enables the construction of data frames containing \code{flint}
      vectors using \code{as.data.frame} and functions that call it such
      as \code{\link{data.frame}} and \code{\link{cbind.data.frame}}.}
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
    \item{\code{cut}}{
      \code{signature(x = "flint")}:\cr
      returns
      \code{findInterval(x=x, vec=breaks, left.open=right, rightmost.closed=include.lowest)},
      hence see below.  The behaviour is consistent with the default
      method for \code{\link{cut}} with argument \code{labels} set to
      \code{FALSE}, provided that \code{breaks} is sorted and no element
      of \code{x} is out of bounds.}
    \item{\code{duplicated}}{
      \code{signature(x = "flint")}:\cr
      returns \code{duplicated(mtfrm(x), \dots)}.}
    \item{\code{findInterval}}{
      returns a \code{\linkS4class{ulong}} vector of length equal to the
      length of \code{x}, following the documented behaviour of the
      \pkg{base} function, hence see \code{\link{findInterval}}.  A
      caveat is that an error is signaled if \code{x} contains
      \code{NaN}, because \code{ulong} has no representation for \R{}'s
      missing value \code{\link{NA_integer_}}.}
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
    \item{\code{match}}{
      \code{signature(x = "ANY", table = "flint")}:\cr
      \code{signature(x = "flint", table = "ANY")}:\cr
      \code{signature(x = "flint", table = "flint")}:\cr
      returns an integer vector matching \code{x} to \code{table} after
      coercing to a common class then \dQuote{match transforming} with
      \code{\link{mtfrm}}.  The behaviour is parallel to that of the
      default method, hence see \code{\link{match}}.}
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
      prints \code{\link{format}(x)} without quotes and returns \code{x}
      invisibly.  The output has a header listing the class and length
      of \code{x} and the address stored by its \code{.xData} slot.  If
      the output might be differenced by \code{\link[tools]{Rdiff}},
      then one can set optional argument \code{Rdiff} to \code{TRUE} to
      indicate that the address should be formatted as
      \code{<pointer: 0x...>} rather than as \code{0x...}, as the longer
      format is recognized and ignored by \code{\link[tools]{Rdiff}}.
      The default value \code{NULL} is equivalent to
      \code{getOption("flint.Rdiff", FALSE)}.  For greater control over
      output, consider doing \code{print(format(x, \dots), \dots)}
      instead of \code{print(x, \dots)}.}
    \item{\code{quantile}}{
      \code{signature(x = "flint")}:\cr
      returns a \code{flint} vector containing sample quantiles computed
      according to additional arguments \code{probs} and \code{type};
      see \code{\link{quantile}}.  Currently, an error is is signaled
      for \code{x} of length zero and \code{x} containing \code{NaN}.}
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
    \item{\code{seq}}{
      \code{signature(\dots = "flint")}:\cr
      generates \code{flint} vectors whose elements are equally spaced.
      This method is dispatched by calls to \code{seq} or \code{seq.int}
      in which the first positional argument is a \code{flint} vector.
      Accepted usage is any of \preformatted{seq(length.out=)
seq(length.out=, by=)
seq(from=, to=)
seq(from=, to=, by=)
seq(from=, to=, length.out=)
seq(from=, by=, length.out=)
seq(to=, by=, length.out=)}
      where \code{length.out=n} and \code{along.with=x} are equivalent
      for \code{x} of length \code{n}.  Good users name all arguments.}
    \item{\code{sequence}}{
      \code{signature(nvec = "flint")}:\cr
      returns the concatenation of
      \code{seq(from = from[i], by = by[i], length.out = nvec[i])}
      after recycling arguments \code{nvec}, \code{from}, and \code{by}
      to a common length.}
    \item{\code{show}}{
      \code{signature(object = "flint")}:\cr
      prints \code{\link{format}(object)} and returns \code{NULL}
      invisibly.}
    \item{\code{summary}}{
      \code{signature(object = "flint")}:\cr
      returns a \code{flint} vector containing the minimum, first
      quartile, median, mean, third quartile, maximum, and (if nonzero)
      the number of \code{NaN}, unless \code{object} is complex
      (inherits from \code{\linkS4class{acf}} or
      \code{\linkS4class{acb}}) or \code{x} has error bounds (inherits
      from \code{\linkS4class{arb}} or \code{\linkS4class{acb}}) or
      optional argument \code{triple} is \code{TRUE}, in which case the
      value is just \code{\link{flintTriple}()} with names.}
    \item{\code{unique}}{
      \code{signature(x = "flint")}:\cr
      returns \code{x[!duplicated(x, \dots)]}.}
  }
  Methods are on purpose \emph{not} defined for generic functions whose
  default methods correctly handle objects inheriting from virtual class
  \code{flint}, typically by calling \emph{other} generic functions for
  which methods \emph{are} defined.  Examples are
  \code{\link{as.character}}, \code{\link{as.list}}, \code{\link{diff}},
  \code{\link{rev}}, \code{\link{seq.int}}, \code{\link{sort}}, and
  \code{\link{split}}.
}
\seealso{
  The nonvirtual subclasses:
  \code{\linkS4class{ulong}}, \code{\linkS4class{slong}},
  \code{\linkS4class{fmpz}}, \code{\linkS4class{fmpq}},
  \code{\linkS4class{mag}},
  \code{\linkS4class{arf}}, \code{\linkS4class{acf}},
  \code{\linkS4class{arb}}, and \code{\linkS4class{acb}}.
}
\examples{
showClass("flint")
showMethods(classes = "flint")
}
\keyword{classes}
