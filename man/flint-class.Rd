\name{flint-class}
\alias{flint-class}
\alias{flint}
\alias{flint.array}
\alias{$,flint-method}
\alias{$<-,flint-method}
\alias{[,ANY,ANY,flint-method}
\alias{[,ANY,flint,ANY-method}
\alias{[,ANY,flint,flint-method}
\alias{[,flint,ANY,ANY-method}
\alias{[,flint,ANY,flint-method}
\alias{[,flint,flint,ANY-method}
\alias{[,flint,flint,flint-method}
\alias{[<-,ANY,ANY,ANY,flint-method}
\alias{[<-,ANY,ANY,flint,ANY-method}
\alias{[<-,ANY,ANY,flint,flint-method}
\alias{[<-,ANY,flint,ANY,ANY-method}
\alias{[<-,ANY,flint,ANY,flint-method}
\alias{[<-,ANY,flint,flint,ANY-method}
\alias{[<-,ANY,flint,flint,flint-method}
\alias{[<-,flint,ANY,ANY,ANY-method}
\alias{[<-,flint,ANY,ANY,flint-method}
\alias{[<-,flint,ANY,flint,ANY-method}
\alias{[<-,flint,ANY,flint,flint-method}
\alias{[<-,flint,flint,ANY,ANY-method}
\alias{[<-,flint,flint,ANY,flint-method}
\alias{[<-,flint,flint,flint,ANY-method}
\alias{[<-,flint,flint,flint,flint-method}
\alias{[[,ANY,ANY,flint-method}
\alias{[[,ANY,flint,ANY-method}
\alias{[[,ANY,flint,flint-method}
\alias{[[,flint,ANY,ANY-method}
\alias{[[,flint,ANY,flint-method}
\alias{[[,flint,flint,ANY-method}
\alias{[[,flint,flint,flint-method}
\alias{[[<-,ANY,ANY,ANY,flint-method}
\alias{[[<-,ANY,ANY,flint,ANY-method}
\alias{[[<-,ANY,ANY,flint,flint-method}
\alias{[[<-,ANY,flint,ANY,ANY-method}
\alias{[[<-,ANY,flint,ANY,flint-method}
\alias{[[<-,ANY,flint,flint,ANY-method}
\alias{[[<-,ANY,flint,flint,flint-method}
\alias{[[<-,flint,ANY,ANY,ANY-method}
\alias{[[<-,flint,ANY,ANY,flint-method}
\alias{[[<-,flint,ANY,flint,ANY-method}
\alias{[[<-,flint,ANY,flint,flint-method}
\alias{[[<-,flint,flint,ANY,ANY-method}
\alias{[[<-,flint,flint,ANY,flint-method}
\alias{[[<-,flint,flint,flint,ANY-method}
\alias{[[<-,flint,flint,flint,flint-method}
\alias{all.equal,ANY,flint-method}
\alias{all.equal,flint,ANY-method}
\alias{all.equal,flint,flint-method}
\alias{anyDuplicated,flint-method}
\alias{aperm,flint-method}
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
\alias{asplit,flint-method}
\alias{c,flint-method}
\alias{cbind2,ANY,flint-method}
\alias{cbind2,flint,ANY-method}
\alias{cbind2,flint,flint-method}
\alias{coerce,ANY,flint-method}
\alias{cut,flint-method}
\alias{diag,flint-method}
\alias{diag<-,flint-method}
\alias{dim,flint-method}
\alias{dim<-,flint,NULL-method}
\alias{dim<-,flint,numeric-method}
\alias{dimnames,flint-method}
\alias{dimnames<-,flint,NULL-method}
\alias{dimnames<-,flint,list-method}
\alias{drop,flint-method}
\alias{duplicated,flint-method}
\alias{eigen,flint-method}
\alias{findInterval,flint-method}
\alias{identical,flint,flint-method}
\alias{is.array,flint-method}
\alias{is.matrix,flint-method}
\alias{is.na<-,flint-method}
\alias{isSymmetric,flint-method}
\alias{kronecker,ANY,flint-method}
\alias{kronecker,flint,ANY-method}
\alias{kronecker,flint,flint-method}
\alias{length,flint-method}
\alias{length<-,flint-method}
\alias{match,ANY,flint-method}
\alias{match,flint,ANY-method}
\alias{match,flint,flint-method}
\alias{mtfrm,flint-method}
\alias{names,flint-method}
\alias{names<-,flint,NULL-method}
\alias{names<-,flint,character-method}
\alias{norm,flint,ANY-method}
\alias{norm,flint,missing-method}
\alias{outer,ANY,flint-method}
\alias{outer,flint,ANY-method}
\alias{outer,flint,flint-method}
\alias{print,flint-method}
\alias{qr,flint-method}
\alias{quantile,flint-method}
\alias{rbind2,ANY,flint-method}
\alias{rbind2,flint,ANY-method}
\alias{rbind2,flint,flint-method}
\alias{rcond,flint-method}
\alias{rep,flint-method}
\alias{rep.int,flint-method}
\alias{rep_len,flint-method}
\alias{scale,flint-method}
\alias{seq,flint-method}
\alias{sequence,flint-method}
\alias{show,flint-method}
\alias{summary,flint-method}
\alias{svd,flint-method}
\alias{t,flint-method}
\alias{unique,flint-method}
\docType{class}
\title{Class of FLINT-Type Vectors}
\description{
  Class \code{flint} is a virtual class representing vectors of any
  \acronym{FLINT} \proglang{C} type.  The \proglang{C} type is
  determined by the class attribute and interfaced exactly using \R{}'s
  external pointer type.
}
\usage{
## Class generator functions

flint(class, \dots)

flint.array(class, \dots)
}
\arguments{
  \item{class}{
    a character string giving the name of a nonvirtual subclass of
    \code{flint}, one of
    \code{"\linkS4class{ulong}"}, \code{"\linkS4class{slong}"},
    \code{"\linkS4class{fmpz}"}, \code{"\linkS4class{fmpq}"},
    \code{"\linkS4class{mag}"},
    \code{"\linkS4class{arf}"}, \code{"\linkS4class{acf}"},
    \code{"\linkS4class{arb}"}, and \code{"\linkS4class{acb}"}.}
  \item{\dots}{
    arguments passed to the class generator function corresponding to
    \code{class}.}
}
\value{
  An object of class \code{class} generated by the corresponding class
  generator function.  For example,
  \code{flint("\linkS4class{ulong}", \dots)} returns
  \code{\link{ulong}(\dots)} and
  \code{flint.array("\linkS4class{slong}", \dots)} returns
  \code{\link{slong.array}(\dots)}.
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
    \item{\code{dim}}{
      either \code{NULL}, indicating that the object is not an array, or
      an integer vector of length \code{d} greater than 0 and with
      product equal to the object length, indicating that the object is
      a \code{d}-dimensional array with dimensions \code{dim}.  Array
      entries are stored in colexicographic order, meaning that the
      first subscript moves fastest.}
    \item{\code{dimnames}}{
      either \code{NULL}, indicating that the object is not an array or
      is an array whose dimensions are not named, or a list of length
      \code{d} equal to \code{length(dim)} such that
      \code{dimnames[[i]]} is either \code{NULL} or a character vector
      of length \code{dim[[i]]}, for all \code{i} in \code{1L:d}.}
    \item{\code{names}}{
      either \code{NULL}, indicating that the object is not named, or a
      character vector of length equal to the object length.  A
      corollary is that objects whose length exceeds the maximum length
      of a character vector cannot have names.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{$}}, \code{\link{$<-}}}{
      \code{signature(x = "flint")}:\cr
      signals an error as \code{x} is \dQuote{atomic-like} and in any
      case not recursive or \code{NULL}.}
    \item{\code{\link{[}}}{
      \code{signature(x = "flint", i = "ANY", j = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "flint")}:\cr
      returns a traditional vector or \code{flint} vector containing the
      elements of \code{x} indexed by \code{(i, j, \dots)} (the
      \dQuote{subscript}).  The components of the subscript can be
      missing, \code{NULL}, logical, integer, double, character,
      \code{\linkS4class{ulong}}, \code{\linkS4class{slong}},
      \code{\linkS4class{fmpz}}, or \code{\linkS4class{fmpq}}.  Methods
      for signatures with \code{x = "flint"} signal an error for
      \code{NA} and out of bounds subscripts, as the \proglang{C} types
      interfaced by \code{flint} vectors have no representation for
      missing values.  Note that \code{[} does not perform S4 dispatch
      if its first positional argument is not an S4 object.  If it is
      known that \code{i} is a \code{flint} vector and not known whether
      \code{x} is a \code{flint} vector, then one option is to call
      \code{[} as \code{`[`(i = i, x = x)} rather than as \code{x[i]}.
      However, it is not guaranteed that such usage of \code{[}, which
      is mostly undocumented, continues to work in future versions of
      \R{}.}
    \item{\code{\link{[<-}}}{
      \code{signature(x = "flint", i = "ANY", j = "ANY", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "ANY", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "flint", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "ANY", value = "flint")}:\cr
      returns the traditional vector or \code{flint} vector obtained by
      replacing the elements of \code{x} indexed by \code{(i, j, \dots)}
      (the \dQuote{subscript}) with elements of \code{value}, which are
      recycled as necessary.  The components of the subscript can be
      missing, \code{NULL}, logical, integer, double, character,
      \code{\linkS4class{ulong}}, \code{\linkS4class{slong}},
      \code{\linkS4class{fmpz}}, or \code{\linkS4class{fmpq}}.  The
      class of the return value is determined following strict rules
      from the classes of \code{x} and \code{value}, which are promoted
      to the value class as necessary.  If the value class is a subclass
      of \code{flint}, then an error is signaled for \code{NA} and out
      of bounds subscripts, as the \proglang{C} types interfaced by
      \code{flint} vectors have no representation for missing values.
      Note that \code{[<-} does not perform S4 dispatch if its first
      positional argument is not an S4 object.  If it is known that
      \code{i} is a \code{flint} vector and not known whether \code{x}
      is a \code{flint} vector, then one option is to call \code{[<-} as
      \code{`[`(i = i, x = x) <- value} rather than as
      \code{x[i] <- value}.  However, it is not guaranteed that such
      usage of \code{[<-}, which is mostly undocumented, continues to
      work in future versions of \R{}.}
    \item{\code{\link{[[}}}{
      \code{signature(x = "flint", i = "ANY", j = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "flint")}:\cr
      similar to \code{[}, with differences as documented in
      \code{\link{Extract}}, particularly for recursive \code{x}.}
    \item{\code{\link{[[<-}}}{
      \code{signature(x = "flint", i = "ANY", j = "ANY", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "flint", j = "ANY", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "flint", value = "ANY")}:\cr
      \code{signature(x = "ANY", i = "ANY", j = "ANY", value = "flint")}:\cr
      similar to \code{[<-}, with differences as documented in
      \code{\link{Extract}}, particularly for recursive \code{x}.}
    \item{\code{\link{all.equal}}}{
      \code{signature(x = "flint", y = "flint")}:\cr
      \code{signature(x = "flint", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "flint")}:\cr
      returns either \code{TRUE}, indicating that there is no meaningful
      difference between \code{x} and \code{y}, or a character vector
      describing differences.  The implementation (including optional
      arguments) is adapted from \code{all.equal.numeric}, hence see
      \emph{its} documentation.  Notably, comparison of objects
      inheriting from different subclasses of virtual class \code{flint}
      and comparison with objects (typically atomic vectors) coercible
      to virtual class \code{flint} are supported with
      \code{check.class = FALSE}.  See the method for \code{identical}
      for much stricter comparison of \code{flint} objects.}
    \item{\code{\link{anyDuplicated}}}{
      \code{signature(x = "flint")}:\cr
      returns \code{anyDuplicated(mtfrm(x), \dots)}.}
    \item{\code{\link{aperm}}}{
      \code{signature(a = "flint")}:\cr
      returns the array obtained by permuting the dimensions of \code{a}
      according to a second argument \code{perm}, following the
      documented behaviour of the S3 default method.}
    \item{\code{\link{as.raw}}, \code{\link{as.logical}}, \code{\link{as.integer}}, \code{\link{as.numeric}}, \code{\link{as.complex}}}{
      \code{signature(x = "flint")}:\cr
      returns the value of \code{as.vector(x, mode = *)}.  Methods for
      \code{as.vector} are defined for subclasses of \code{flint}.  Note
      that \code{\link{as.double}} dispatches internally the method for
      \code{as.numeric}, so there is no method for \code{as.double};
      see \code{\link{as.numeric}}, section \sQuote{S4 methods}.}
    \item{\code{\link{as.matrix}}, \code{\link{as.array}}, \code{\link{as.Date}}, \code{\link{as.POSIXct}}, \code{\link{as.POSIXlt}}}{
      \code{signature(x = "flint")}:\cr
      coerces the argument with \code{as.vector}, restores dimensions,
      dimension names, and names, and dispatches.  \code{as.matrix} and
      \code{as.array} obtain the same result more efficiently.}
    \item{\code{\link{as.data.frame}}}{
      \code{signature(x = "flint")}:\cr
      behaves as \code{as.data.frame.vector},
      \code{as.data.frame.matrix}, or \code{as.data.frame.array},
      depending on the length of the \code{dim} slot.  It enables the
      construction of data frames containing \code{flint} vectors using
      \code{as.data.frame} and functions that call it such as
      \code{\link{data.frame}} and \code{\link{cbind.data.frame}}.}
    \item{\code{\link{asplit}}}{
      \code{signature(x = "flint")}:\cr
      returns a list array containing the marginal splits of \code{x}
      indicated by a second argument \code{MARGIN}, following the
      documented behaviour of the \pkg{base} function.}
    \item{\code{\link{c}}}{
      \code{signature(x = "flint")}:\cr
      returns \code{\link{c.flint}(x, \dots)}, the concatenation of the
      arguments.  Function \code{c.flint} is exported to work around the
      fact that \code{c(x, \dots)} dispatches only on \code{x}.}
    \item{\code{\link[methods]{cbind2}}}{
      \code{signature(x = "flint", y = "flint")}:\cr
      \code{signature(x = "flint", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "flint")}:\cr
      returns \code{\link{cbind.flint}(x, y, \dots)}, the horizontal
      concatenation of \code{x} and \code{y}.  These methods are
      dispatched by \code{\link{cbind}} in case of S3 dispatch
      ambiguities.}
    \item{\code{\link[methods]{coerce}}}{
      \code{signature(from = "ANY", to = "flint")}:\cr
      coerces atomic (except character) vectors \code{from} to the most
      specific subclass of \code{flint} whose range contains the range
      of \code{typeof(from)}.}
    \item{\code{\link{cut}}}{
      \code{signature(x = "flint")}:\cr
      returns
      \code{findInterval(x=x, vec=breaks, left.open=right, rightmost.closed=include.lowest)},
      hence see below.  The behaviour is consistent with the S3 default
      method with argument \code{labels} set to \code{FALSE}, provided
      that \code{breaks} is sorted and no element of \code{x} is out of
      bounds.}
    \item{\code{\link{diag}}}{
      \code{signature(x = "flint")}:\cr
      if \code{x} is a matrix, then returns a \code{flint} vector
      containing the diagonal entries of \code{x}; otherwise, returns a
      diagonal matrix with diagonal entries taken from \code{x}.
      Optional arguments \code{nrow}, \code{ncol}, and \code{names} are
      handled as by the \pkg{base} function.}
    \item{\code{\link{diag<-}}}{
      \code{signature(x = "flint", value = "ANY")}:\cr
      returns \code{x}, which must be a matrix, after setting its main
      diagonal to \code{value}, whose length must be equal to 1 or the
      length of \code{x}.  Arguments \code{x} and \code{value} are
      coerced to a common class following the rules used for general
      subassignment; see the methods for \code{[<-} and \code{[[<-}.}
    \item{\code{\link{dim}}}{
      \code{signature(x = "flint")}:\cr
      returns the \code{dim} slot of \code{x}.}
    \item{\code{\link{dim<-}}}{
      \code{signature(x = "flint", value = "NULL")}:\cr
      returns \code{x} with \code{dim} and \code{dimnames} slots set to
      \code{NULL}.}
    \item{\code{\link{dim<-}}}{
      \code{signature(x = "flint", value = "numeric")}:\cr
      returns \code{x} with \code{dim} slot set to \code{value} and
      \code{dimnames} slot set to \code{NULL}.  \code{value} of double
      type is coerced to integer.}
    \item{\code{\link{dimnames}}}{
      \code{signature(x = "flint")}:\cr
      returns the \code{dimnames} slot of \code{x}.}
    \item{\code{\link{dimnames<-}}}{
      \code{signature(x = "flint", value = "NULL")}:\cr
      returns \code{x} with \code{dimnames} slot set to \code{NULL}.}
    \item{\code{\link{dimnames<-}}}{
      \code{signature(x = "flint", value = "list")}:\cr
      returns \code{x} with \code{dimnames} slot set to \code{value}.
      Elements of \code{value} of a vector type are coerced to character
      using \code{as.character.default}.  Exceptionally, factors are
      coerced to character using \code{as.character.factor}.}
    \item{\code{\link{drop}}}{
      \code{signature(x = "flint")}:\cr
      returns \code{x} with \code{dim}, \code{dimnames}, and
      \code{names} slots modified, following the documented behaviour of
      the \pkg{base} function.}
    \item{\code{\link{duplicated}}}{
      \code{signature(x = "flint")}:\cr
      returns \code{duplicated(mtfrm(x), \dots)}.}
    \item{\code{\link{findInterval}}}{
      returns a \code{\linkS4class{ulong}} vector of length equal to the
      length of \code{x}, following the documented behaviour of the
      \pkg{base} function.  A caveat is that an error is signaled if
      \code{x} contains \code{NaN}, because \code{ulong} has no
      representation for \R{}'s missing value
      \code{\link{NA_integer_}}.}
    \item{\code{\link{identical}}}{
      \code{signature(x = "flint", y = "flint")}:\cr
      returns a logical indicating if \code{x} and \code{y} are
      \dQuote{exactly equal}.  Compared to the default method (which is
      the \pkg{base} function), this method handles the \code{.xData}
      slots of \code{x} and \code{y} specially: by default (if
      \code{extptr.as.ref} is \code{FALSE}), it does not test for
      equality of the stored pointers but rather for entrywise equality
      of the \emph{pointed to} arrays.  Hence by default the
      \code{.xData} slots are compared as if they were traditional
      numeric or complex vectors.}
    \item{\code{\link{is.array}}}{
      \code{signature(x = "flint")}:\cr
      returns a logical indicating if \code{x} has a non-\code{NULL}
      \code{dim} slot.}
    \item{\code{\link{is.matrix}}}{
      \code{signature(x = "flint")}:\cr
      returns a logical indicating if \code{x} has a \code{dim} slot of
      length 2.}
    \item{\code{\link{is.na<-}}}{
      \code{signature(x = "flint")}:\cr
      returns the value of \code{x} after \code{x[value] <- \var{na}},
      where \code{\var{na}} is an \code{\link{NA}} of integer, double,
      or complex type, depending on the class of \code{x}.}
    \item{\code{\link{isSymmetric}}}{
      \code{signature(x = "flint")}:\cr
      returns a logical indicating if \code{x} is a Hermitian matrix or
      if \code{x} is a symmetric matrix, depending on optional argument
      \code{trans}, following the documented behaviour of the S3 method
      for traditional matrices.}
    \item{\code{\link{kronecker}}}{
      \code{signature(X = "flint", Y = "flint")}:\cr
      \code{signature(X = "flint", Y = "ANY")}:\cr
      \code{signature(X = "ANY", Y = "flint")}:\cr
      these methods are copies of the \pkg{base} function
      \code{\link{.kronecker}} with calls to \code{\link{as.array}}
      substituted for calls to \code{\link{asArray}}, as only
      \code{asArray} preserves \code{flint} subclass inheritance.}
    \item{\code{\link{length}}}{
      \code{signature(x = "flint")}:\cr
      returns \code{\link{flintLength}(x, exact = FALSE)}.}
    \item{\code{\link{length<-}}}{
      \code{signature(x = "flint")}:\cr
      returns a \code{flint} vector of length given by the second
      argument \code{value}.  The first \code{min(length(x), value)}
      elements are copied from \code{x} and the remaining elements are
      initialized to zero.}
    \item{\code{\link{match}}}{
      \code{signature(x = "flint", table = "flint")}:\cr
      \code{signature(x = "flint", table = "ANY")}:\cr
      \code{signature(x = "ANY", table = "flint")}:\cr
      returns an integer vector matching \code{x} to \code{table} after
      coercing to a common class then \dQuote{match transforming} with
      \code{\link{mtfrm}}.  The behaviour is parallel to that of the
      \pkg{base} function.}
    \item{\code{\link{mtfrm}}}{
      \code{signature(x = "flint")}:\cr
      returns
      \code{format(x, base = 62L, digits = 0L, digits.mag = 0L)},
      a character vector representing the elements of \code{x} exactly
      in base 62 (chosen over smaller bases to reduce the number of
      characters in the output); see also \code{\link{format-methods}}.}
    \item{\code{\link{names}}}{
      \code{signature(x = "flint")}:\cr
      returns the value of the \code{names} slot.}
    \item{\code{\link{names<-}}}{
      \code{signature(x = "flint", value = "NULL")}:\cr
      returns \code{x} with \code{names} slot set to \code{NULL}.}
    \item{\code{\link{names<-}}}{
      \code{signature(x = "flint", value = "character")}:\cr
      returns \code{x} with \code{names} slot set to \code{value}.
      Attributes of \code{value} are stripped.
      \code{\link{NA_character_}} are appended to \code{value} if its
      length is less than the length of \code{x}.  An error is signaled
      if its length is greater.}
    \item{\code{\link{norm}}}{
      \code{signature(x = "flint")}:\cr
      returns the matrix norm of \code{x} as a \code{flint} vector of
      length 1.  The class of the return value can depend on the norm
      type indicated by argument \code{type}; see \code{\link{norm}}.}
    \item{\code{\link{outer}}}{
      \code{signature(X = "flint", Y = "flint")}:\cr
      \code{signature(X = "flint", Y = "ANY")}:\cr
      \code{signature(X = "ANY", Y = "flint")}:\cr
      these methods are copies of the \pkg{base} function
      \code{\link{outer}} with calls to \code{\link{as.vector}}
      substituted for calls to \code{\link{asVector}}, as only
      \code{asVector} preserves \code{flint} subclass inheritance.}
    \item{\code{\link{print}}}{
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
    \item{\code{\link[stats]{quantile}}}{
      \code{signature(x = "flint")}:\cr
      returns a \code{flint} vector containing sample quantiles computed
      according to additional arguments \code{probs} and \code{type};
      see \code{\link[stats]{quantile}}.  Currently, an error is is
      signaled for \code{x} of length zero and \code{x} containing
      \code{NaN}.}
    \item{\code{\link[methods]{rbind2}}}{
      \code{signature(x = "flint", y = "flint")}:\cr
      \code{signature(x = "flint", y = "ANY")}:\cr
      \code{signature(x = "ANY", y = "flint")}:\cr
      returns \code{\link{rbind.flint}(x, y, \dots)}, the vertical
      concatenation of \code{x} and \code{y}.  These methods are
      dispatched by \code{\link{rbind}} in case of S3 dispatch
      ambiguities.}
    \item{\code{\link{rep}}}{
      \code{signature(x = "flint")}:\cr
      repeats \code{x} (or elements of \code{x}) according to optional
      arguments \code{times}, \code{length.out}, and \code{each}.  The
      behaviour is parallel to that of the internal default method.  One
      difference is that
      \code{rep(\var{0-length}, length.out=\var{nonzero})} signals an
      error, because the underlying \proglang{C} types have no
      representation for missing values.}
    \item{\code{\link{rep.int}}, \code{\link{rep_len}}}{
      \code{signature(x = "flint")}:\cr
      analogues of \code{rep(x, times=)} and
      \code{rep(x, length.out=)} not preserving names, faster than
      \code{rep} when \code{x} has names.}
    \item{\code{\link{scale}}}{
      \code{signature(x = "flint")}:\cr
      returns the result of optionally centering and optionally scaling
      the columns of \code{x}, following the documented behaviour of the
      S3 default method.}
    \item{\code{\link{seq}}}{
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
    \item{\code{\link{sequence}}}{
      \code{signature(nvec = "flint")}:\cr
      returns the concatenation of
      \code{seq(from = from[i], by = by[i], length.out = nvec[i])}
      after recycling arguments \code{nvec}, \code{from}, and \code{by}
      to a common length.}
    \item{\code{\link[methods]{show}}}{
      \code{signature(object = "flint")}:\cr
      prints \code{\link{format}(object)} and returns \code{NULL}
      invisibly.}
    \item{\code{\link{summary}}}{
      \code{signature(object = "flint")}:\cr
      returns a \code{flint} vector containing the minimum, first
      quartile, median, mean, third quartile, maximum, and (if nonzero)
      the number of \code{NaN}, unless \code{object} is complex
      (inherits from \code{\linkS4class{acf}} or
      \code{\linkS4class{acb}}) or \code{x} has error bounds (inherits
      from \code{\linkS4class{arb}} or \code{\linkS4class{acb}}) or
      optional argument \code{triple} is \code{TRUE}, in which case the
      value is just \code{\link{flintTriple}()} with names.}
    \item{\code{\link{t}}}{
      \code{signature(x = "flint")}:\cr
      returns the transpose of \code{x} if \code{x} is a matrix,
      handling non-array \code{x} as \code{length(x)}-by-1 matrices.}
    \item{\code{\link{unique}}}{
      \code{signature(x = "flint")}:\cr
      if \code{x} is not an array or \code{MARGIN} is empty, returns
      a vector containing the unique elements of \code{x}; otherwise,
      returns an array containing the unique splits of \code{x} by
      margin \code{MARGIN}.  Elements (splits) of \code{x} are
      considered distinct if the corresponding elements (splits) of
      \code{mtfrm(x)} are not identical.}
  }
  Methods are on purpose \emph{not} defined for generic functions whose
  default methods correctly handle objects inheriting from virtual class
  \code{flint}, typically by calling \emph{other} generic functions for
  which methods \emph{are} defined.  Examples are
  \code{\link{as.character}}, \code{\link{as.list}}, \code{\link{rev}},
  \code{\link{seq.int}}, \code{\link{sort}}, and \code{\link{split}}.
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
