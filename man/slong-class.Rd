\name{slong-class}
\alias{slong-class}
\alias{ulong-class}
\alias{nslong-class}
\alias{nulong-class}
\alias{+,slong,missing-method}
\alias{+,ulong,missing-method}
\alias{-,slong,missing-method}
\alias{-,ulong,missing-method}
\alias{Complex,slong-method}
\alias{Complex,ulong-method}
\alias{Math,slong-method}
\alias{Math,ulong-method}
\alias{Math2,slong-method}
\alias{Math2,ulong-method}
\alias{Ops,slong,slong-method}
\alias{Ops,ulong,ulong-method}
\alias{Summary,slong-method}
\alias{Summary,ulong-method}
\alias{as.vector,slong-method}
\alias{as.vector,ulong-method}
\alias{coerce,ANY,slong-method}
\alias{coerce,ANY,ulong-method}
\alias{coerce,nslong,slong-method}
\alias{coerce,nulong,ulong-method}
\alias{coerce,slong,nslong-method}
\alias{coerce,ulong,nulong-method}
\alias{initialize,slong-method}
\alias{initialize,ulong-method}
\title{Classes of Vectors of Signed or Unsigned Integers}
\docType{class}
\description{
  Classes \code{slong} and \code{ulong} extend virtual class
  \code{\linkS4class{flint}}.  They represent vectors of signed and
  unsigned integers, respectively.  The integer size is 32 or 64 bits,
  depending on the configuration of the linked \acronym{FLINT} library.
  The number of bits is given by \code{\link{flintBits}()}.

  Classes \code{nslong} and \code{nulong} extend virtual class
  \code{\linkS4class{nflint}} and basic vector class
  \code{\linkS4class{integer}}.  They are the \dQuote{native} analogues
  of \code{slong} and \code{ulong}.
}
\section{Slots}{
  \code{slong}, \code{ulong}:
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{nslong}, \code{nulong}:
  \describe{
    \item{\code{.Data}}{
      a pseudo-slot whose \emph{apparent} value is the
      \dQuote{data part} of the object: a copy of the object with S4 bit
      unset and attributes removed.  \code{NA} represents integers not
      in \eqn{(-2^{31}, 2^{31})}, the range of \R{}'s integer type.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{as.vector}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      return \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero.}
    \item{\code{initialize}}{
      \code{signature(.Object = "slong", length = "numeric")}:\cr
      \code{signature(.Object = "ulong", length = "numeric")}:\cr
      return \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "slong", x = "numeric")}:\cr
      \code{signature(.Object = "ulong", x = "numeric")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if any \code{x[i]} is not in the range of the
      \proglang{C} type, in particular if any \code{x[i]} is \code{NaN}.
      The range is \eqn{(-2^{n-1}-1, 2^{n-1})} for \code{slong} and
      \eqn{(-1, 2^{n})} for \code{ulong}, where \eqn{n} is the value of
      \code{\link{flintBits}()}.}
    \item{\code{coerce}}{
      \code{signature(from = "vector", to = "slong")}:\cr
      \code{signature(from = "nslong", to = "slong")}:\cr
      \code{signature(from = "vector", to = "ulong")}:\cr
      \code{signature(from = "nulong", to = "ulong")}:\cr
      return \code{new("slong", x = from)} or
      \code{new("ulong", x = from)}.}
    \item{\code{coerce}}{
      \code{signature(from = "slong", to = "nslong")}:\cr
      \code{signature(from = "ulong", to = "nulong")}:\cr
      return an object of class \code{nslong} or \code{nulong}
      whose data part is \code{as.vector(from, "integer")}.}
    \item{\code{format}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      return a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  types: \url{https://flintlib.org/doc/flint.html}
}
\examples{
showClass("slong")
showClass("ulong")
showClass("nslong")
showClass("nulong")
}
