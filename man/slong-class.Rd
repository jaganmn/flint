\name{slong-class}
\alias{slong-class}
\alias{ulong-class}
\alias{!,slong-method}
\alias{!,ulong-method}
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
\alias{Ops,ANY,slong-method}
\alias{Ops,ANY,ulong-method}
\alias{Ops,slong,ANY-method}
\alias{Ops,ulong,ANY-method}
\alias{Ops,slong,slong-method}
\alias{Ops,ulong,slong-method}
\alias{Ops,slong,ulong-method}
\alias{Ops,ulong,ulong-method}
\alias{Ops,slong,fmpz-method}
\alias{Ops,ulong,fmpz-method}
\alias{Ops,slong,fmpq-method}
\alias{Ops,ulong,fmpq-method}
\alias{Ops,slong,arf-method}
\alias{Ops,ulong,arf-method}
\alias{Ops,slong,mag-method}
\alias{Ops,ulong,mag-method}
\alias{Ops,slong,arb-method}
\alias{Ops,ulong,arb-method}
\alias{Ops,slong,acb-method}
\alias{Ops,ulong,acb-method}
\alias{Summary,slong-method}
\alias{Summary,ulong-method}
\alias{as.vector,slong-method}
\alias{as.vector,ulong-method}
\alias{coerce,ANY,slong-method}
\alias{coerce,ANY,ulong-method}
\alias{initialize,slong-method}
\alias{initialize,ulong-method}
\alias{is.finite,slong-method}
\alias{is.finite,ulong-method}
\alias{is.infinite,slong-method}
\alias{is.infinite,ulong-method}
\alias{is.na,slong-method}
\alias{is.na,ulong-method}
\alias{is.nan,slong-method}
\alias{is.nan,ulong-method}
\title{Classes of Vectors of Signed or Unsigned Integers}
\docType{class}
\description{
  Classes \code{slong} and \code{ulong} extend virtual class
  \code{\linkS4class{flint}}.  They represent vectors of signed and
  unsigned integers, respectively.  The integer size is 32 or 64 bits,
  depending on the configuration of the linked \acronym{FLINT} library.
  The number of bits is given by \code{\link{flintBits}()}.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{!}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      coerces the argument to \code{\linkS4class{fmpz}} and dispatches.}
    \item{\code{+, -}}{
      \code{signature(e1 = "slong", e2 = "missing")}:\cr
      \code{signature(e1 = "ulong", e2 = "missing")}:\cr
      coerces the argument to \code{\linkS4class{fmpz}} and dispatches.}
    \item{\code{Complex}}{
      \code{signature(z = "slong")}:\cr
      \code{signature(z = "ulong")}:\cr
      coerces the argument to \code{\linkS4class{fmpz}} and dispatches.}
    \item{\code{Math, Math2, Summary}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      coerces the argument to \code{\linkS4class{fmpz}} and dispatches.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "slong")}:\cr
      \code{signature(e1 = "ANY", e2 = "ulong")}:\cr
      \code{signature(e1 = "slong", e2 = "ANY")}:\cr
      \code{signature(e1 = "ulong", e2 = "ANY")}:\cr
      coerces \code{slong} and \code{ulong} operands to a different
      subclass of \code{\linkS4class{flint}} (never \code{slong} or
      \code{ulong}, but always a class that can represent their range)
      and dispatches.}
    \item{\code{as.vector}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number in the direction of zero.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "slong")}:\cr
      \code{signature(from = "ANY", to = "ulong")}:\cr
      returns \code{new("slong", x = from)} or
      \code{new("ulong", x = from)}.}
    \item{\code{format}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      returns a character vector suitable for printing.  Optional
      arguments control the output; see \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "slong", length = "\var{numeric}")}:\cr
      \code{signature(.Object = "ulong", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "slong", x = "\var{atomic|flint}")}:\cr
      \code{signature(.Object = "ulong", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x} truncated towards zero.  An
      error is signaled if any \code{x[i]} is not in the range of the
      \proglang{C} type, in particular if any \code{x[i]} is \code{NaN}.
      The range is \eqn{(-2^{n-1}-1, 2^{n-1})} for \code{slong} and
      \eqn{(-1, 2^{n})} for \code{ulong}, where \eqn{n} is the value of
      \code{\link{flintBits}()}.}
    \item{\code{is.na, is.nan, is.infinite}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      returns a logical vector whose elements are all \code{FALSE}, as
      \code{slong} and \code{ulong} have no representation for
      \code{NaN}, \code{-Inf}, and \code{Inf}.}
    \item{\code{is.finite}}{
      \code{signature(x = "slong")}:\cr
      \code{signature(x = "ulong")}:\cr
      returns a logical vector whose elements are all \code{TRUE}, as
      \code{slong} and \code{ulong} have no representation for
      \code{NaN}, \code{-Inf}, and \code{Inf}.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  types: \url{https://flintlib.org/doc/flint.html}
}
\examples{
showClass("slong")
showClass("ulong")
showMethods(classes = c("slong", "ulong"))
}
