\name{arb-class}
\alias{arb-class}
\alias{narb-class}
\alias{+,arb,missing-method}
\alias{-,arb,missing-method}
\alias{Complex,arb-method}
\alias{Math,arb-method}
\alias{Math2,arb-method}
\alias{Ops,ANY,arb-method}
\alias{Ops,arb,ANY-method}
\alias{Ops,arb,slong-method}
\alias{Ops,arb,ulong-method}
\alias{Ops,arb,fmpz-method}
\alias{Ops,arb,fmpq-method}
\alias{Ops,arb,arf-method}
\alias{Ops,arb,mag-method}
\alias{Ops,arb,arb-method}
\alias{Ops,arb,acb-method}
\alias{Summary,arb-method}
\alias{as.vector,arb-method}
\alias{coerce,ANY,arb-method}
\alias{coerce,narb,arb-method}
\alias{coerce,arb,narb-method}
\alias{initialize,arb-method}
\alias{length,narb-method}
\alias{log,arb-method}
\title{Class of Real Numbers with Error Bounds}
\docType{class}
\description{
  Class \code{arb} extends virtual class \code{flint}.  It represents
  vectors of real numbers with error bounds.  Elements are specified by
  a pair of mixed format floating-point numbers: an
  \code{\linkS4class{arf}} midpoint and a \code{\linkS4class{mag}}
  radius.

  Class \code{narb} extends virtual class \code{\linkS4class{nflint}}.
  It is the \dQuote{native} analogue of \code{arb}.
}
\section{Slots}{
  \code{arb}:
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
  \code{narb}:
  \describe{
    \item{\code{mid}}{
      an \code{\linkS4class{narf}} vector listing midpoints.}
    \item{\code{rad}}{
      an \code{\linkS4class{nmag}} vector listing radii.}
  }
  The lengths of \code{mid} and \code{rad} must be equal.
}
\section{Methods}{
  Due to constraints imposed by generic functions, methods typically
  do \emph{not} provide a formal argument \code{prec} allowing for a
  precision to be indicated in the function call.  Such methods use
  the current default precision set by \code{\link{flintPrec}}.
  \describe{
    \item{\code{+}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "arb", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "arb")}:\cr
      a placeholder signaling a \dQuote{not yet implemented} error.}
    \item{\code{Math}}{
      \code{signature(x = "arb")}:\cr
      mathematical functions of one argument, where supported.}
    \item{\code{Math2}}{
      \code{signature(x = "arb")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "arb")}:\cr
      \code{signature(e1 = "arb", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators.  The
      \dQuote{other} operand must be atomic or inherit from virtual
      class \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "arb")}:\cr
      univariate summary statistics, returning a logical vector of
      length 1 (\code{any}, \code{all}) or an \code{arb} vector of
      length 1 or 2 (\code{sum}, \code{prod}, \code{min}, \code{max},
      \code{range}).}
    \item{\code{as.vector}}{
      \code{signature(x = "arb")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting the midpoints of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number (with precedence to even
      significands in case of ties), unless a midpoint exceeds
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "arb")}:\cr
      returns \code{new("arb", x = from)}.  An error is signaled if the
      class or type of \code{from} is not supported by the method for
      \code{\link{initialize}}.}
    \item{\code{coerce}}{
      \code{signature(from = "narb", to = "arb")}:\cr
      returns \code{new("arb", mid = from@mid, rad = from@rad)}.}
    \item{\code{coerce}}{
      \code{signature(from = "arb", to = "narb")}:\cr
      returns the analogous object of class \code{narb}.}
    \item{\code{format}}{
      \code{signature(x = "arb")}:\cr
      returns a character vector suitable for printing, using string
      format \code{"(m +/- r)"} and scientific format for \code{m} and
      \code{r}.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros (zero midpoint, zero radius).
      \code{.Object} is not copied, so all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized by the elements of \code{x}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arb", mid = "\var{atomic|flint}", rad = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array whose
      midpoints and radii are initialized separately with values from
      \code{mid} and \code{rad}, which are coerced to classes
      \code{\linkS4class{arf}} and \code{\linkS4class{mag}} and recycled
      as necessary.}
    \item{\code{length}}{
      \code{signature(x = "narb")}:\cr
      returns \code{length(x@mid)}.}
    \item{\code{log}}{
      \code{signature(x = "arb")}:\cr
      returns the logarithm of the argument.  The natural logarithm is
      computed by default (when optional argument \code{base} is
      unset).}
  }
}
\seealso{
  Virtual classes \code{\linkS4class{flint}} and
  \code{\linkS4class{nflint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arb.html}
}
\examples{
showClass("arb")
showMethods(classes = "arb")
showClass("narb")
showMethods(classes = "narb")
}
