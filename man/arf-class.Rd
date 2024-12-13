\name{arf-class}
\alias{arf-class}
\alias{!,arf-method}
\alias{+,arf,missing-method}
\alias{-,arf,missing-method}
\alias{Complex,arf-method}
\alias{Math,arf-method}
\alias{Math2,arf-method}
\alias{Ops,ANY,arf-method}
\alias{Ops,arf,ANY-method}
\alias{Ops,arf,slong-method}
\alias{Ops,arf,ulong-method}
\alias{Ops,arf,fmpz-method}
\alias{Ops,arf,fmpq-method}
\alias{Ops,arf,arf-method}
\alias{Ops,arf,mag-method}
\alias{Ops,arf,arb-method}
\alias{Ops,arf,acb-method}
\alias{Summary,arf-method}
\alias{anyNA,arf-method}
\alias{as.vector,arf-method}
\alias{coerce,ANY,arf-method}
\alias{initialize,arf-method}
\alias{is.finite,arf-method}
\alias{is.infinite,arf-method}
\alias{is.na,arf-method}
\alias{is.nan,arf-method}
\alias{mean,arf-method}
\title{Class of Vectors of Arbitrary Precision Floating-Point Numbers}
\docType{class}
\description{
  Class \code{arf} extends virtual class \code{flint}.  It represents
  vectors of signed floating-point numbers with an arbitrary precision
  significand and an arbitrary precision exponent.
}
\section{Slots}{
  \describe{
    \item{\code{.xData}}{
      inherited from virtual class \code{\linkS4class{flint}}.}
  }
}
\section{Methods}{
  Due to constraints imposed by generic functions, methods typically
  do \emph{not} provide a formal argument \code{prec} allowing for a
  precision to be indicated in the function call.  Such methods use
  the current default precision set by \code{\link{flintPrec}}.
  \describe{
    \item{\code{!}}{
      \code{signature(x = "arf")}:\cr
      equivalent to (but faster than) \code{x == 0}.}
    \item{\code{+}}{
      \code{signature(e1 = "arf", e2 = "missing")}:\cr
      returns a copy of the argument.}
    \item{\code{-}}{
      \code{signature(e1 = "arf", e2 = "missing")}:\cr
      returns the negation of the argument.}
    \item{\code{Complex}}{
      \code{signature(z = "arf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.}
    \item{\code{Math}}{
      \code{signature(x = "arf")}:\cr
      mathematical functions of one argument; see
      \code{\link[methods]{S4groupGeneric}}.  Notably, the logarithmic,
      exponential, (inverse) trigonometric, (inverse) hyperbolic, and
      gamma-related member functions are not yet implemented.  Users
      wanting those can (for now) operate on
      \code{as(x, "\linkS4class{arb}")}.}
    \item{\code{Math2}}{
      \code{signature(x = "arf")}:\cr
      decimal rounding according to a second argument \code{digits}; see
      \code{\link[methods]{S4groupGeneric}}.  There are just two member
      member functions: \code{\link{round}}, \code{\link{signif}}.}
    \item{\code{Ops}}{
      \code{signature(e1 = "ANY", e2 = "arf")}:\cr
      \code{signature(e1 = "arf", e2 = "ANY")}:\cr
      binary arithmetic, comparison, and logical operators; see
      \code{\link[methods]{S4groupGeneric}}.  The \dQuote{other} operand
      must be atomic or inherit from virtual class
      \code{\linkS4class{flint}}.  The operands are recycled and
      promoted as necessary.}
    \item{\code{Summary}}{
      \code{signature(x = "arf")}:\cr
      univariate summary statistics; see
      \code{\link[methods]{S4groupGeneric}}.  The return value is a
      logical vector of length 1 (\code{any}, \code{all}) or an
      \code{arf} vector of length 1 or 2 (\code{sum}, \code{prod},
      \code{min}, \code{max}, \code{range}).}
    \item{\code{anyNA}}{
      \code{signature(x = "arf")}:\cr
      returns \code{TRUE} if any element of \code{x} is \code{NaN},
      \code{FALSE} otherwise.}
    \item{\code{as.vector}}{
      \code{signature(x = "arf")}:\cr
      returns \code{as.vector(y, mode)}, where \code{y} is a double
      vector containing the result of converting each element of
      \code{x} to the range of double, rounding if the value is not
      exactly representable in double precision.  The rounding mode is
      to the nearest representable number (with precedence to even
      significands in case of ties), unless the element exceeds
      \code{.Machine[["double.xmax"]]} in absolute value, in which case
      \code{-Inf} or \code{Inf} is introduced with a warning.}
    \item{\code{coerce}}{
      \code{signature(from = "ANY", to = "arf")}:\cr
      returns \code{new("arf", x = from)}.}
    \item{\code{format}}{
      \code{signature(x = "arf")}:\cr
      returns a character vector suitable for printing, using scientific
      format.  Optional arguments control the output; see
      \code{\link{format-methods}}.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arf", length = "\var{numeric}")}:\cr
      returns \code{.Object} after setting its \code{.xData} slot.  The
      new slot value is an external pointer to an array of the
      corresponding \proglang{C} type, which is newly allocated and
      initialized entirely with zeros.  \code{.Object} is not copied, so
      all references are affected.}
    \item{\code{initialize}}{
      \code{signature(.Object = "arf", x = "\var{atomic|flint}")}:\cr
      as above, except that the new slot value points to an array
      initialized with values from \code{x}.}
    \item{\code{is.na, is.nan}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{NaN}.}
    \item{\code{is.infinite}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are \code{-Inf} or \code{Inf}.}
    \item{\code{is.finite}}{
      \code{signature(x = "arf")}:\cr
      returns a logical vector indicating which elements of \code{x}
      are not \code{NaN}, \code{-Inf}, or \code{Inf}.}
    \item{\code{mean}}{
      \code{signature(x = "arf")}:\cr
      returns the arithmetic mean.}
  }
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.
}
\references{
  The \acronym{FLINT} documentation of the underlying \proglang{C}
  type: \url{https://flintlib.org/doc/arf.html}
}
\examples{
showClass("arf")
showMethods(classes = "arf")
}
