\name{nflint-class}
\alias{nflint-class}
\title{Class of \dQuote{Native} Representations of FLINT-Type Vectors}
\docType{class}
\description{
  Class \code{nflint} is a virtual class representing vectors of any
  \acronym{FLINT} \proglang{C} type.  The \proglang{C} type is
  determined by the class attribute and interfaced inexactly (with less
  precision or narrower range) using \R{}'s basic vector types.

  The main purpose of \code{nflint} is to expose at \R{} level the
  elements of \code{flint} objects which for efficiency are hidden
  behind an external pointer.
}
\seealso{
  Virtual class \code{\linkS4class{flint}}.

  The nonvirtual subclasses:
  \code{\linkS4class{nslong}}, \code{\linkS4class{nulong}},
  \code{\linkS4class{nfmpz}}, \code{\linkS4class{nfmpq}},
  \code{\linkS4class{narf}}, \code{\linkS4class{nmag}},
  \code{\linkS4class{narb}}, and \code{\linkS4class{nacb}}.
}
\examples{
showClass("nflint")
}
