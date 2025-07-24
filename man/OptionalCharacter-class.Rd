\name{OptionalCharacter-class}
\alias{OptionalInteger-class}
\alias{OptionalList-class}
\alias{OptionalCharacter-class}
\title{Unions of \sQuote{NULL} and Vector Classes}
\description{
  Class unions in the style of \code{\linkS4class{OptionalFunction}}
  from package \pkg{methods}, whose purpose is to allow slots
  \code{dim}, \code{dimnames}, and \code{names} of virtual class
  \code{\linkS4class{flint}} to be \code{NULL} or a vector of suitable
  type.

  \code{OptionalInteger}, \code{OptionalList}, and
  \code{OptionalCharacter} are the unions of \code{NULL} and
  \code{integer}, \code{list}, and \code{character}, respectively.
}
\examples{
showClass("OptionalInteger")
showClass("OptionalList")
(oc <- getClass("OptionalCharacter"))

stopifnot(isVirtualClass(oc),
          isClassUnion(oc),
          all(c("NULL", "character") \%in\% names(oc@subclasses)),
          any(extends("NULL") == "OptionalCharacter"),
          any(extends("character") == "OptionalCharacter"))

getClass("flint")@slots
}
