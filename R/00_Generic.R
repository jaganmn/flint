setGeneric("Num",
           function (q)
               standardGeneric("Num"))

setGeneric("Num<-",
           function (q, value)
               standardGeneric("Num<-"))

setGeneric("Den",
           function (q)
               standardGeneric("Den"))

setGeneric("Den<-",
           function (q, value)
               standardGeneric("Den<-"))

setGeneric("Mid",
           function (x)
               standardGeneric("Mid"))

setGeneric("Mid<-",
           function (x, value)
               standardGeneric("Mid<-"))

setGeneric("Rad",
           function (x)
               standardGeneric("Rad"))

setGeneric("Rad<-",
           function (x, value)
               standardGeneric("Rad<-"))

setGeneric("Real",
           function (z)
               standardGeneric("Real"))

setGeneric("Real<-",
           function (z, value)
               standardGeneric("Real<-"))

setGeneric("Imag",
           function (z)
               standardGeneric("Imag"))

setGeneric("Imag<-",
           function (z, value)
               standardGeneric("Imag<-"))

setGeneric("isComplex",
           function (x)
               standardGeneric("isComplex"))

setGeneric("isFloating",
           function (x)
               standardGeneric("isFloating"))

setGeneric("isSigned",
           function (x)
               standardGeneric("isSigned"))

setMethod("Real",
          c(z = "ANY"),
          function (z)
              Re(z))

setMethod("Real<-",
          c(z = "ANY"),
          function (z, value) {
              stopifnot(!isComplex(value))
              value + Im(z) * 1i
          })

setMethod("Imag",
          c(z = "ANY"),
          function (z)
              Im(z))

setMethod("Imag<-",
          c(z = "ANY"),
          function (z, value) {
              stopifnot(!isComplex(value))
              Re(z) + value * 1i
          })

setMethod("isComplex",
          c(x = "ANY"),
          function (x)
              switch(typeof(x),
                     "raw" =, "logical" =, "integer" =, "double" =
                         FALSE,
                     "complex" =
                         TRUE,
                     stop(gettextf("%s(<%s>) is not yet implemented",
                                   .Generic, if (isS4(x)) class(x) else typeof(x)),
                          domain = NA)))

setMethod("isFloating",
          c(x = "ANY"),
          function (x)
              switch(typeof(x),
                     "raw" =, "logical" =, "integer" =
                         FALSE,
                     "double" =, "complex" =
                         TRUE,
                     stop(gettextf("%s(<%s>) is not yet implemented",
                                   .Generic, if (isS4(x)) class(x) else typeof(x)),
                          domain = NA)))

setMethod("isSigned",
          c(x = "ANY"),
          function (x)
              switch(typeof(x),
                     "raw" =
                         FALSE,
                     "logical" =, "integer" =, "double" =, "complex" =
                         TRUE,
                     stop(gettextf("%s(<%s>) is not yet implemented",
                                   .Generic, if (isS4(x)) class(x) else typeof(x)),
                          domain = NA)))
