setClass("flint",
         contains = "VIRTUAL",
         slots = c(length = "integer", x = "externalptr"),
         prototype = list(length = 0L),
         validity = function (.Object) {
             n <- .Object@length
             if (n < 1L || n >= 3L)
                 gettextf("length of '%s' slot is not %d or %d",
                          "length", 1L, 2L)
             else TRUE
         })

setClass("fmpz", contains = "flint")
setClass("fmpq", contains = "flint")
setClass( "mag", contains = "flint")
setClass( "arf", contains = "flint")
setClass( "arb", contains = "flint")
setClass( "acb", contains = "flint")
