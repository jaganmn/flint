setClass("flint",
         contains = "VIRTUAL",
         slots = c(length = "integer", x = "externalptr"),
         prototype = list(length = integer(2L)),
         validity = function (.Object) {
             if (length(.Object@length) != 2L)
                 gettextf("length of '%s' slot is not %d", "length", 2L)
             else TRUE
         })

setClass("fmpz", contains = "flint")
setClass("fmpq", contains = "flint")
setClass( "mag", contains = "flint")
setClass( "arf", contains = "flint")
setClass( "arb", contains = "flint")
setClass( "acb", contains = "flint")
