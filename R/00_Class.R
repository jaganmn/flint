setClass("flint",
         contains = "VIRTUAL",
         slots = c(length = "integer", x = "externalptr"),
         prototype = list(length = integer(2L)),
         validity = function (object) {
             if (length(object@length) != 2L)
                 gettextf("length of '%s' slot is not %d", "length", 2L)
             else TRUE
         })

setClass("slong", contains = "flint")
setClass("ulong", contains = "flint")
setClass( "fmpz", contains = "flint")
setClass( "fmpq", contains = "flint")
setClass(  "arf", contains = "flint")
setClass(  "mag", contains = "flint")
setClass(  "arb", contains = "flint")
setClass(  "acb", contains = "flint")
