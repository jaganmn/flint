setClass("flint",
         contains = "VIRTUAL",
         slots = c(length = "numeric", x = "externalptr"),
         prototype = list(length = integer(0L)))

setClass("fmpz", contains = "flint")
setClass("fmpq", contains = "flint")
setClass( "mag", contains = "flint")
setClass( "arf", contains = "flint")
setClass( "arb", contains = "flint")
setClass( "acb", contains = "flint")
