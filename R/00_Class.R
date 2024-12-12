setClass("flint",
         contains = c("VIRTUAL", "externalptr"),
         validity = function (object) flintValid(object))

setClass("slong", contains = "flint")
setClass("ulong", contains = "flint")
setClass( "fmpz", contains = "flint")
setClass( "fmpq", contains = "flint")
setClass(  "arf", contains = "flint")
setClass(  "mag", contains = "flint")
setClass(  "arb", contains = "flint")
setClass(  "acb", contains = "flint")
