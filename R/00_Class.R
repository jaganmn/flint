setClass("flint",
         contains = c("VIRTUAL", "externalptr"),
         slots = c(names = "character"),
         validity = function (object) .Call(R_flint_valid, object))

.ulong <- setClass("ulong", contains = "flint")
.slong <- setClass("slong", contains = "flint")
 .fmpz <- setClass( "fmpz", contains = "flint")
 .fmpq <- setClass( "fmpq", contains = "flint")
  .mag <- setClass(  "mag", contains = "flint")
  .arf <- setClass(  "arf", contains = "flint")
  .acf <- setClass(  "acf", contains = "flint")
  .arb <- setClass(  "arb", contains = "flint")
  .acb <- setClass(  "acb", contains = "flint")
