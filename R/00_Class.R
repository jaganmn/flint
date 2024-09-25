## FLINT vectors represented directly using R external pointers

setClass("flint",
         contains = "VIRTUAL",
         slots = c(length = "integer", x = "externalptr"),
         prototype = list(length = integer(2L)),
         validity = function (object) flintValid(object))

setClass("slong", contains = "flint")
setClass("ulong", contains = "flint")
setClass( "fmpz", contains = "flint")
setClass( "fmpq", contains = "flint")
setClass(  "arf", contains = "flint")
setClass(  "mag", contains = "flint")
setClass(  "arb", contains = "flint")
setClass(  "acb", contains = "flint")


## FLINT vectors represented natively using R vectors

setClass("nflint",
         contains = "VIRTUAL")

setClass("nslong",
         contains = c("nflint", "integer"))

setClass("nulong",
         contains = c("nflint", "integer"),
         validity = function (object) {
             if (min(0L, object, na.rm = TRUE) < 0L)
                 gettext("vector contains negative numbers")
             else TRUE
         })

setClass("nfmpz",
         contains = c("nflint", "integer"))

setClass("nfmpq",
         contains = "nflint",
         slots = c(num = "nfmpz", den = "nfmpz"),
         validity = function (object) {
             if (length(object@num) != length(object@den))
                 gettextf("lengths of '%s' and '%s' slots are not equal", "num", "den")
             else if (!all(TRUE, object@den, na.rm = TRUE))
                 gettextf("'%s' slot contains zero", "den")
             else TRUE
         })

setClass("narf",
         contains = c("nflint", "numeric"),
         validity = function (object) {
             if (typeof(object) != "double")
                 gettextf("vector type is not \"%s\"", "double")
             else TRUE
         })

setClass("nmag",
         contains = c("nflint", "numeric"),
         validity = function (object) {
             if (typeof(object) != "double")
                 gettextf("vector type is not \"%s\"", "double")
             else if (is.na(m <- min(0, object)))
                 gettext("vector contains NaN")
             else if (m < 0)
                 gettext("vector contains negative numbers")
             else TRUE
         })

setClass("narb",
         contains = "nflint",
         slots = c(mid = "narf", rad = "nmag"),
         validity = function (object) {
             if (length(object@mid) != length(object@rad))
                 gettextf("lengths of '%s' and '%s' slots are not equal", "mid", "rad")
             else TRUE
         })

setClass("nacb",
         contains = "nflint",
         slots = c(real = "narb", imag = "narb"),
         validity = function (object) {
             if (length(object@real@mid) != length(object@imag@mid))
                 gettextf("lengths of '%s' and '%s' slots are not equal", "real", "imag")
             else TRUE
         })
