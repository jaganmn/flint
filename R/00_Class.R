setClassUnion("OptionalInteger"  , c("NULL", "integer"  ))
setClassUnion("OptionalList"     , c("NULL", "list"     ))
setClassUnion("OptionalCharacter", c("NULL", "character"))

setClass("flint",
         contains = c("VIRTUAL", "externalptr"),
         slots = c(dim = "OptionalInteger",
                   dimnames = "OptionalList",
                   names = "OptionalCharacter"),
         validity = function (object) .Call(R_flint_valid, object))

flint <-
new("classGeneratorFunction",
    function (class, ...)
        switch(class,
               ulong = ulong(...),
               slong = slong(...),
               fmpz = fmpz(...),
               fmpq = fmpq(...),
               mag = mag(...),
               arf = arf(...),
               acf = acf(...),
               arb = arb(...),
               acb = acb(...),
               stop(gettextf("invalid class name \"%s\"", class),
                    domain = NA)),
    className = "flint",
    package = "flint")

flint.array <-
new("classGeneratorFunction",
    function (class, ...)
        switch(class,
               ulong = ulong.array(...),
               slong = slong.array(...),
               fmpz = fmpz.array(...),
               fmpq = fmpq.array(...),
               mag = mag.array(...),
               arf = arf.array(...),
               acf = acf.array(...),
               arb = arb.array(...),
               acb = acb.array(...),
               stop(gettextf("invalid class name \"%s\"", class),
                    domain = NA)),
    className = "flint",
    package = "flint")

## FIXME: do below in for loop

setClass("ulong", contains = "flint")
setClass("slong", contains = "flint")
setClass( "fmpz", contains = "flint")
setClass( "fmpq", contains = "flint")
setClass(  "mag", contains = "flint")
setClass(  "arf", contains = "flint")
setClass(  "acf", contains = "flint")
setClass(  "arb", contains = "flint")
setClass(  "acb", contains = "flint")

ulong <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_ulong_initialize,
              flintNew("ulong"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names)
    },
    className = "ulong",
    package = "flint")

ulong.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        .Call(R_flint_ulong_initialize,
              flintNew("ulong"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL),
    className = "ulong",
    package = "flint")

slong <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_slong_initialize,
              flintNew("slong"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names)
    },
    className = "slong",
    package = "flint")

slong.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        .Call(R_flint_slong_initialize,
              flintNew("slong"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL),
    className = "slong",
    package = "flint")

fmpz <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_fmpz_initialize,
              flintNew("fmpz"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names)
    },
    className = "fmpz",
    package = "flint")

fmpz.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        .Call(R_flint_fmpz_initialize,
              flintNew("fmpz"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL),
    className = "fmpz",
    package = "flint")

fmpq <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL, num = 0L, den = 1L) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_fmpq_initialize,
              flintNew("fmpq"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names,
              if (!missing(num)) as(num, "fmpz"),
              if (!missing(den)) as(den, "fmpz"))
    },
    className = "fmpq",
    package = "flint")

fmpq.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL, num = 0L, den = 1L)
        .Call(R_flint_fmpq_initialize,
              flintNew("fmpq"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL,
              if (!missing(num)) as(num, "fmpz"),
              if (!missing(den)) as(den, "fmpz")),
    className = "fmpq",
    package = "flint")

mag <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_mag_initialize,
              flintNew("mag"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names)
    },
    className = "mag",
    package = "flint")

mag.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL)
        .Call(R_flint_mag_initialize,
              flintNew("mag"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL),
    className = "mag",
    package = "flint")

arf <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_arf_initialize,
              flintNew("arf"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names)
    },
    className = "arf",
    package = "flint")

arf.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL)
        .Call(R_flint_arf_initialize,
              flintNew("arf"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL),
    className = "arf",
    package = "flint")

acf <- ACF <-
new("classGeneratorFunction",
    function (x = 0i, length = 0L, names = NULL, real = 0, imag = 0) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_acf_initialize,
              flintNew("acf"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names,
              if (!missing(real)) as(real, "arf"),
              if (!missing(imag)) as(imag, "arf"))
    },
    className = "acf",
    package = "flint")

acf.array <- ACF.array <-
new("classGeneratorFunction",
    function (x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0)
        .Call(R_flint_acf_initialize,
              flintNew("acf"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL,
              if (!missing(real)) as(real, "arf"),
              if (!missing(imag)) as(imag, "arf")),
    className = "acf",
    package = "flint")

arb <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL, mid = 0, rad = 0) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_arb_initialize,
              flintNew("arb"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names,
              if (!missing(mid)) as(mid, "arf"),
              if (!missing(rad)) as(rad, "mag"))
    },
    className = "arb",
    package = "flint")

arb.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL, mid = 0, rad = 0)
        .Call(R_flint_arb_initialize,
              flintNew("arb"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL,
              if (!missing(mid)) as(mid, "arf"),
              if (!missing(rad)) as(rad, "mag")),
    className = "arb",
    package = "flint")

acb <-
new("classGeneratorFunction",
    function (x = 0i, length = 0L, names = NULL, real = 0, imag = 0) {
        simple <- nargs() == 1L && !missing(x)
        .Call(R_flint_acb_initialize,
              flintNew("acb"),
              if (!missing(x)) x,
              if (!missing(length)) length,
              if (simple) dim(x),
              if (simple) dimnames(x),
              if (simple) names(x) else names,
              if (!missing(real)) as(real, "arb"),
              if (!missing(imag)) as(imag, "arb"))
    },
    className = "acb",
    package = "flint")

acb.array <-
new("classGeneratorFunction",
    function (x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0)
        .Call(R_flint_acb_initialize,
              flintNew("acb"),
              if (!missing(x)) x,
              NULL,
              if (is.null(dim)) integer(0L) else dim,
              dimnames,
              NULL,
              if (!missing(real)) as(real, "arb"),
              if (!missing(imag)) as(imag, "arb")),
    className = "acb",
    package = "flint")
