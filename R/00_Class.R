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
        new("ulong",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names)
    },
    className = "ulong",
    package = "flint")

ulong.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        new("ulong",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL),
    className = "ulong",
    package = "flint")

slong <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        new("slong",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names)
    },
    className = "slong",
    package = "flint")

slong.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        new("slong",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL),
    className = "slong",
    package = "flint")

fmpz <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        new("fmpz",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names)
    },
    className = "fmpz",
    package = "flint")

fmpz.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL)
        new("fmpz",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL),
    className = "fmpz",
    package = "flint")

fmpq <-
new("classGeneratorFunction",
    function (x = 0L, length = 0L, names = NULL, num = 0L, den = 1L) {
        simple <- nargs() == 1L && !missing(x)
        new("fmpq",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names,
            num = if (!missing(num)) fmpz(num),
            den = if (!missing(den)) fmpz(den))
    },
    className = "fmpq",
    package = "flint")

fmpq.array <-
new("classGeneratorFunction",
    function (x = 0L, dim = length(x), dimnames = NULL, num = 0L, den = 1L)
        new("fmpq",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL,
            num = if (!missing(num)) fmpz(num),
            den = if (!missing(den)) fmpz(den)),
    className = "fmpq",
    package = "flint")

mag <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        new("mag",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names)
    },
    className = "mag",
    package = "flint")

mag.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL)
        new("mag",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL),
    className = "mag",
    package = "flint")

arf <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL) {
        simple <- nargs() == 1L && !missing(x)
        new("arf",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names)
    },
    className = "arf",
    package = "flint")

arf.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL)
        new("arf",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL),
    className = "arf",
    package = "flint")

acf <- ACF <-
new("classGeneratorFunction",
    function (x = 0i, length = 0L, names = NULL, real = 0, imag = 0) {
        simple <- nargs() == 1L && !missing(x)
        new("acf",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names,
            real = if (!missing(real)) arf(real),
            imag = if (!missing(imag)) arf(imag))
    },
    className = "acf",
    package = "flint")

acf.array <- ACF.array <-
new("classGeneratorFunction",
    function (x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0)
        new("acf",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL,
            real = if (!missing(real)) arf(real),
            imag = if (!missing(imag)) arf(imag)),
    className = "acf",
    package = "flint")

arb <-
new("classGeneratorFunction",
    function (x = 0, length = 0L, names = NULL, mid = 0, rad = 0) {
        simple <- nargs() == 1L && !missing(x)
        new("arb",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names,
            mid = if (!missing(mid)) arf(mid),
            rad = if (!missing(rad)) mag(rad))
    },
    className = "arb",
    package = "flint")

arb.array <-
new("classGeneratorFunction",
    function (x = 0, dim = length(x), dimnames = NULL, mid = 0, rad = 0)
        new("arb",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL,
            mid = if (!missing(mid)) arf(mid),
            rad = if (!missing(rad)) mag(rad)),
    className = "arb",
    package = "flint")

acb <-
new("classGeneratorFunction",
    function (x = 0i, length = 0L, names = NULL, real = 0, imag = 0) {
        simple <- nargs() == 1L && !missing(x)
        new("acb",
            x = if (!missing(x)) x,
            length = if (!missing(length)) length,
            dim = if (simple) dim(x),
            dimnames = if (simple) dimnames(x),
            names = if (simple) names(x) else names,
            real = if (!missing(real)) arb(real),
            imag = if (!missing(imag)) arb(imag))
    },
    className = "acb",
    package = "flint")

acb.array <-
new("classGeneratorFunction",
    function (x = 0i, dim = length(x), dimnames = NULL, real = 0, imag = 0)
        new("acb",
            x = if (!missing(x)) x,
            length = NULL,
            dim = if (is.null(dim)) integer(0L) else dim,
            dimnames = dimnames,
            names = NULL,
            real = if (!missing(real)) arb(real),
            imag = if (!missing(imag)) arb(imag)),
    className = "acb",
    package = "flint")
