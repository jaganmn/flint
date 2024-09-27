library(methods) # initialize, new
loadNamespace("flint") # work with or without attaching


## Test that length(new(., length = value)) ==        value
##           length(new(.,      x = value)) == length(value).

cl <- c("slong", "ulong", "fmpz", "fmpq", "arf", "mag", "arb", "acb")
n <- length(cl):1L
a <- .mapply(new, list(cl, length = n), NULL)
b <- .mapply(new, list(cl, x = lapply(n, raw)), NULL)
stopifnot(identical(lapply(a, length), as.list(n)),
          identical(lapply(b, length), as.list(n)))


## Test implicit default values of initializers.

new.fmpq <-
function(Class, num = 0L, den = 1L, ...)
    new(Class, num = num, den = den)
new.arb <-
function(Class, mid = 0, rad = 0, ...)
    new(Class, mid = mid, rad = rad)
new.acb <-
function(Class, real.mid = 0, real.rad = 0, imag.mid = 0, imag.rad = 0, ...)
    new(Class, real.mid = real.mid, real.rad = real.rad, imag.mid = imag.mid, imag.rad = imag.rad)

e <- expression(as(new(, 1), ), )
for (s in list(c("fmpq",      "num",      "den"),
               c( "arb",      "mid",      "rad"),
               c( "acb", "real.mid", "real.rad", "imag.mid", "imag.rad"))) {
    e[[c(1L, 3L)]] <- paste0("n", e[[c(1L, 2L, 2L)]] <- s[1L])
    e[[2L]] <- e[[1L]]
    e[[c(2L, 2L, 1L)]] <- as.name(paste0("new.", s[1L]))
    for (nm in s[-1L]) {
        for (i in 1:2)
            names(e[[c(i, 2L)]])[3L] <- nm
        v <- lapply(e, eval)
        stopifnot(identical(v[[1L]], v[[2L]]))
    }
}


## Test recycling of initializers.

. <- integer(0L); a <- 1:2; b <- 3:4; aa <- c(a, a); bb <- c(b, b)
e <- expression(as(new(,  a, bb), ),
                as(new(, aa, bb), ),
                as(new(,  ., bb), ),
                as(new(,  .,  .), ))
for (s in list(c("fmpq",      "num",      "den"),
               c( "arb",      "mid",      "rad"),
               c( "acb", "real.mid", "real.rad"),
               c( "acb", "imag.mid", "imag.rad"))) {
    for (p in list(1:2, 2:1)) {
        for (i in 1:4) {
            e[[c(i, 3L)]] <- paste0("n", e[[c(i, 2L, 2L)]] <- s[1L])
            names(e[[c(i, 2L)]])[3:4] <- s[-1L][p]
        }
        v <- lapply(e, eval)
        stopifnot(identical(v[[1L]], v[[2L]]),
                  identical(v[[3L]], v[[4L]]))
    }
}


## Test single initialization of 'fmpq'.

x <- c(-10, -0.125, 0, 1.25, 2.5)
a <- as(new("fmpq", x = x), "nfmpq")
b <- new("nfmpq",
         num = new("nfmpz", c(-10L, -1L, 0L, 5L, 5L)),
         den = new("nfmpz", c(  1L,  8L, 1L, 4L, 2L)))
stopifnot(identical(a, b))


## Test single initialization of 'arb'.

x <- atan(-2:2)
a <- as(new("arb", x = x), "narb")
b <- new("narb",
         mid = new("narf", x),
         rad = new("nmag", double(length(x))))
stopifnot(identical(a, b))


## Test single initialization of 'acb'.

x <- complex(real = exp(-2:2), imaginary = sin(-2:2))
a <- as(new("acb", x = x), "nacb")
b <- new("nacb",
         real = new("narb",
                    mid = new("narf", Re(x)),
                    rad = new("nmag", double(length(x)))),
         imag = new("narb",
                    mid = new("narf", Im(x)),
                    rad = new("nmag", double(length(x)))))
stopifnot(identical(a, b))


## Test handling of unrepresentable values.

allError <- function(call, l) {
    call <- substitute(call)
    for (value in l) {
        call. <- do.call(substitute, list(call, list(. = value)))
        tools::assertError(eval(call.))
    }
    invisible(NULL)
}

o <- list()
allError(new("slong", x = .),
         list(NA_integer_, 1i, o))
allError(new("ulong", x = .),
         list(NA_integer_, -1L, 1i, o))
allError(new( "fmpz", x = .),
         list(NA_integer_, NaN, -Inf, Inf, 1i, o))
allError(new( "fmpq", x = .),
         list(NA_integer_, NaN, -Inf, Inf, 1i, o))
allError(new( "fmpq", num = .),
         list(NA_integer_, NaN, -Inf, Inf, 1i, o))
allError(new( "fmpq", den = .),
         list(NA_integer_, 0L, NaN, -Inf, Inf, 1i, o))
allError(new(  "arf", x = .),
         list(1i, o))
allError(new(  "mag", x = .),
         list(NaN, 1i, o))
allError(new(  "arb", x = .),
         list(1i, o))
allError(new(  "arb", mid = .),
         list(1i, o))
allError(new(  "arb", rad = .),
         list(NaN, 1i, o))
allError(new(  "acb", x = .),
         list(o))
allError(new(  "acb", real.mid = .),
         list(1i, o))
allError(new(  "acb", real.rad = .),
         list(NaN, 1i, o))
allError(new(  "acb", imag.mid = .),
         list(1i, o))
allError(new(  "acb", imag.rad = .),
         list(NaN, 1i, o))
