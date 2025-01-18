library(flint)


## Test that length(new(., length = value)) ==        value
##           length(new(.,      x = value)) == length(value).

cl <- c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
        "arb", "acb")
n <- length(cl):1L
a <- .mapply(new, list(cl, length = n), NULL)
b <- .mapply(new, list(cl, x = lapply(n, raw)), NULL)
stopifnot(identical(lapply(a, length), as.list(n)),
          identical(lapply(b, length), as.list(n)))


## Test implicit default values of initializers.

new.fmpq <-
function (Class, num = 0L, den = 1L, ...)
    new(Class, num = num, den = den)
new.arb <-
function (Class, mid = 0, rad = 0, ...)
    new(Class, mid = mid, rad = rad)
new.acf <-
new.acb <-
function (Class, real = 0, imag = 0, ...)
    new(Class, real = real, imag = imag)

e <- expression(new(, 1), )
for (s in list(c("fmpq",  "num",  "den"),
               c( "arb",  "mid",  "rad"),
               c( "acf", "real", "imag"),
               c( "acb", "real", "imag"))) {
    e[[c(1L, 2L)]] <- s[1L]
    e[[2L]] <- e[[1L]]
    e[[c(2L, 1L)]] <- as.name(paste0("new.", s[1L]))
    for (nm in s[-1L]) {
        for (i in 1:2)
            names(e[[i]])[3L] <- nm
        print(e)
        v <- lapply(e, eval)
        stopifnot(flintIdentical(v[[1L]], v[[2L]]))
    }
}


## Test recycling of initializers.

. <- integer(0L); a <- 1:2; b <- 3:4; aa <- c(a, a); bb <- c(b, b)
e <- expression(new(,  a, bb),
                new(, aa, bb),
                new(,  ., bb),
                new(,  .,  .))
for (s in list(c("fmpq",  "num",  "den"),
               c( "arb",  "mid",  "rad"),
               c( "acf", "real", "imag"),
               c( "acb", "real", "imag"))) {
    for (p in list(1:2, 2:1)) {
        for (i in 1:4) {
            e[[c(i, 2L)]] <- s[1L]
            names(e[[i]])[3:4] <- s[-1L][p]
        }
        print(e)
        v <- lapply(e, eval)
        stopifnot(flintIdentical(v[[1L]], v[[2L]]),
                  flintIdentical(v[[3L]], v[[4L]]))
    }
}


## Test single initialization of 'fmpq'.

x <- c(-10, -0.125, 0, 1.25, 2.5)
a <- new("fmpq", x = x)
b <- new("fmpq",
         num = c(-10L, -1L, 0L, 5L, 5L),
         den = c(  1L,  8L, 1L, 4L, 2L))
stopifnot(flintIdentical(a, b))


## Test single initialization of 'arb'.

x <- atan(-2:2)
a <- new("arb", x = x)
b <- new("arb", mid = x, rad = 0)
stopifnot(flintIdentical(a, b))


## Test single initialization of 'acf', 'acb'.

x <- complex(real = exp(-2:2), imaginary = sin(-2:2))
a <- new("acf", x = x)
b <- new("acf", real = Re(x), imag = Im(x))
stopifnot(flintIdentical(a, b))
a <- new("acb", x = x)
b <- new("acb", real = Re(x), imag = Im(x))
stopifnot(flintIdentical(a, b))


## Test handling of unrepresentable values.

testError <-
function (call, l) {
    call <- substitute(call)
    fn <-
    function (value) {
        call. <- do.call(substitute, list(call, list(. = value)))
        tryCatch({ eval(call.); FALSE }, error = function (e) TRUE)
    }
    vapply(l, fn, FALSE)
}

wl <- flintABI()
wd <- .Machine[["double.digits"]]
a <- 2^wl
b <- 2^max(0L, wl - wd)
stopifnot(testError(new("ulong", x = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         -1, a)),
          testError(new("slong", x = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         -a/2 - b*2, a/2)),
          testError(new("fmpz", x = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(new("fmpq", x = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(new("fmpq", num = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(new("fmpq", den = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         0)),
          testError(new("mag", x = .),
                    list(NA, NA_integer_, NA_real_, NaN)),
          testError(new("arb", rad = .),
                    list(NA, NA_integer_, NA_real_, NaN)))
