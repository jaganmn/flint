library(flint)


## Test that length(flint(., length = value)) ==        value
##           length(flint(.,      x = value)) == length(value).

cl <- c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
        "arb", "acb")
n <- length(cl):1L
a <- .mapply(flint, list(cl, length =        n      ), NULL)
b <- .mapply(flint, list(cl,      x = lapply(n, raw)), NULL)
stopifnot(identical(lapply(a, length), as.list(n)),
          identical(lapply(b, length), as.list(n)))


## Test implicit default values of initializers.

for (call. in expression(fmpq( num = 0L,  den = 1L),
                          arb( mid = 0 ,  rad = 0 ),
                          ACF(real = 0 , imag = 0 ),
                          acb(real = 0 , imag = 0 ))) {
    v <- eval(call.)
    stopifnot(identical(eval(call.[-2L]), v),
              identical(eval(call.[-3L]), v))
}


## Test recycling of initializers.

. <- integer(0L); a <- 1:2; b <- 3:4; aa <- c(a, a); bb <- c(b, b)
L <- list(list(a, bb), list(aa, bb), list(., bb), list(., .))
for (call. in expression(fmpq( num = ,  den = ),
                          arb( mid = ,  rad = ),
                          ACF(real = , imag = ),
                          acb(real = , imag = ))) {
    v <- lapply(L, function (a) eval(`[<-`(call., 2L:3L, a)))
    stopifnot(identical(v[[1L]], v[[2L]]),
              identical(v[[3L]], v[[4L]]))
}


## Test single initialization of 'fmpq'.

x <- c(-10, -0.125, 0, 1.25, 2.5)
a <- fmpq(x)
b <- fmpq(num = c(-10L, -1L, 0L, 5L, 5L),
          den = c(  1L,  8L, 1L, 4L, 2L))
stopifnot(identical(a, b))


## Test single initialization of 'arb'.

x <- atan(-2:2)
a <- arb(x)
b <- arb(mid = x, rad = 0)
stopifnot(identical(a, b))


## Test single initialization of 'acf', 'acb'.

x <- complex(real = exp(-2:2), imaginary = sin(-2:2))
a <- ACF(x)
b <- ACF(real = Re(x), imag = Im(x))
stopifnot(identical(a, b))
a <- acb(x)
b <- acb(real = Re(x), imag = Im(x))
stopifnot(identical(a, b))


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
stopifnot(testError(ulong(.),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         -1, a)),
          testError(slong(.),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         -a/2 - b*2, a/2)),
          testError(fmpz(.),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(fmpq(.),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(fmpq(num = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf)),
          testError(fmpq(den = .),
                    list(NA, NA_integer_, NA_real_, NaN, -Inf, Inf,
                         0)),
          testError(mag(.),
                    list(NA, NA_integer_, NA_real_, NaN)),
          testError(arb(rad = .),
                    list(NA, NA_integer_, NA_real_, NaN)))
