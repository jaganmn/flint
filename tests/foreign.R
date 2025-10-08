library(flint)

if (requireNamespace("Rmpfr")) withAutoprint({
    .initForeign("Rmpfr")

    ## arf->mpfr->arf will be perfect
    (w <- c(-Inf, Inf, NaN, 0:3))
    (x <- arf(w))
    (y <- as(x, "mpfr"))
    (z <- as(y, "arf"))
    stopifnot(identical(z, x))

    ## mpfr->arf->mpfr will lose unused precision
    (w <- 0.75)
    (x <- Rmpfr::mpfr(w, 2^(1:6)))
    (y <- as(x, "arf"))
    (z <- as(y, "mpfr"))
    stopifnot(identical(z, Rmpfr::mpfr(0.75, rep(2, length(x)))))

    same <-
    function (F, ...)
        length(unique(lapply(list(...), F))) == 1L

    ## keeping 'dim' and 'dimnames'
    (w <- array(0, c(1L, 1L, 1L), list(A = "a", B = "b", C = "c")))
    (x <- Rmpfr::mpfr(w, 2L))
    (y <- as(x, "arf"))
    (z <- as(y, "mpfr"))
    stopifnot(same(dim, w, x, y, z), same(dimnames, w, x, y, z))

    ## keeping 'names'
    (w <- c(. = 0))
    (x <- Rmpfr::mpfr(w, 2L))
    if (is.null(names(x))) {
        cat("Rmpfr::mpfr lost names\n")
        names(x) <- names(w)
    }
    (y <- as(x, "arf"))
    (z <- as(y, "mpfr"))
    stopifnot(same(names, w, x, y, z))
})

if (requireNamespace("gmp")) withAutoprint({
    .initForeign("gmp")

    ## more simple round trips
    (w <- "-121122111222111122221111122222111111222222")
    (x <- gmp::as.bigz(w))
    (y <- as(x, "fmpz"))
    (z <- as(y, "bigz"))
    stopifnot(identical(z, x), identical(y, fmpz(w)))
    (x <- gmp::as.bigq(w, w))
    (y <- as(x, "fmpq"))
    (z <- as(y, "bigq"))
    stopifnot(identical(z, x), identical(y, fmpq(num = w, den = w)))

    ## 'bigz' modulus is unused
    (x <- gmp::as.bigz(w, mod = 11L))
    (y <- as(x, "fmpz")) # with warning
    stopifnot(identical(y, fmpz(w) %% 11L))

    ## 'bigz' missing value is not representable
    asTRUE <- function (x) TRUE
    (x <- gmp::as.bigz(NA_integer_))
    stopifnot(tryCatch({ as(x, "fmpz"); FALSE }, error = asTRUE))
    (x <- gmp::as.bigq(NA_integer_))
    stopifnot(tryCatch({ as(x, "fmpq"); FALSE }, error = asTRUE))

    ## 'bigz' and 'bigq' do not support 'dim' of length not equal to 2
    ## nor 'dim' of the form c(0, n) [n > 0] nor 'dimnames' nor 'names'
    b <- gmp::as.bigz(integer(0L))
    (x <- fmpz.array(0L, c(0L, 0L, 0L)))
    (y <- as(x, "bigz")) # with warning
    stopifnot(identical(y, b))
    (x <- fmpz.array(0L, c(0L, 1L)))
    (y <- as(x, "bigz")) # with warning
    stopifnot(identical(y, b))
    (x <- fmpz.array(0L, c(1L, 0L), list(A = "a", B = NULL)))
    (y <- as(x, "bigz")) # with warning
    stopifnot(identical(y, `dim<-`(b, c(1L, 0L))))
    (x <- fmpz(0L, 1L, "."))
    (y <- as(x, "bigz")) # with warning
    stopifnot(identical(y, c(b, 0L)))
})
