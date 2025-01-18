library(flint)

n <- 6L
x.. <- list(c(       0,   0,    0,   0, 0, 1),
            c(       0,   0,    0, Inf, 0, 1),
            c(NA_real_, NaN, -Inf, Inf, 0, 1))
x..[[4L]] <- complex(imaginary = x..[[3L]])
y.. <- lapply(x.., `names<-`, letters[seq_len(n)])

k.. <- rep(seq_along(x..), c(4L, 1L, 2L, 2L))
names(k..) <- c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "arb",
                "acf", "acb")

for (.cl in names(k..)) {
    k. <- k..[[.cl]]
    x. <- x..[[k.]]
    y. <- y..[[k.]]
    x <- new(.cl, x = x.)
    y <- new(.cl, x = y.)
    stopifnot(identical(anyNA(x), anyNA(x.)),
              identical(anyNA(y), anyNA(y.)),
              identical(is.na(x), is.na(x.)),
              identical(is.na(y), is.na(y.)),
              identical(is.nan(x), is.na(x.)),
              identical(is.nan(y), is.na(y.)),
              identical(is.infinite(x), is.infinite(x.)),
              identical(is.infinite(y), is.infinite(y.)),
              identical(is.finite(x), is.finite(x.)),
              identical(is.finite(y), is.finite(y.)))
    switch(k.,
           stopifnot(tryCatch(is.na(x) <- n, error = function (e) TRUE)),
           stopifnot(flintIdentical(`is.na<-`(x, n),
                                    `is.na<-`(as(x, "arf"), n)),
                     flintIdentical(`is.na<-`(y, n),
                                    `is.na<-`(as(y, "arf"), n))),
           stopifnot(flintIdentical(`is.na<-`(x, n),
                                    new(.cl, x = `is.na<-`(x., n))),
                     flintIdentical(`is.na<-`(y, n),
                                    new(.cl, x = `is.na<-`(y., n)))))
}
