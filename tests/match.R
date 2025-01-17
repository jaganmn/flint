library(flint)

n <- 10L
x. <- seq_len(n); y. <- rev(x.); z. <- integer(length(x.))
for (.cl in c("slong", "ulong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    x <- new(.cl, x = x.); y <- new(.cl, x = y.); z <- new(.cl, x = z.)
    stopifnot(identical(mtfrm(x), format(x, base = 62L, digits = 0L)),
              identical(match(x, x, 0L), x.),
              identical(match(x, y, 0L), y.),
              identical(match(x, z, 0L), z.),
              identical(anyDuplicated(x), 0L),
              identical(anyDuplicated(x, fromLast = TRUE), 0L),
              identical(anyDuplicated(z), 2L),
              identical(anyDuplicated(z, fromLast = TRUE), n - 1L),
              identical(anyDuplicated(z, incomparables = 0L), 0L),
              identical(duplicated(x), logical(n)),
              identical(duplicated(x, fromLast = TRUE), logical(n)),
              identical(duplicated(z), !replace(logical(n), 1L, TRUE)),
              identical(duplicated(z, fromLast = TRUE), !replace(logical(n), n, TRUE)),
              identical(duplicated(z, incomparables = 0L), logical(n)),
              flintIdentical(unique(x), x),
              flintIdentical(unique(z), z[1L]),
              flintIdentical(unique(z, incomparables = 0L), z))
}
