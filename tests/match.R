library(flint)

n <- 10L
x. <- seq_len(n); y. <- rev(x.); z. <- integer(length(x.))
b. <- c(2L, 5L, 8L)

g1 <- expand.grid(rightmost.closed = c(FALSE, TRUE),
                  all.inside = c(FALSE, TRUE),
                  left.open = c(FALSE, TRUE))
g2 <- g1 |>
    transform(include.lowest = rightmost.closed,
              right = left.open) |>
    subset(!all.inside, c(include.lowest, right))

f1 <- lapply(.mapply(findInterval, g1, list(x = x., vec = b.)), ulong)
f2 <- f1[!g1$all.inside]

for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    x <- flint(.cl, x.); y <- flint(.cl, y.); z <- flint(.cl, z.)
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
              identical(unique(x), x),
              identical(unique(z), z[1L]),
              identical(unique(z, incomparables = 0L), z))
    switch(.cl,
           "acf" =, "arb" =, "acb" = NULL,
           {
               b <- flint(.cl, b.)
               stopifnot(all(mapply(identical, f1, .mapply(findInterval, g1, list(x = x,    vec = b)))),
                         all(mapply(identical, f2, .mapply(         cut, g2, list(x = x, breaks = b)))))
           })
}
