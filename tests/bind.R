library(flint)
flint:::.initBasic()

## Hack:
identicalRecursive <-
function (x, y, ...) {
    stopifnot(any(typeof(x) == c("list", "expression")))
    if (typeof(x) != typeof(y) || length(x) != length(y) ||
        !identical(names(x), names(y)))
        return(FALSE)
    for (i in seq_along(x))
        if (!identical(x[[i]], y[[i]]))
            return(FALSE)
    TRUE
}

L <- list(c("NULL"),
          c("raw"),
          c("ulong"),
          c("logical", "integer", "slong", "fmpz"),
          c("fmpq"),
          c("double", "mag", "arf"),
          c("arb"),
          c("complex", "acf", "acb"),
          c("character"),
          c("name", "pairlist", "list"),
          c("expression"))
l <- unlist(L)
p <- rep(cumsum(lengths(L)), times = lengths(L))
u <- lapply(l, as, object = 1L)

for (i in seq_along(u)) {
    class. <- l[[p[[i]]]]
    identical. <-
    switch(class.,
           "list" =, "expression" = identicalRecursive,
           identical)
    a <- do.call(c.flint, u[seq_len(i)], quote = TRUE)
    b <-
    switch(class.,
           "character" =,
           "list" =, "expression" = unlist(lapply(u[seq_len(i)], as, class.), recursive = FALSE),
           rep(u[[p[[i]]]], times = i - 1L))
    stopifnot(identical.(a, b))
}
