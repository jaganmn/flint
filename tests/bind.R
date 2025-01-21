library(flint)
flint:::.initBasic()

## Hack:
flintIdenticalRecursive <-
function (object, reference) {
    stopifnot(any(typeof(object) == c("list", "expression")))
    if (typeof(object) != typeof(reference) ||
        length(object) != length(reference) ||
        !identical(names(object), names(reference)))
        return(FALSE)
    for (i in seq_along(object))
        if (!(if (is(object[[i]], "flint") && is(reference[[i]], "flint"))
                  flintIdentical
              else identical)(object[[i]], reference[[i]]))
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
           "NULL" =, "raw" =, "character" = identical,
           "list" =, "expression" = flintIdenticalRecursive,
           flintIdentical)
    a <- do.call(c.flint, u[seq_len(i)], quote = TRUE)
    b <-
    switch(class.,
           "character" =, "list" =, "expression" = unlist(lapply(u[seq_len(i)], as, class.), recursive = FALSE),
           rep(u[[p[[i]]]], times = i - 1L))
    stopifnot(identical.(a, b))
}
