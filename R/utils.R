flintBits <- function () .Call(R_flint_bits)
flintVersion <- function () {
    n <- .Call(R_flint_version)
    v <- .mapply(function(n, p, b, class) {
                     r <- integer(p)
                     while (p > 0L) {
                         r[p] <- tmp <- n %% b
                         n <- (n - tmp) %/% b
                         p <- p - 1L
                     }
                     v <- list(r)
                     class(v) <- c(class, "numeric_version")
                     v
                 },
                 list(n = n, p = 3L, b = 256L,
                      class = list("package_version", NULL, NULL, NULL, NULL, NULL, NULL)),
                 NULL)
    names(v) <- names(n)
    v
}
flintClass <- function (object) .Call(R_flint_class, object)
flintNew <- function (class) .Call(R_flint_new, class)
flintValid <- function (object) .Call(R_flint_valid, object)
flintLength <- function (object) .Call(R_flint_length, object)
flintTriple <- function (object) .Call(R_flint_triple, object)
