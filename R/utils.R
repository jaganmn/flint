flintBits <-
function ()
    .Call(R_flint_bits)

flintClass <-
function (object)
    .Call(R_flint_class, object)

flintIdentical <-
function (object, reference)
    .Call(R_flint_identical, object, reference)

flintLength <-
function (object)
    .Call(R_flint_length, object)

flintNew <-
function (class)
    .Call(R_flint_new, class)

flintPrec <-
function (prec = NULL) {
    default <- .Machine[["double.digits"]]
    if (is.null(prec))
        getOption("flint.prec", default)
    else if (is.null(oop <- options(flint.prec = prec)[["flint.prec"]]))
        default
    else oop
}

flintRnd <-
function (rnd = NULL) {
    default <- "N"
    if (is.null(rnd))
        getOption("flint.rnd", default)
    else if (is.null(oop <- options(flint.rnd = rnd)[["flint.rnd"]]))
        default
    else oop
}

flintTriple <-
function (object)
    .Call(R_flint_triple, object)

flintValid <-
function (object)
    .Call(R_flint_valid, object)

flintVersion <-
function () {
    n <- .Call(R_flint_version)
    v <- .mapply(function (n, p, b, class) {
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
