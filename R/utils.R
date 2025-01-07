flintABI <-
function ()
    .Call(R_flint_abi)

flintBitsPerLimb <-
function ()
    .Call(R_flint_bits_per_limb)

flintClass <-
function (object)
    .Call(R_flint_class, object)

flintClassCommon <-
function (classes, strict = TRUE) {
    classes. <-
    c("NULL", "raw", "logical", "integer", "double", "complex",
      "ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
      "arb", "acb")
    m <- match(classes., classes, 0L) > 0L
    if (!strict && (w <- max(1L, which(m))) <= 6L)
        return(classes.[w])
    names(m) <- classes.
    if (m[["acb"]] || m[["arb"]]) {
        if (m[["acb"]] || m[["acf"]] || m[["complex"]])
            "acb"
        else "arb"
    }
    else if (m[["acf"]] || m[["complex"]])
        "acf"
    else if (m[["arf"]] || m[["mag"]] || m[["double"]]) {
        if (sum(m) != m[["mag"]]) # at least one is not 'mag'
            "arf"
        else "mag"
    }
    else if (m[["fmpq"]])
        "fmpq"
    else if (m[["fmpz"]] || (m[["ulong"]] && m[["slong"]]))
        "fmpz"
    else if (m[["slong"]] || m[["integer"]] || m[["logical"]])
        "slong"
    else "ulong"
}

flintIdentical <-
function (object, reference)
    .Call(R_flint_identical, object, reference)

flintLength <-
function (object, exact = TRUE)
    .Call(R_flint_length, object, as.logical(exact))

flintLengthAny <-
function (object, exact = TRUE) {
    if (is.na(flintClass(object))) {
        n <- length(object)
        if (exact)
            as(n, "ulong")
        else n
    }
    else flintLength(object, exact = exact)
}

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

flintSize <-
function (object)
    `class<-`(.Call(R_flint_size, object), "object_size")

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
