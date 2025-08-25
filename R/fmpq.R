setMethod("!",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "fmpq"),
          function (z)
              .Call(R_flint_fmpq_ops1, .Generic, z, NULL))

setMethod("Den",
          c(q = "fmpq"),
          function (q)
              .Call(R_flint_fmpq_part, q, 1L))

setMethod("Den<-",
          c(q = "fmpq"),
          function (q, value) {
              nq <- length(q)
              nv <- length(value)
              if (nv != 1L && nv != nq)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "q", nq),
                       domain = NA)
              ans <- .fmpq(num = Num(q), den = value)
              ans@dim <- q@dim
              ans@dimnames <- q@dimnames
              ans@names <- q@names
              ans
          })

setMethod("Math",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpq"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_fmpq_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Num",
          c(q = "fmpq"),
          function (q)
              .Call(R_flint_fmpq_part, q, 0L))

setMethod("Num<-",
          c(q = "fmpq"),
          function (q, value) {
              nq <- length(q)
              nv <- length(value)
              if (nv != 1L && nv != nq)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "q", nq),
                       domain = NA)
              ans <- .fmpq(num = value, den = Den(q))
              ans@dim <- q@dim
              ans@dimnames <- q@dimnames
              ans@names <- q@names
              ans
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpq"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpq(e1), e2),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "fmpq"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(e1, .fmpq(e2)),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpq", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpq"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops2, .Generic, e1, e2, list()))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), .arf(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(e1), e2))

setMethod("Summary",
          c(x = "fmpq"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpq_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "fmpq"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "fmpq"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_fmpq_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(.fmpq(r), x, k, upper.tri, transpose),
                     "double" =
                         backsolve(.arf(r), .arf(x), k, upper.tri, transpose),
                     "complex" =
                         backsolve(.acf(r), .acf(x), k, upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "fmpq"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "fmpq", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_fmpq_ops1, "backsolve", r, list(as.integer(k), as.logical(upper.tri), as.logical(transpose))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(r, .fmpq(x), k, upper.tri, transpose),
                     "double" =
                         backsolve(.arf(r), .arf(x), k, upper.tri, transpose),
                     "complex" =
                         backsolve(.acf(r), .acf(x), k, upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "fmpq", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "fmpq", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, .fmpq(x), k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, .fmpq(x), k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, .fmpq(x), k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_fmpq_ops2, "backsolve", r, x, list(as.integer(k), as.logical(upper.tri), as.logical(transpose))))

setMethod("backsolve",
          c(r = "fmpq", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(.arf(r), .arf(x), k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(.arf(r), x, k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(.acf(r), x, k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(.arb(r), x, k, upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpq", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(.acb(r), x, k, upper.tri, transpose))

setMethod("chol2inv",
          c(x = "fmpq"),
          function (x, size = ncol(x), ...)
              .Call(R_flint_fmpq_ops1, "chol2inv", x, list(as.integer(size))))

setAs("ANY", "fmpq",
      function (from)
          new("fmpq", x = from, length = NULL,
              dim = dim(from), dimnames = dimnames(from),
              names = names(from), num = NULL, den = NULL))

setMethod("colMeans",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...) {
              p <- format(Num(x), base = base, ...)
              q <- format(Den(x), base = base, ...)
              p[] <- paste0(p, "/", q)
              p
          })

setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, x, length, dim, dimnames, names,
                    num, den, ...)
              .Call(R_flint_fmpq_initialize,
                    .Object, x, length, dim, dimnames, names,
                    num, den))

setMethod("is.finite",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_fmpq_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMatrixOpsMethod(
          c(x = "ANY", y = "fmpq"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpq(x), y),
                     "double" =
                         g(.arf(x), .arf(y)),
                     "complex" =
                         g(.acf(x), .acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "fmpq"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "fmpq", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_fmpq_ops2, .Generic, x, x, list()))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(x, .fmpq(y)),
                     "double" =
                         g(.arf(x), .arf(y)),
                     "complex" =
                         g(.acf(x), .acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "fmpq", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "fmpq", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, .fmpq(y)))

setMatrixOpsMethod(
          c(x = "fmpq", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, .fmpq(y)))

setMatrixOpsMethod(
          c(x = "fmpq", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(x, .fmpq(y)))

setMatrixOpsMethod(
          c(x = "fmpq", y = "fmpq"),
          function (x, y)
              .Call(R_flint_fmpq_ops2, .Generic, x, y, list()))

setMatrixOpsMethod(
          c(x = "fmpq", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(.arf(x), .arf(y)))

setMatrixOpsMethod(
          c(x = "fmpq", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(.arf(x), y))

setMatrixOpsMethod(
          c(x = "fmpq", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(.acf(x), y))

setMatrixOpsMethod(
          c(x = "fmpq", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(.arb(x), y))

setMatrixOpsMethod(
          c(x = "fmpq", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(.acb(x), y))

setMethod("mean",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpq_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "rowSums", x, list(NULL, as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "fmpq"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(.fmpq(a), b, ...),
                     "double" =
                         solve(.arf(a), .arf(b), ...),
                     "complex" =
                         solve(.acf(a), .acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "fmpq"),
                          domain = NA)))

setMethod("solve",
          c(a = "fmpq", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_fmpq_ops1, "solve", a, list()))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(a, .fmpq(b), ...),
                     "double" =
                         solve(.arf(a), .arf(b), ...),
                     "complex" =
                         solve(.acf(a), .acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "fmpq", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "fmpq", b = "ulong"),
          function (a, b, ...)
              solve(a, .fmpq(b), ...))

setMethod("solve",
          c(a = "fmpq", b = "slong"),
          function (a, b, ...)
              solve(a, .fmpq(b), ...))

setMethod("solve",
          c(a = "fmpq", b = "fmpz"),
          function (a, b, ...)
              solve(a, .fmpq(b), ...))

setMethod("solve",
          c(a = "fmpq", b = "fmpq"),
          function (a, b, ...)
              .Call(R_flint_fmpq_ops2, "solve", a, b, list()))

setMethod("solve",
          c(a = "fmpq", b = "mag"),
          function (a, b, ...)
              solve(.arf(a), .arf(b), ...))

setMethod("solve",
          c(a = "fmpq", b = "arf"),
          function (a, b, ...)
              solve(.arf(a), b, ...))

setMethod("solve",
          c(a = "fmpq", b = "acf"),
          function (a, b, ...)
              solve(.acf(a), b, ...))

setMethod("solve",
          c(a = "fmpq", b = "arb"),
          function (a, b, ...)
              solve(.arb(a), b, ...))

setMethod("solve",
          c(a = "fmpq", b = "acb"),
          function (a, b, ...)
              solve(.acb(a), b, ...))
