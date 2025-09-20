setMethod("!",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_slong_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_slong_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "slong"),
          function (z)
              .Call(R_flint_slong_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "slong"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_slong_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "slong"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "logical" =, "integer" =
                         g(slong(e1), e2),
                     "raw" =
                         g(fmpz(e1), fmpz(e2)),
                     "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "slong"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "slong", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "logical" =, "integer" =
                         g(e1, slong(e2)),
                     "raw" =
                         g(fmpz(e1), fmpz(e2)),
                     "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "slong", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "slong", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(fmpz(e1), fmpz(e2)))

setMethod("Ops",
          c(e1 = "slong", e2 = "slong"),
          function (e1, e2)
              .Call(R_flint_slong_ops2, .Generic, e1, e2, list()))

setMethod("Ops",
          c(e1 = "slong", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(fmpz(e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(fmpq(e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "slong", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acf(e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(arb(e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Summary",
          c(x = "slong"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_slong_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "slong"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "slong"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_slong_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(fmpz(r), fmpz(x), , upper.tri, transpose),
                     "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "slong"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "slong", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(backsolve(fmpz(r), , , upper.tri, transpose))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(fmpz(r), fmpz(x), , upper.tri, transpose),
                     "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "slong", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "slong", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(fmpz(r), fmpz(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(fmpz(r), fmpz(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(fmpz(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(fmpq(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arb(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "slong", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "slong"),
          function (x, ...)
              chol(arf(x), ...))

setMethod("chol2inv",
          c(x = "slong"),
          function (x, ...)
              tcrossprod(backsolve(x)))

setAs("ANY", "slong",
      function (from)
          .Call(R_flint_slong_initialize, flintNew("slong"), from, NULL,
                dim(from), dimnames(from), names(from)))

setMethod("colMeans",
          c(x = "slong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_slong_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "slong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_slong_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("det",
          c(x = "slong"),
          function (x, ...)
              det(fmpz(x), ...))

setMethod("determinant",
          c(x = "slong"),
          function (x, logarithm = TRUE, ...)
              determinant(fmpz(x), logarithm = logarithm, ...))

setMethod("format",
          c(x = "slong"),
          function (x, base = 10L, ...)
              .Call(R_flint_slong_format, x, base))

setMethod("is.finite",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "slong"),
          function (x)
              .Call(R_flint_slong_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "slong"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_slong_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMatrixOpsMethod(
          c(x = "ANY", y = "ulong"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(fmpz(x), fmpz(y)),
                     "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "slong"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "ulong", y = "ANY"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(g(fmpz(x)))
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(fmpz(x), fmpz(y)),
                     "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "slong", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "slong", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(fmpz(x), fmpz(y)))

setMatrixOpsMethod(
          c(x = "slong", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(fmpz(x), fmpz(y)))

setMatrixOpsMethod(
          c(x = "slong", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(fmpz(x), y))

setMatrixOpsMethod(
          c(x = "slong", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(fmpq(x), y))

setMatrixOpsMethod(
          c(x = "slong", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "slong", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), y))

setMatrixOpsMethod(
          c(x = "slong", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(acf(x), y))

setMatrixOpsMethod(
          c(x = "slong", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(arb(x), y))

setMatrixOpsMethod(
          c(x = "slong", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "slong"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_slong_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "slong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_slong_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "slong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_slong_ops1, "rowSums", x, list(NULL, as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "slong"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(fmpz(a), fmpz(b), ...),
                     "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "slong"),
                          domain = NA)))

setMethod("solve",
          c(a = "slong", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(solve(fmpz(a), ...))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(fmpz(a), fmpz(b), ...),
                     "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "slong", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "slong", b = "ulong"),
          function (a, b, ...)
              solve(fmpz(a), fmpz(b), ...))

setMethod("solve",
          c(a = "slong", b = "slong"),
          function (a, b, ...)
              solve(fmpz(a), fmpz(b), ...))

setMethod("solve",
          c(a = "slong", b = "fmpz"),
          function (a, b, ...)
              solve(fmpz(a), b, ...))

setMethod("solve",
          c(a = "slong", b = "fmpq"),
          function (a, b, ...)
              solve(fmpq(a), b, ...))

setMethod("solve",
          c(a = "slong", b = "mag"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "slong", b = "arf"),
          function (a, b, ...)
              solve(arf(a), b, ...))

setMethod("solve",
          c(a = "slong", b = "acf"),
          function (a, b, ...)
              solve(acf(a), b, ...))

setMethod("solve",
          c(a = "slong", b = "arb"),
          function (a, b, ...)
              solve(arb(a), b, ...))

setMethod("solve",
          c(a = "slong", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))
