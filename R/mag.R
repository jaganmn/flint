setMethod("!",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "mag"),
          function (z)
              .Call(R_flint_mag_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "mag"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_mag_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "mag"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "mag"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "mag", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "mag", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "mag", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "mag"),
          function (e1, e2)
              .Call(R_flint_mag_ops2, .Generic, e1, e2, NULL))

setMethod("Ops",
          c(e1 = "mag", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acf(e1), e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(arb(e1), e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Summary",
          c(x = "mag"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_mag_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "mag"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits.mag = 8L, rnd.mag = "A"), mode),
                     as.vector(.Call(R_flint_mag_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "mag"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "mag", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(backsolve(arf(r), , , upper.tri, transpose))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "mag", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "mag", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arb(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "mag", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "mag"),
          function (x, ...)
              chol(arf(x), ...))

setMethod("chol2inv",
          c(x = "mag"),
          function (x, ...)
              chol2inv(arf(x), ...))

setAs("ANY", "mag",
      function (from)
          .Call(R_flint_mag_initialize, flintNew("mag"), from, NULL,
                dim(from), dimnames(from), names(from), NULL))

setMethod("colMeans",
          c(x = "mag"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_mag_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "mag"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_mag_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("det",
          c(x = "mag"),
          function (x, ...)
              determinant(arf(x), ...))

setMethod("determinant",
          c(x = "mag"),
          function (x, logarithm = TRUE, ...)
              determinant(arf(x), logarithm = logarithm, ...))

setMethod("diff",
          c(x = "mag"),
          function (x, ...)
              diff(arf(x), ...))

setMethod("diffinv",
          c(x = "mag"),
          function (x, ...)
              diffinv(arf(x), ...))

setMethod("format",
          c(x = "mag"),
          function (x, base = 10L, sep = NULL,
                    digits.mag = NULL, rnd.mag = NULL, ...) {
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "@"
              if (is.null(digits.mag))
                  digits.mag <- getOption("digits.mag", 4L)
              .Call(R_flint_mag_format, x, base, sep, digits.mag, rnd.mag)
          })

setMethod("is.finite",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "mag"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_mag_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMethod("isComplex",
          c(x = "mag"),
          function (x) FALSE)

setMethod("isFloating",
          c(x = "mag"),
          function (x) TRUE)

setMethod("isSigned",
          c(x = "mag"),
          function (x) FALSE)

setMethod("log",
          c(x = "mag"),
          function (x, base, ...)
              .Call(R_flint_mag_ops1, "log", x,
                    if (!missing(base)) list(as(base, "arf"))))

setMatrixOpsMethod(
          c(x = "ANY", y = "mag"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "mag"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "mag", y = "ANY"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(g(arf(x)))
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "mag", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "mag", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "mag", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "mag", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "mag", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "mag", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "mag", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), y))

setMatrixOpsMethod(
          c(x = "mag", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(acf(x), y))

setMatrixOpsMethod(
          c(x = "mag", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(arb(x), y))

setMatrixOpsMethod(
          c(x = "mag", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "mag"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_mag_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "mag"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_mag_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "mag"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_mag_ops1, "rowSums", x, list(NULL, as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "mag"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "mag"),
                          domain = NA)))

setMethod("solve",
          c(a = "mag", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(solve(arf(a), ...))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "mag", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "mag", b = "ulong"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "mag", b = "slong"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "mag", b = "fmpz"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "mag", b = "fmpq"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "mag", b = "mag"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "mag", b = "arf"),
          function (a, b, ...)
              solve(arf(a), b, ...))

setMethod("solve",
          c(a = "mag", b = "acf"),
          function (a, b, ...)
              solve(acf(a), b, ...))

setMethod("solve",
          c(a = "mag", b = "arb"),
          function (a, b, ...)
              solve(arb(a), b, ...))

setMethod("solve",
          c(a = "mag", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))
