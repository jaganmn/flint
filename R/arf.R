setMethod("!",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "arf"),
          function (z)
              .Call(R_flint_arf_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arf"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_arf_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "arf"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(e1), e2),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arf"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(e1, arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "arf"),
          function (e1, e2)
              .Call(R_flint_arf_ops2, .Generic, e1, e2, NULL))

setMethod("Ops",
          c(e1 = "arf", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acf(e1), e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(arb(e1), e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Summary",
          c(x = "arf"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = FALSE)
              else .Call(R_flint_arf_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("anyNA",
          c(x = "arf"),
          function (x, recursive = FALSE)
              .Call(R_flint_arf_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, rnd = "N"), mode),
                     as.vector(.Call(R_flint_arf_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(arf(r), x, , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "arf"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "arf", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_arf_ops1, if (transpose) "tbacksolve" else "backsolve", r, list(as.logical(upper.tri))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(r, arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "arf", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "arf", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_arf_ops2, if (transpose) "tbacksolve" else "backsolve", r, x, list(as.logical(upper.tri))))

setMethod("backsolve",
          c(r = "arf", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arb(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arf", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "arf"),
          function (x, ...)
              .Call(R_flint_arf_ops1, "chol", x, NULL))

setMethod("chol2inv",
          c(x = "arf"),
          function (x, ...)
              .Call(R_flint_arf_ops1, "chol2inv", x, NULL))

setAs("ANY", "arf",
      function (from)
          .Call(R_flint_arf_initialize, flintNew("arf"), from, NULL,
                dim(from), dimnames(from), names(from), NULL, NULL))

setMethod("colMeans",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "colMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("colSums",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "colSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("diff",
          c(x = "arf"),
          function (x, lag = 1L, differences = 1L, ...)
              .Call(R_flint_arf_ops1, "diff", x,
                    list(as.integer(lag), as.integer(differences))))

setMethod("diffinv",
          c(x = "arf"),
          function (x, lag = 1L, differences = 1L, xi, ...)
              .Call(R_flint_arf_ops1, "diffinv", x,
                    list(as.integer(lag), as.integer(differences),
                         if (!missing(xi)) as(xi, "arf"))))

setMethod("format",
          c(x = "arf"),
          function (x, base = 10L, sep = NULL,
                    digits = NULL, rnd = NULL, ...) {
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "@"
              if (is.null(digits))
                  digits <- getOption("digits")
              .Call(R_flint_arf_format, x, base, sep, digits, rnd)
          })

setMethod("det",
          c(x = "arf"),
          function (x, ...)
              .Call(R_flint_arf_ops1, "det", x, NULL))

setMethod("determinant",
          c(x = "arf"),
          function (x, logarithm = TRUE, ...) {
              D <- det(x)
              `class<-`(list(modulus =
                                 if (logarithm)
                                     `attr<-`(log(abs(D)), "logarithm", TRUE)
                                 else `attr<-`(D, "logarithm", FALSE),
                             sign = if (D >= 0) 1L else -1L),
                        "det")
          })

setMethod("is.finite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "arf"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_arf_ops1, "is.unsorted", x, list(as.logical(na.rm), as.logical(strictly))))

setMethod("isComplex",
          c(x = "arf"),
          function (x) FALSE)

setMethod("isFloating",
          c(x = "arf"),
          function (x) TRUE)

setMethod("isSigned",
          c(x = "arf"),
          function (x) TRUE)

setMethod("log",
          c(x = "arf"),
          function (x, base, ...)
              .Call(R_flint_arf_ops1, "log", x,
                    if (!missing(base)) list(as(base, "arf"))))

setMatrixOpsMethod(
          c(x = "ANY", y = "arf"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arf(x), y),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "arf"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "arf", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_arf_ops2, .Generic, x, x, NULL))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(x, arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "arf", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "arf", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, arf(y)))

setMatrixOpsMethod(
          c(x = "arf", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, arf(y)))

setMatrixOpsMethod(
          c(x = "arf", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(x, arf(y)))

setMatrixOpsMethod(
          c(x = "arf", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(x, arf(y)))

setMatrixOpsMethod(
          c(x = "arf", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(x, arf(y)))

setMatrixOpsMethod(
          c(x = "arf", y = "arf"),
          function (x, y)
              .Call(R_flint_arf_ops2, .Generic, x, y, NULL))

setMatrixOpsMethod(
          c(x = "arf", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(acf(x), y))

setMatrixOpsMethod(
          c(x = "arf", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(arb(x), y))

setMatrixOpsMethod(
          c(x = "arf", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "arf"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = FALSE)
              else .Call(R_flint_arf_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("rowMeans",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "rowMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("rowSums",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "rowSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "arf"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(arf(a), b, ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "arf"),
                          domain = NA)))

setMethod("solve",
          c(a = "arf", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_arf_ops1, "solve", a, NULL))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(a, arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "arf", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "arf", b = "ulong"),
          function (a, b, ...)
              solve(a, arf(b), ...))

setMethod("solve",
          c(a = "arf", b = "slong"),
          function (a, b, ...)
              solve(a, arf(b), ...))

setMethod("solve",
          c(a = "arf", b = "fmpz"),
          function (a, b, ...)
              solve(a, arf(b), ...))

setMethod("solve",
          c(a = "arf", b = "fmpq"),
          function (a, b, ...)
              solve(a, arf(b), ...))

setMethod("solve",
          c(a = "arf", b = "mag"),
          function (a, b, ...)
              solve(a, arf(b), ...))

setMethod("solve",
          c(a = "arf", b = "arf"),
          function (a, b, ...)
              .Call(R_flint_arf_ops2, "solve", a, b, NULL))

setMethod("solve",
          c(a = "arf", b = "acf"),
          function (a, b, ...)
              solve(acf(a), b, ...))

setMethod("solve",
          c(a = "arf", b = "arb"),
          function (a, b, ...)
              solve(arb(a), b, ...))

setMethod("solve",
          c(a = "arf", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))
