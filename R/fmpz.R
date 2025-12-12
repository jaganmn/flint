setMethod("!",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "fmpz"),
          function (z)
              .Call(R_flint_fmpz_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpz"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_fmpz_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpz"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(fmpz(e1), e2),
                     "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "fmpz"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpz", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(e1, fmpz(e2)),
                     "double" =
                         g(arf(e1), arf(e2)),
                     "complex" =
                         g(acf(e1), acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpz", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpz", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, fmpz(e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, fmpz(e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpz"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops2, .Generic, e1, e2, NULL))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(fmpq(e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), arf(e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(arf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(arb(e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Summary",
          c(x = "fmpz"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpz_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "fmpz"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "fmpz"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_fmpz_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(fmpz(r), x, , upper.tri, transpose),
                     "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "fmpz"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "fmpz", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_fmpz_ops1, if (transpose) "tbacksolve" else "backsolve", r, list(as.logical(upper.tri))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         backsolve(r, fmpz(x), , upper.tri, transpose),
                     "double" =
                         backsolve(arf(r), arf(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acf(r), acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "fmpz", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "fmpz", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, fmpz(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, fmpz(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_fmpz_ops2, if (transpose) "tbacksolve" else "backsolve", r, x, list(as.logical(upper.tri))))

setMethod("backsolve",
          c(r = "fmpz", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(fmpq(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), arf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acf(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(arb(r), x, , upper.tri, transpose))

setMethod("backsolve",
          c(r = "fmpz", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "fmpz"),
          function (x, ...)
              chol(arf(x), ...))

setMethod("chol2inv",
          c(x = "fmpz"),
          function (x, ...)
              tcrossprod(backsolve(x)))

setAs("ANY", "fmpz",
      function (from)
          .Call(R_flint_fmpz_initialize, flintNew("fmpz"), from, NULL,
                dim(from), dimnames(from), names(from)))

setMethod("colMeans",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpz_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpz_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("det",
          c(x = "fmpz"),
          function (x, ...)
              .Call(R_flint_fmpz_ops1, "det", x, NULL))

setMethod("determinant",
          c(x = "fmpz"),
          function (x, logarithm = TRUE, ...) {
              D <- det(x)
              `class<-`(list(modulus =
                                 if (logarithm)
                                     `attr<-`(log(arf(abs(D))), "logarithm", TRUE)
                                 else `attr<-`(D, "logarithm", FALSE),
                             sign = if (D >= 0L) 1L else -1L),
                        "det")
          })

setMethod("diff",
          c(x = "fmpz"),
          function (x, lag = 1L, differences = 1L, ...)
              .Call(R_flint_fmpz_ops1, "diff", x,
                    list(as.integer(lag), as.integer(differences))))

setMethod("diffinv",
          c(x = "fmpz"),
          function (x, lag = 1L, differences = 1L, xi, ...)
              .Call(R_flint_fmpz_ops1, "diffinv", x,
                    list(as.integer(lag), as.integer(differences),
                         if (!missing(xi)) as(xi, "fmpz"))))

setMethod("format",
          c(x = "fmpz"),
          function (x, base = 10L, ...)
              .Call(R_flint_fmpz_format, x, base))

setMethod("is.finite",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_fmpz_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMethod("isComplex",
          c(x = "fmpz"),
          function (x) FALSE)

setMethod("isFloating",
          c(x = "fmpz"),
          function (x) FALSE)

setMethod("isSigned",
          c(x = "fmpz"),
          function (x) TRUE)

setMatrixOpsMethod(
          c(x = "ANY", y = "fmpz"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(fmpz(x), y),
                     "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "fmpz"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "fmpz", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_fmpz_ops2, .Generic, x, x, NULL))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(x, fmpz(y)),
                     "double" =
                         g(arf(x), arf(y)),
                     "complex" =
                         g(acf(x), acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "fmpz", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "fmpz", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, fmpz(y)))

setMatrixOpsMethod(
          c(x = "fmpz", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, fmpz(y)))

setMatrixOpsMethod(
          c(x = "fmpz", y = "fmpz"),
          function (x, y)
              .Call(R_flint_fmpz_ops2, .Generic, x, y, NULL))

setMatrixOpsMethod(
          c(x = "fmpz", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(fmpq(x), y))

setMatrixOpsMethod(
          c(x = "fmpz", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), arf(y)))

setMatrixOpsMethod(
          c(x = "fmpz", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(arf(x), y))

setMatrixOpsMethod(
          c(x = "fmpz", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(acf(x), y))

setMatrixOpsMethod(
          c(x = "fmpz", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(arb(x), y))

setMatrixOpsMethod(
          c(x = "fmpz", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpz_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpz_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpz_ops1, "rowSums", x, list(NULL, as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "fmpz"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(fmpz(a), b, ...),
                     "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "fmpz"),
                          domain = NA)))

setMethod("solve",
          c(a = "fmpz", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_fmpz_ops1, "solve", a, NULL))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         solve(a, fmpz(b), ...),
                     "double" =
                         solve(arf(a), arf(b), ...),
                     "complex" =
                         solve(acf(a), acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "fmpz", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "fmpz", b = "ulong"),
          function (a, b, ...)
              solve(a, fmpz(b), ...))

setMethod("solve",
          c(a = "fmpz", b = "slong"),
          function (a, b, ...)
              solve(a, fmpz(b), ...))

setMethod("solve",
          c(a = "fmpz", b = "fmpz"),
          function (a, b, ...)
              .Call(R_flint_fmpz_ops2, "solve", a, b, NULL))

setMethod("solve",
          c(a = "fmpz", b = "fmpq"),
          function (a, b, ...)
              solve(fmpq(a), b, ...))

setMethod("solve",
          c(a = "fmpz", b = "mag"),
          function (a, b, ...)
              solve(arf(a), arf(b), ...))

setMethod("solve",
          c(a = "fmpz", b = "arf"),
          function (a, b, ...)
              solve(arf(a), b, ...))

setMethod("solve",
          c(a = "fmpz", b = "acf"),
          function (a, b, ...)
              solve(acf(a), b, ...))

setMethod("solve",
          c(a = "fmpz", b = "arb"),
          function (a, b, ...)
              solve(arb(a), b, ...))

setMethod("solve",
          c(a = "fmpz", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))
