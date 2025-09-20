setMethod("!",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "acf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acf_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "acf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acf_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "acf"),
          function (z)
              .Call(R_flint_acf_ops1, .Generic, z, NULL))

setMethod("Imag",
          c(z = "acf"),
          function (z)
              .Call(R_flint_acf_part, z, 1L))

setMethod("Imag<-",
          c(z = "acf"),
          function (z, value) {
              nz <- length(z)
              nv <- length(value)
              if (nv != 1L && nv != nz)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nz),
                       domain = NA)
              ans <- acf(real = Real(z), imag = value)
              ans@dim <- z@dim
              ans@dimnames <- z@dimnames
              ans@names <- z@names
              ans
          })

setMethod("Math",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "acf"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_acf_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "acf"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(acf(e1), e2),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "acf"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acf", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(e1, acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acf(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acf"),
          function (e1, e2)
              .Call(R_flint_acf_ops2, .Generic, e1, e2, list()))

setMethod("Ops",
          c(e1 = "acf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), acb(e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Real",
          c(z = "acf"),
          function (z)
              .Call(R_flint_acf_part, z, 0L))

setMethod("Real<-",
          c(z = "acf"),
          function (z, value) {
              nz <- length(z)
              nv <- length(value)
              if (nv != 1L && nv != nz)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nz),
                       domain = NA)
              ans <- acf(real = value, imag = Imag(z))
              ans@dim <- z@dim
              ans@dimnames <- z@dimnames
              ans@names <- z@names
              ans
          })

setMethod("Summary",
          c(x = "acf"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acf_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("anyNA",
          c(x = "acf"),
          function (x, recursive = FALSE)
              .Call(R_flint_acf_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "acf"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, rnd = "N"), mode),
                     as.vector(.Call(R_flint_acf_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         backsolve(acf(r), x, , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "acf"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "acf", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_acf_ops1, if (transpose) "tbacksolve" else "backsolve", r, list(as.logical(upper.tri))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         backsolve(r, acf(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "acf", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "acf", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acf(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_acf_ops2, if (transpose) "tbacksolve" else "backsolve", r, x, list(as.logical(upper.tri))))

setMethod("backsolve",
          c(r = "acf", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acf", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "acf"),
          function (x, ...)
              .Call(R_flint_acf_ops1, "chol", x, list()))

setMethod("chol2inv",
          c(x = "acf"),
          function (x, ...)
              .Call(R_flint_acf_ops1, "chol2inv", x, list()))

setAs("ANY", "acf",
      function (from)
          .Call(R_flint_acf_initialize, flintNew("acf"), from, NULL,
                dim(from), dimnames(from), names(from), NULL, NULL))

setMethod("colMeans",
          c(x = "acf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acf_ops1, "colMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("colSums",
          c(x = "acf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acf_ops1, "colSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("det",
          c(x = "acf"),
          function (x, ...)
              .Call(R_flint_acf_ops1, "det", x, NULL))

setMethod("determinant",
          c(x = "acf"),
          function (x, logarithm = TRUE, ...) {
              D <- det(x)
              `class<-`(list(modulus =
                                 if (logarithm)
                                     `attr<-`(log(Mod(D)), "logarithm", TRUE)
                                 else `attr<-`(D, "logarithm", FALSE),
                             argument = Arg(D)),
                        "det")
          })

setMethod("format",
          c(x = "acf"),
          function (x, base = 10L, digits = NULL,
                    sep = NULL, rnd = flintRnd(), ...) {
              r <- format(Real(x), base = base, digits = digits, sep = sep, rnd = rnd, ...)
              i <- format(Imag(x), base = base, digits = digits, sep = sep, rnd = rnd, ...)
              if (!any(s <- startsWith(i, "-")))
                  r[] <- paste0(r, "+", i, "i")
              else {
                  substr(i[!s], 1L, 1L) <- "+"
                  r[] <- paste0(r,      i, "i")
              }
              r
          })

setMethod("is.finite",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "acf"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_acf_ops1, "is.unsorted", x, list(as.logical(na.rm), as.logical(strictly))))

setMatrixOpsMethod(
          c(x = "ANY", y = "acf"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(acf(x), y),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "acf"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "acf", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_acf_ops2, .Generic, x, x, list()))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(x, acf(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "acf", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "acf", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(x, acf(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "acf"),
          function (x, y)
              .Call(R_flint_acf_ops2, .Generic, x, y, list()))

setMatrixOpsMethod(
          c(x = "acf", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), acb(y)))

setMatrixOpsMethod(
          c(x = "acf", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "acf"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acf_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("rowMeans",
          c(x = "acf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acf_ops1, "rowMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("rowSums",
          c(x = "acf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acf_ops1, "rowSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "acf"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         solve(acf(a), b, ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "acf"),
                          domain = NA)))

setMethod("solve",
          c(a = "acf", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_acf_ops1, "solve", a, list()))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         solve(a, acf(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "acf", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "acf", b = "ulong"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "slong"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "fmpz"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "fmpq"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "mag"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "arf"),
          function (a, b, ...)
              solve(a, acf(b), ...))

setMethod("solve",
          c(a = "acf", b = "acf"),
          function (a, b, ...)
              .Call(R_flint_acf_ops2, "solve", a, b, list()))

setMethod("solve",
          c(a = "acf", b = "arb"),
          function (a, b, ...)
              solve(acb(a), acb(b), ...))

setMethod("solve",
          c(a = "acf", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))

setMethod("xtfrm",
          c(x = "acf"),
          function (x) {
              if (!anyNA(x))
                  xtfrm(flintLength(x) * ulong(xtfrm(Real(x)) - 1L) + ulong(xtfrm(Imag(x))))
              else {
                  n <- length(w <- which(k <- !is.na(x)))
                  x <- x[w]
                  ans <- rep(if (is.integer(n)) NA_integer_ else NA_real_, length(k))
                  ans[w] <- xtfrm(ulong(n) * ulong(xtfrm(Real(x)) - 1L) + ulong(xtfrm(Imag(x))))
                  ans
              }
          })
