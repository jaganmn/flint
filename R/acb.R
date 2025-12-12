setMethod("!",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "acb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acb_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "acb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acb_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_ops1, .Generic, z, NULL))

setMethod("Imag",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_part, z, 1L))

setMethod("Imag<-",
          c(z = "acb"),
          function (z, value) {
              nz <- length(z)
              nv <- length(value)
              if (nv != 1L && nv != nz)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nz),
                       domain = NA)
              ans <- acb(real = Real(z), imag = value)
              ans@dim <- z@dim
              ans@dimnames <- z@dimnames
              ans@names <- z@names
              ans
          })

setMethod("Math",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "acb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_acb_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Mid",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_part, x, NA_integer_))

setMethod("Mid<-",
          c(x = "acb"),
          function (x, value) {
              nx <- length(x)
              nv <- length(value)
              if (nv != 1L && nv != nx)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nx),
                       domain = NA)
              ans <- acb(real = arb(mid = Real(value),
                                    rad = Rad(Real(x))),
                         imag = arb(mid = Imag(value),
                                    rad = Rad(Imag(x))))
              ans@dim <- x@dim
              ans@dimnames <- x@dimnames
              ans@names <- x@names
              ans
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "acb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(acb(e1), e2),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "acb"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acb", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(e1, acb(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "acb"),
          function (e1, e2)
              .Call(R_flint_acb_ops2, .Generic, e1, e2, NULL))

setMethod("Real",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_part, z, 0L))

setMethod("Real<-",
          c(z = "acb"),
          function (z, value) {
              nz <- length(z)
              nv <- length(value)
              if (nv != 1L && nv != nz)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nz),
                       domain = NA)
              ans <- acb(real = value, imag = Imag(z))
              ans@dim <- z@dim
              ans@dimnames <- z@dimnames
              ans@names <- z@names
              ans
          })

setMethod("Summary",
          c(x = "acb"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acb_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("anyNA",
          c(x = "acb"),
          function (x, recursive = FALSE)
              .Call(R_flint_acb_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "acb"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, digits.mag = 8L, rnd = "N", rnd.mag = "A"), mode),
                     as.vector(.Call(R_flint_acb_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         backsolve(acb(r), x, k, upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "acb"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "acb", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_acb_ops1, if (transpose) "tbacksolve" else "backsolve", r, list(as.logical(upper.tri))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         backsolve(r, acb(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "acb", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "acb", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "acb", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_acb_ops2, if (transpose) "tbacksolve" else "backsolve", r, x, list(as.logical(upper.tri))))

setMethod("chol",
          c(x = "acb"),
          function (x, ...)
              .Call(R_flint_acb_ops1, "chol", x, NULL))

setMethod("chol2inv",
          c(x = "acb"),
          function (x, ...)
              .Call(R_flint_acb_ops1, "chol2inv", x, NULL))

setAs("ANY", "acb",
      function (from)
          .Call(R_flint_acb_initialize, flintNew("acb"), from, NULL,
                dim(from), dimnames(from), names(from), NULL, NULL,
                NULL))

setMethod("colMeans",
          c(x = "acb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acb_ops1, "colMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("colSums",
          c(x = "acb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acb_ops1, "colSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("det",
          c(x = "acb"),
          function (x, ...)
              .Call(R_flint_acb_ops1, "det", x, NULL))

setMethod("determinant",
          c(x = "acb"),
          function (x, logarithm = TRUE, ...) {
              D <- det(x)
              `class<-`(list(modulus =
                                 if (logarithm)
                                     `attr<-`(log(Mod(D)), "logarithm", TRUE)
                                 else `attr<-`(D, "logarithm", FALSE),
                             argument = Arg(D)),
                        "det")
          })

setMethod("diff",
          c(x = "acb"),
          function (x, lag = 1L, differences = 1L, ...)
              .Call(R_flint_acb_ops1, "diff", x,
                    list(as.integer(lag), as.integer(differences))))

setMethod("diffinv",
          c(x = "acb"),
          function (x, lag = 1L, differences = 1L, xi, ...)
              .Call(R_flint_acb_ops1, "diffinv", x,
                    list(as.integer(lag), as.integer(differences),
                         if (!missing(xi)) as(xi, "acb"))))

setMethod("format",
          c(x = "acb"),
          function (x, base = 10L, sep = NULL,
                    digits = NULL, digits.mag = NULL,
                    rnd = NULL, rnd.mag = "A", ...) {
              r <- format(Real(x), base = base, sep = sep,
                          digits = digits, digits.mag = digits.mag,
                          rnd = rnd, rnd.mag = rnd.mag, ...)
              i <- format(Imag(x), base = base, sep = sep,
                          digits = digits, digits.mag = digits.mag,
                          rnd = rnd, rnd.mag = rnd.mag, ...)
              r[] <- paste0(r, "+", i, "i")
              r
          })

setMethod("is.finite",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "acb"),
          function (x, na.rm = FALSE, strictly = FALSE)
              stop(.error.notTotalOrder()))

setMethod("isComplex",
          c(x = "acb"),
          function (x) TRUE)

setMethod("isFloating",
          c(x = "acb"),
          function (x) TRUE)

setMethod("isSigned",
          c(x = "acb"),
          function (x) TRUE)

setMethod("log",
          c(x = "acb"),
          function (x, base, ...)
              .Call(R_flint_acb_ops1, "log", x,
                    if (!missing(base)) list(as(base, "acb"))))

setMatrixOpsMethod(
          c(x = "ANY", y = "acb"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(acb(x), y),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "acb"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "acb", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_acb_ops2, .Generic, x, x, NULL))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(x, acb(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "acb", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "acb", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "arb"),
          function (x, y)
              get(.Generic, mode = "function")(x, acb(y)))

setMatrixOpsMethod(
          c(x = "acb", y = "acb"),
          function (x, y)
              .Call(R_flint_acb_ops2, .Generic, x, y, NULL))

setMethod("mean",
          c(x = "acb"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acb_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("rowMeans",
          c(x = "acb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acb_ops1, "rowMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("rowSums",
          c(x = "acb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_acb_ops1, "rowSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "acb"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         solve(acb(a), b, ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "acb"),
                          domain = NA)))

setMethod("solve",
          c(a = "acb", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_acb_ops1, "solve", a, NULL))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         solve(a, acb(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "acb", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "acb", b = "ulong"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "slong"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "fmpz"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "fmpq"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "mag"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "arf"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "acf"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "arb"),
          function (a, b, ...)
              solve(a, acb(b), ...))

setMethod("solve",
          c(a = "acb", b = "acb"),
          function (a, b, ...)
              .Call(R_flint_acb_ops2, "solve", a, b, NULL))

setMethod("xtfrm",
          c(x = "acb"),
          function (x)
              stop(.error.notTotalOrder()))
