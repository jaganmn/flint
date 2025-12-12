setMethod("!",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "arb"),
          function (z)
              .Call(R_flint_arb_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_arb_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Mid",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 0L))

setMethod("Mid<-",
          c(x = "arb"),
          function (x, value) {
              nx <- length(x)
              nv <- length(value)
              if (nv != 1L && nv != nx)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nx),
                       domain = NA)
              ans <- arb(mid = value, rad = Rad(x))
              ans@dim <- x@dim
              ans@dimnames <- x@dimnames
              ans@names <- x@names
              ans
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "arb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arb(e1), e2),
                     "complex" =
                         g(acb(e1), acb(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arb"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(e1, arb(e2)),
                     "complex" =
                         g(acb(e1), acb(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), acb(e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arb"),
          function (e1, e2)
              .Call(R_flint_arb_ops2, .Generic, e1, e2, NULL))

setMethod("Ops",
          c(e1 = "arb", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(e1), e2))

setMethod("Summary",
          c(x = "arb"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_arb_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("Rad",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 1L))

setMethod("Rad<-",
          c(x = "arb"),
          function (x, value) {
              nx <- length(x)
              nv <- length(value)
              if (nv != 1L && nv != nx)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "x", nx),
                       domain = NA)
              ans <- arb(mid = Mid(x), rad = value)
              ans@dim <- x@dim
              ans@dimnames <- x@dimnames
              ans@names <- x@names
              ans
          })

setMethod("anyNA",
          c(x = "arb"),
          function (x, recursive = FALSE)
              .Call(R_flint_arb_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, digits.mag = 8L, rnd = "N", rnd.mag = "A"), mode),
                     as.vector(.Call(R_flint_arb_atomic, x), mode)))

setMethod("backsolve",
          c(r = "ANY", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              switch(typeof(r),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(arb(r), x, , upper.tri, transpose),
                     "complex" =
                         backsolve(acb(r), acb(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", if (isS4(r)) class(r) else typeof(r), "arb"),
                          domain = NA)))

setMethod("backsolve",
          c(r = "arb", x = "ANY"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE) {
              if (missing(x))
                  return(.Call(R_flint_arb_ops1, if (transpose) "tbacksolve" else "backsolve", r, list(as.logical(upper.tri))))
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         backsolve(r, arb(x), , upper.tri, transpose),
                     "complex" =
                         backsolve(acb(r), acb(x), , upper.tri, transpose),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "backsolve", "arb", if (isS4(x)) class(x) else typeof(x)),
                          domain = NA))
          })

setMethod("backsolve",
          c(r = "arb", x = "ulong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "slong"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "fmpz"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "fmpq"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "mag"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "arf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(r, arb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "acf"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), acb(x), , upper.tri, transpose))

setMethod("backsolve",
          c(r = "arb", x = "arb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              .Call(R_flint_arb_ops2, if (transpose) "tbacksolve" else "backsolve", r, x, list(as.logical(upper.tri))))

setMethod("backsolve",
          c(r = "arb", x = "acb"),
          function (r, x, k = ncol(r), upper.tri = TRUE, transpose = FALSE)
              backsolve(acb(r), x, , upper.tri, transpose))

setMethod("chol",
          c(x = "arb"),
          function (x, ...)
              .Call(R_flint_arb_ops1, "chol", x, NULL))

setMethod("chol2inv",
          c(x = "arb"),
          function (x, ...)
              .Call(R_flint_arb_ops1, "chol2inv", x, NULL))

setAs("ANY", "arb",
      function (from)
          .Call(R_flint_arb_initialize, flintNew("arb"), from, NULL,
                dim(from), dimnames(from), names(from), NULL, NULL,
                NULL))

setMethod("colMeans",
          c(x = "arb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arb_ops1, "colMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("colSums",
          c(x = "arb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arb_ops1, "colSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("det",
          c(x = "arb"),
          function (x, ...)
              .Call(R_flint_arb_ops1, "det", x, NULL))

setMethod("determinant",
          c(x = "arb"),
          function (x, logarithm = TRUE, ...) {
              D <- det(x)
              `class<-`(list(modulus =
                                 if (logarithm)
                                     `attr<-`(log(abs(D)), "logarithm", TRUE)
                                 else `attr<-`(D, "logarithm", FALSE),
                             sign = if (D >= 0) 1L else if (D <= 0) -1L else NA_integer_),
                        "det")
          })

setMethod("diff",
          c(x = "arb"),
          function (x, lag = 1L, differences = 1L, ...)
              .Call(R_flint_arb_ops1, "diff", x,
                    list(as.integer(lag), as.integer(differences))))

setMethod("diffinv",
          c(x = "arb"),
          function (x, lag = 1L, differences = 1L, xi, ...)
              .Call(R_flint_arb_ops1, "diffinv", x,
                    list(as.integer(lag), as.integer(differences),
                         if (!missing(xi)) as(xi, "arb"))))

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, sep = NULL,
                    digits = NULL, digits.mag = NULL,
                    rnd = NULL, rnd.mag = "A", ...) {
              m <- format(Mid(x), base = base, sep = sep,
                          digits = digits,
                          rnd = rnd, ...)
              r <- format(Rad(x), base = base, sep = sep,
                          digits.mag = digits.mag,
                          rnd.mag = rnd.mag, ...)
              m[] <- paste0("(", m, " +/- ", r, ")")
              m
          })

setMethod("is.finite",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "arb"),
          function (x, na.rm = FALSE, strictly = FALSE)
              stop(.error.notTotalOrder()))

setMethod("isComplex",
          c(x = "arb"),
          function (x) FALSE)

setMethod("isFloating",
          c(x = "arb"),
          function (x) TRUE)

setMethod("isSigned",
          c(x = "arb"),
          function (x) TRUE)

setMethod("log",
          c(x = "arb"),
          function (x, base, ...)
              .Call(R_flint_arb_ops1, "log", x,
                    if (!missing(base)) list(as(base, "arb"))))

setMatrixOpsMethod(
          c(x = "ANY", y = "arb"),
          function (x, y) {
              g <- get(.Generic, mode = "function")
              switch(typeof(x),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arb(x), y),
                     "complex" =
                         g(acb(x), acb(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), if (isS4(x)) class(x) else typeof(x), "arb"),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "arb", y = "ANY"),
          function (x, y) {
              if (.Generic != "%*%" && (missing(y) || is.null(y)))
                  return(.Call(R_flint_arb_ops2, .Generic, x, x, NULL))
              g <- get(.Generic, mode = "function")
              switch(typeof(y),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(x, arb(y)),
                     "complex" =
                         g(acb(x), acb(y)),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   deparse(as.name(.Generic), backtick = TRUE), "arb", if (isS4(y)) class(y) else typeof(y)),
                          domain = NA))
          })

setMatrixOpsMethod(
          c(x = "arb", y = "ulong"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "slong"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "fmpz"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "fmpq"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "mag"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "arf"),
          function (x, y)
              get(.Generic, mode = "function")(x, arb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "acf"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), acb(y)))

setMatrixOpsMethod(
          c(x = "arb", y = "arb"),
          function (x, y)
              .Call(R_flint_arb_ops2, .Generic, x, y, NULL))

setMatrixOpsMethod(
          c(x = "arb", y = "acb"),
          function (x, y)
              get(.Generic, mode = "function")(acb(x), y))

setMethod("mean",
          c(x = "arb"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_arb_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("rowMeans",
          c(x = "arb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arb_ops1, "rowMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("rowSums",
          c(x = "arb"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arb_ops1, "rowSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("solve",
          c(a = "ANY", b = "arb"),
          function (a, b, ...)
              switch(typeof(a),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(arb(a), b, ...),
                     "complex" =
                         solve(acb(a), acb(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", if (isS4(a)) class(a) else typeof(b), "arb"),
                          domain = NA)))

setMethod("solve",
          c(a = "arb", b = "ANY"),
          function (a, b, ...) {
              if (missing(b))
                  return(.Call(R_flint_arb_ops1, "solve", a, NULL))
              switch(typeof(b),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         solve(a, arb(b), ...),
                     "complex" =
                         solve(acb(a), acb(b), ...),
                     stop(gettextf("%s(<%s>, <%s>) is not yet implemented",
                                   "solve", "arb", if (isS4(b)) class(b) else typeof(b)),
                          domain = NA))
          })

setMethod("solve",
          c(a = "arb", b = "ulong"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "slong"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "fmpz"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "fmpq"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "mag"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "arf"),
          function (a, b, ...)
              solve(a, arb(b), ...))

setMethod("solve",
          c(a = "arb", b = "acf"),
          function (a, b, ...)
              solve(acb(a), acb(b), ...))

setMethod("solve",
          c(a = "arb", b = "arb"),
          function (a, b, ...)
              .Call(R_flint_arb_ops2, "solve", a, b, NULL))

setMethod("solve",
          c(a = "arb", b = "acb"),
          function (a, b, ...)
              solve(acb(a), b, ...))

setMethod("xtfrm",
          c(x = "arb"),
          function (x)
              stop(.error.notTotalOrder()))
