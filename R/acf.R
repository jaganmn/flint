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
              ans <- .acf(real = Real(z), imag = value)
              if (!is.null(nms <- names(z)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
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
                         g(.acf(x = e1), e2),
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
                         g(e1, .acf(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acf(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acf"),
          function (e1, e2)
              .Call(R_flint_acf_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "acf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), e2))

setMethod("Real",
          c(z = "acf"),
          function (z)
              .Call(R_flint_acf_part, z, 0L))

setMethod("Real<-",
          c(z = "acf"),
          function (z, value) {
              ans <- .acf(real = value, imag = Imag(z))
              if (!is.null(nms <- names(z)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
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

setAs("ANY", "acf",
      function (from)
          .acf(x = from))

setMethod("format",
          c(x = "acf"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              `names<-`(paste0(format(Real(x), base = base, digits = digits, sep = sep, rnd = rnd, ...),
                               "+",
                               format(Imag(x), base = base, digits = digits, sep = sep, rnd = rnd, ...),
                               "i"),
                        names(x)))

setMethod("initialize",
          c(.Object = "acf"),
          function (.Object, length = NULL, x = NULL, real, imag, ...)
              .Call(R_flint_acf_initialize, .Object, length, x,
                    if (!missing(real)) as(real, "arf"),
                    if (!missing(imag)) as(imag, "arf")))

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

setMethod("mean",
          c(x = "acf"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acf_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("xtfrm",
          c(x = "acf"),
          function (x) {
              if (!anyNA(x))
                  xtfrm(flintLength(x) * .ulong(x = xtfrm(Real(x)) - 1L) + .ulong(x = xtfrm(Imag(x))))
              else {
                  n <- length(w <- which(k <- !is.na(x)))
                  x <- x[w]
                  ans <- rep(if (is.integer(n)) NA_integer_ else NA_real_, length(k))
                  ans[w] <- xtfrm(.ulong(x = n) * .ulong(x = xtfrm(Real(x)) - 1L) + .ulong(x = xtfrm(Imag(x))))
                  ans
              }
          })
