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
              ans <- .acb(real = Real(z), imag = value)
              if (!is.null(nms <- names(z)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
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

setMethod("Ops",
          c(e1 = "ANY", e2 = "acb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(.acb(x = e1), e2),
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
                         g(e1, .acb(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "acb"),
          function (e1, e2)
              .Call(R_flint_acb_ops2, .Generic, e1, e2))

setMethod("Real",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_part, z, 0L))

setMethod("Real<-",
          c(z = "acb"),
          function (z, value) {
              ans <- .acb(real = value, imag = Imag(z))
              if (!is.null(nms <- names(z)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
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
                         as.vector(format(x, digits = 15L, rnd = "N"), mode),
                     as.vector(.Call(R_flint_acb_atomic, x), mode)))

setAs("ANY", "acb",
      function (from)
          .acb(x = from))

setMethod("format",
          c(x = "acb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              `names<-`(paste0(format(Real(x), base = base, digits = digits, sep = sep, rnd = rnd, ...),
                               "+",
                               format(Imag(x), base = base, digits = digits, sep = sep, rnd = rnd, ...),
                               "i"),
                        names(x)))

setMethod("initialize",
          c(.Object = "acb"),
          function (.Object, length = NULL, x = NULL, real, imag, ...)
              .Call(R_flint_acb_initialize, .Object, length, x,
                    if (!missing(real)) as(real, "arb"),
                    if (!missing(imag)) as(imag, "arb")))

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

setMethod("log",
          c(x = "acb"),
          function (x, base, ...)
              .Call(R_flint_acb_ops1, "log", x,
                    if (!missing(base)) list(as(base, "acb"))))

setMethod("mean",
          c(x = "acb"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_acb_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("xtfrm",
          c(x = "acb"),
          function (x)
              stop(.error.notTotalOrder()))
