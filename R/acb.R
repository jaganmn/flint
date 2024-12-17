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
          function (z, value)
              acb(real = Real(z), imag = value))

setMethod("Math",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "acb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_acb_ops1, .Generic, x, as(digits, "slong"))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "acb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(acb(x = e1), e2),
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
                         g(e1, acb(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

setMethod("Ops",
          c(e1 = "acb", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, acb(x = e2)))

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
          function (z, value)
              acb(real = value, imag = Imag(z)))

setMethod("Summary",
          c(x = "acb"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_acb_ops1, .Generic, x, as.logical(na.rm)))

setMethod("all.equal",
          c(target = "acb", current = "acb"),
          function (target, current, ...)
              all.equal(list(real = Real(target),
                             imag = Imag(target)),
                        list(real = Real(current),
                             imag = Imag(current)),
                        ...))

setMethod("anyNA",
          c(x = "acb"),
          function (x, recursive = FALSE)
              .Call(R_flint_acb_ops1, "anyNA", x, NULL))

setMethod("as.vector",
          c(x = "acb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_acb_vector, x), mode))

setAs("ANY", "acb",
      function (from)
          acb(x = from))

setMethod("format",
          c(x = "acb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              paste0(format(Real(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "+",
                     format(Imag(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "i"))

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

setMethod("log",
          c(x = "acb"),
          function (x, base, ...) {
              if (missing(base))
                  base <- NULL
              else if (length(base) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "base", "log"),
                       domain = NA)
              else base <- as(base, "acb")
              .Call(R_flint_acb_ops1, "log", x, base)
          })

setMethod("mean",
          c(x = "acb"),
          function (x, na.rm = FALSE, ...)
              .Call(R_flint_acb_ops1, "mean", x, as.logical(na.rm)))

setMethod("xtfrm",
          c(x = "acb"),
          function (x)
              stop(gettextf("'%s' is not a total order on the range of '%s'",
                            "<=", "arb"),
                   domain = NA))
