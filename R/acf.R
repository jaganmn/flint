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
          function (z, value)
              new("acf", real = Real(z), imag = value))

setMethod("Math",
          c(x = "acf"),
          function (x)
              .Call(R_flint_acf_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "acf"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_acf_ops1, .Generic, x, digits)
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "acf"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(new("acf", x = e1), e2),
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
                         g(e1, new("acf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "acf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "acf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("acf", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acf"),
          function (e1, e2)
              .Call(R_flint_acf_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "acf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), new("acb", x = e2)))

setMethod("Ops",
          c(e1 = "acf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Real",
          c(z = "acf"),
          function (z)
              .Call(R_flint_acf_part, z, 0L))

setMethod("Real<-",
          c(z = "acf"),
          function (z, value)
              new("acf", real = value, imag = Imag(z)))

setMethod("Summary",
          c(x = "acf"),
          function (x, ..., na.rm = FALSE) {
              if (missing(na.rm))
                  NULL
              else if (length(na.rm) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "na.rm", .Generic),
                       domain = NA)
              else na.rm <- as.logical(na.rm)
              .Call(R_flint_acf_ops1, .Generic, x, na.rm)
          })

setMethod("all.equal",
          c(target = "acf", current = "acf"),
          function (target, current, ...)
              all.equal(list(real = Real(target),
                             imag = Imag(target)),
                        list(real = Real(current),
                             imag = Imag(current)),
                        ...))

setMethod("anyNA",
          c(x = "acf"),
          function (x, recursive = FALSE)
              .Call(R_flint_acf_ops1, "anyNA", x, NULL))

setMethod("as.vector",
          c(x = "acf"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_acf_vector, x), mode))

setAs("ANY", "acf",
      function (from)
          new("acf", x = from))

setMethod("format",
          c(x = "acf"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              paste0(format(Real(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "+",
                     format(Imag(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "i"))

setMethod("initialize",
          c(.Object = "acf"),
          function (.Object, length = 0L, x = NULL, real, imag, ...)
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

setMethod("mean",
          c(x = "acf"),
          function (x, na.rm = FALSE, ...) {
              if (missing(na.rm))
                  NULL
              else if (length(na.rm) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "na.rm", "mean"),
                       domain = NA)
              else na.rm <- as.logical(na.rm)
              .Call(R_flint_acf_ops1, "mean", x, na.rm)
          })
