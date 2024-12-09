setMethod("initialize",
          c(.Object = "fmpz"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_fmpz_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "fmpz"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpz_vector, x), mode))

setAs("ANY", "fmpz",
      function (from)
          new("fmpz", x = from))

setMethod("format",
          c(x = "fmpz"),
          function (x, base = 10L, ...)
              .Call(R_flint_fmpz_format, x, base))

setMethod("+",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "-", e1, NULL))

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpz"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpz", x = e1), e2),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
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
                         g(e1, new("fmpz", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpz", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpz", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpz"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpq", x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Math",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpz"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_fmpz_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "fmpz"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_fmpz_ops1, .Generic, x, NULL))

setMethod("Complex",
          c(z = "fmpz"),
          function (z)
              .Call(R_flint_fmpz_ops1, .Generic, z, NULL))
