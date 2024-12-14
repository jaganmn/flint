setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_mag_vector, x), mode))

setAs("ANY", "mag",
      function (from)
          new("mag", x = from))

setMethod("format",
          c(x = "mag"),
          function (x, base = 10L, digits = NULL, sep = NULL, ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "|"
              .Call(R_flint_mag_format, x, base, digits, sep)
          })

setMethod("+",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "-", e1, NULL))

setMethod("Ops",
          c(e1 = "ANY", e2 = "mag"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "mag"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "mag", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "mag", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "mag", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "mag", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "mag"),
          function (e1, e2)
              .Call(R_flint_mag_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "mag", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("log",
          c(x = "mag"),
          function (x, base, ...) {
              if (missing(base))
                  base <- NULL
              else if (length(base) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "base", "log"),
                       domain = NA)
              else base <- as(base, "arf")
              .Call(R_flint_mag_ops1, "log", x, base)
          })

setMethod("Math",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "mag"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_mag_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "mag"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_mag_ops1, .Generic, x, NULL))

setMethod("Complex",
          c(z = "mag"),
          function (z)
              .Call(R_flint_mag_ops1, .Generic, z, NULL))

setMethod("anyNA",
          c(x = "mag"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("is.na",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.nan", x, NULL))

setMethod("is.infinite",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.infinite", x, NULL))

setMethod("is.finite",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "is.finite", x, NULL))

setMethod("!",
          c(x = "mag"),
          function (x)
              .Call(R_flint_mag_ops1, "!", x, NULL))

setMethod("mean",
          c(x = "mag"),
          function (x, ...)
              .Call(R_flint_mag_ops1, "mean", x, NULL))
