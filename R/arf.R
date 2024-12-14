setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arf_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arf_vector, x, "N"), mode))

setAs("ANY", "arf",
      function (from)
          new("arf", x = from))

setMethod("format",
          c(x = "arf"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "|"
              .Call(R_flint_arf_format, x, base, digits, sep, rnd)
          })

setMethod("+",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "-", e1, NULL))

setMethod("Ops",
          c(e1 = "ANY", e2 = "arf"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(new("arf", x = e1), e2),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arf"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(e1, new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "arf"),
          function (e1, e2)
              .Call(R_flint_arf_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Math",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arf"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_arf_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "arf"),
          function (x, ..., na.rm = FALSE) {
              if (missing(na.rm))
                  NULL
              else if (length(na.rm) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "na.rm", .Generic),
                       domain = NA)
              else na.rm <- as.logical(na.rm)
              .Call(R_flint_arf_ops1, .Generic, x, na.rm)
          })

setMethod("Complex",
          c(z = "arf"),
          function (z)
              .Call(R_flint_arf_ops1, .Generic, z, NULL))

setMethod("anyNA",
          c(x = "arf"),
          function (x, recursive = FALSE)
              .Call(R_flint_arf_ops1, "anyNA", x, NULL))

setMethod("is.na",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.nan", x, NULL))

setMethod("is.infinite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.infinite", x, NULL))

setMethod("is.finite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.finite", x, NULL))

setMethod("!",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "!", x, NULL))

setMethod("mean",
          c(x = "arf"),
          function (x, na.rm = FALSE, ...) {
              if (missing(na.rm))
                  NULL
              else if (length(na.rm) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "na.rm", "mean"),
                       domain = NA)
              else na.rm <- as.logical(na.rm)
              .Call(R_flint_arf_ops1, "mean", x, na.rm)
          })
