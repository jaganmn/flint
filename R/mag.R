setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_mag_vector, x), mode))

setAs("vector", "mag",
      function (from)
          new("mag", x = from))

setAs("nmag", "mag",
      function (from)
          new("mag", x = from))

setAs("mag", "nmag",
      function (from)
          .Call(R_flint_mag_nmag, from))

setMethod("format",
          c(x = "mag"),
          function (x, base = 10L, digits = NULL, sep = NULL, rnd = "U", ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "|"
              .Call(R_flint_mag_format, x, base, digits, sep, rnd)
          })

setMethod("+",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "mag", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_mag_ops1, "-", e1, NULL))

setMethod("^",
          c(e1 = "mag", e2 = "arf"),
          function (e1, e2)
              .Call(R_flint_mag_ops2, "^", e1, e2))

setMethod("^",
          c(e1 = "mag", e2 = "mag"),
          function (e1, e2)
              stop(gettextf("exponentiation of '%s' requires signed exponent",
                            "mag"),
                   domain = NA))

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

setMethod("Ops",
          c(e1 = "mag", e2 = "mag"),
          function (e1, e2)
              .Call(R_flint_mag_ops2, .Generic, e1, e2))

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
