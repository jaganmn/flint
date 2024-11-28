setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arf_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arf_vector, x, "N"), mode))

setAs("vector", "arf",
      function (from)
          new("arf", x = from))

setAs("narf", "arf",
      function (from)
          new("arf", x = from))

setAs("arf", "narf",
      function (from)
          .Call(R_flint_arf_narf, from, "N"))

setMethod("format",
          c(x = "arf"),
          function (x, base = 10L, digits = NULL, sep = NULL, rnd = "N", ...) {
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
          c(e1 = "arf", e2 = "arf"),
          function (e1, e2)
              .Call(R_flint_arf_ops2, .Generic, e1, e2))

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
