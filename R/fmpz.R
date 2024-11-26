setMethod("initialize",
          c(.Object = "fmpz"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_fmpz_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "fmpz"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpz_vector, x), mode))

setAs("vector", "fmpz",
      function (from)
          new("fmpz", x = from))

setAs("nfmpz", "fmpz",
      function (from)
          new("fmpz", x = from))

setAs("fmpz", "nfmpz",
      function (from)
          .Call(R_flint_fmpz_nfmpz, from))

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
          c(e1 = "fmpz", e2 = "fmpz"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops2, .Generic, e1, e2))

setMethod("Math",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpz"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              digits <- as(digits, "slong")
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
