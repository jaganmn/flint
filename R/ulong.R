setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_ulong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "ulong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_ulong_vector, x), mode))

setAs("vector", "ulong",
      function (from)
          new("ulong", x = from))

setAs("nulong", "ulong",
      function (from)
          new("ulong", x = from))

setAs("ulong", "nulong",
      function (from)
          .Call(R_flint_ulong_nulong, from))

setMethod("format",
          c(x = "ulong"),
          function (x, base = 10L, ...)
              .Call(R_flint_ulong_format, x, base))

setMethod("+",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              +as(e1, "fmpz"))

setMethod("-",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              -as(e1, "fmpz"))

setMethod("Ops",
          c(e1 = "ulong", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(as(e1, "fmpz"), as(e2, "fmpz")))

setMethod("Math",
          c(x = "ulong"),
          function (x)
              get(.Generic, mode = "function")(as(x, "fmpz")))

setMethod("Math2",
          c(x = "ulong"),
          function (x, digits) {
              if (missing(digits))
                  get(.Generic, mode = "function")(as(x, "fmpz"))
              else get(.Generic, mode = "function")(as(x, "fmpz"), digits = digits)
          })

setMethod("Summary",
          c(x = "ulong"),
          function (x, ..., na.rm = FALSE)
              get(.Generic, mode = "function")(as(x, "fmpz"), ..., na.rm = na.rm))

setMethod("Complex",
          c(z = "ulong"),
          function (z)
              get(.Generic, mode = "function")(as(z, "fmpz")))
