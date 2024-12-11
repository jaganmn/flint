setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_slong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "slong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_slong_vector, x), mode))

setAs("ANY", "slong",
      function (from)
          new("slong", x = from))

setAs("nslong", "slong",
      function (from)
          new("slong", x = from))

setAs("slong", "nslong",
      function (from)
          .Call(R_flint_slong_nslong, from))

setMethod("format",
          c(x = "slong"),
          function (x, base = 10L, ...)
              .Call(R_flint_slong_format, x, base))

setMethod("+",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              +as(e1, "fmpz"))

setMethod("-",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              -as(e1, "fmpz"))

setMethod("Ops",
          c(e1 = "slong", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(as(e1, "fmpz"), as(e2, "fmpz")))

setMethod("Math",
          c(x = "slong"),
          function (x)
              get(.Generic, mode = "function")(as(x, "fmpz")))

setMethod("Math2",
          c(x = "slong"),
          function (x, digits) {
              if (missing(digits))
                  get(.Generic, mode = "function")(as(x, "fmpz"))
              else get(.Generic, mode = "function")(as(x, "fmpz"), digits = digits)
          })

setMethod("Summary",
          c(x = "slong"),
          function (x, ..., na.rm = FALSE)
              get(.Generic, mode = "function")(as(x, "fmpz"), ..., na.rm = na.rm))

setMethod("Complex",
          c(z = "slong"),
          function (z)
              get(.Generic, mode = "function")(as(z, "fmpz")))
