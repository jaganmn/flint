setMethod("initialize",
          c(.Object = "fmpz", x = "numeric"),
          function (.Object, x, ...)
              .Call(R_flint_fmpz_initialize, .Object, x))

setAs("numeric", "fmpz",
      function (from) new("fmpz", x = from))

setAs("fmpz", "integer",
      function (from) .Call(R_flint_fmpz_integer, from))

setAs("fmpz", "double",
      function (from) .Call(R_flint_fmpz_double, from))
