setMethod("initialize",
          c(.Object = "fmpz"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_fmpz_initialize, .Object, length, x))

setAs("numeric", "fmpz",
      function (from) new("fmpz", x = from))

setAs("fmpz", "nfmpz",
      function (from) .Call(R_flint_fmpz_nfmpz, from))

setAs("fmpz", "double",
      function (from) .Call(R_flint_fmpz_double, from))
