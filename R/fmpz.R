setMethod("initialize",
          c(.Object = "fmpz", x = "numeric"),
          function (.Object, x, ...)
              .Call(R_flint_fmpz_initialize, .Object, x))

setAs("numeric", "fmpz",
      function (from) new("fmpz", x = from))
