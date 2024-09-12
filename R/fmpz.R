setMethod("initialize",
          c(.Object = "fmpz"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_fmpz_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "fmpz"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpz_vector, x), mode))

setAs("nfmpz", "flint",
      function (from)
          new("fmpz", x = from))

setAs("fmpz", "nflint",
      function (from)
          .Call(R_flint_fmpz_nflint, from))
