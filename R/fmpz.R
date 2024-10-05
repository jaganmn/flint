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
