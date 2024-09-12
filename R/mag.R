setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_mag_vector, x), mode))

setAs("nmag", "flint",
      function (from)
          new("mag", x = from))

setAs("mag", "nflint",
      function (from)
          .Call(R_flint_mag_nflint, from))
