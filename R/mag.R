setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_mag_vector, x), mode))

setAs("vector", "mag",
      function (from)
          new("mag", x = from))

setAs("nmag", "mag",
      function (from)
          new("mag", x = from))

setAs("mag", "nmag",
      function (from)
          .Call(R_flint_mag_nflint, from))
