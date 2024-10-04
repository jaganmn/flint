setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_slong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "slong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_slong_vector, x), mode))

setAs("vector", "slong",
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
          function (x, ...)
              .Call(R_flint_slong_format, x))
