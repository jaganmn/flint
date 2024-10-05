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
