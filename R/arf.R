setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arf_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arf_vector, x, "Z"), mode))

setAs("vector", "arf",
      function (from)
          new("arf", x = from))

setAs("narf", "arf",
      function (from)
          new("arf", x = from))

setAs("arf", "narf",
      function (from)
          .Call(R_flint_arf_narf, from, "Z"))
