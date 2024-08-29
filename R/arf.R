setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, x, ...)
              .Call(R_flint_arf_initialize, .Object, x))

setAs("numeric", "arf",
      function (from) new("arf", x = from))

setAs("arf", "double",
      function (from) .Call(R_flint_arf_double, from))
