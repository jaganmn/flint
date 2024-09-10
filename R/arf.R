setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arf_initialize, .Object, length, x))

setAs("numeric", "arf",
      function (from) new("arf", x = from))

setAs("arf", "narf",
      function (from) .Call(R_flint_arf_narf, from, "down"))
