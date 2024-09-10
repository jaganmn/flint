setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setAs("numeric", "mag",
      function (from) new("mag", x = from))

setAs("mag", "nmag",
      function (from) .Call(R_flint_mag_nmag, from))
