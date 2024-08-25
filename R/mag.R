setMethod("initialize",
          c(.Object = "mag", x = "numeric"),
          function (.Object, x, ...)
              .Call(R_flint_mag_initialize, .Object, x))

setAs("numeric", "mag",
      function (from) new("mag", x = from))

setAs("mag", "integer",
      function (from) .Call(R_flint_mag_integer, from))

setAs("mag", "double",
      function (from) .Call(R_flint_mag_double, from))
