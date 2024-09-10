setMethod("initialize",
          c(.Object = "acb"),
          function (.Object, length = 0L, x = NULL, real = NULL, imag = NULL, ...)
              .Call(R_flint_acb_initialize, .Object, length, x, real, imag))

setAs("numeric", "acb",
      function (from) new("acb", x = from))

setAs("complex", "acb",
      function (from) new("acb", x = from))

setAs("acb", "nacb",
      function (from) .Call(R_flint_acb_nacb, from, "down"))
