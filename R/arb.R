setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arb_initialize, .Object, length, x))

setAs("numeric", "arb",
      function (from) new("arb", x = from))

setAs("arb", "narb",
      function (from) .Call(R_flint_arb_narb, from, "down"))
