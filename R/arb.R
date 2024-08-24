setMethod("initialize",
          c(.Object = "arb", x = "numeric"),
          function (.Object, x, ...)
              .Call(R_flint_arb_initialize, .Object, x))

setAs("numeric", "arb",
      function (from) new("arb", x = from))
