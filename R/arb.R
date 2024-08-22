setMethod("initialize",
          c(.Object = "arb", x = "numeric"),
          function(.Object, x, ...)
              .Call(R_flint_arb_initialize, .Object, x))
