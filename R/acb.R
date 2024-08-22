setMethod("initialize",
          c(.Object = "acb", r = "numeric", i = "numeric"),
          function(.Object, r, i, ...)
              .Call(R_flint_acb_initialize, .Object, r, i))
