setMethod("initialize",
          c(.Object = "arf", x = "numeric"),
          function(.Object, x, ...)
              .Call(R_flint_arf_initialize, .Object, x))
