setMethod("initialize",
          c(.Object = "mag", x = "numeric"),
          function (.Object, x, ...)
              .Call(R_flint_mag_initialize, .Object, x))
