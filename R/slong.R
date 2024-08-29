setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, x = integer(0L), ...)
              .Call(R_flint_slong_initialize, .Object, x))

setAs("numeric", "slong",
      function (from) new("slong", x = from))

setAs("slong", "integer",
      function (from) .Call(R_flint_slong_integer, from))

setAs("slong", "double",
      function (from) .Call(R_flint_slong_double, from))
