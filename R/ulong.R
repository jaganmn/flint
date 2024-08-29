setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, x = integer(0L), ...)
              .Call(R_flint_ulong_initialize, .Object, x))

setAs("numeric", "ulong",
      function (from) new("ulong", x = from))

setAs("ulong", "integer",
      function (from) .Call(R_flint_ulong_integer, from))

setAs("ulong", "double",
      function (from) .Call(R_flint_ulong_double, from))
