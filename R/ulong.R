setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_ulong_initialize, .Object, length, x))

setAs("numeric", "ulong",
      function (from) new("ulong", x = from))

setAs("ulong", "nulong",
      function (from) .Call(R_flint_ulong_nulong, from))

setAs("ulong", "double",
      function (from) .Call(R_flint_ulong_double, from))
