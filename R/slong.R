setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_slong_initialize, .Object, length, x))

setAs("numeric", "slong",
      function (from) new("slong", x = from))

setAs("slong", "nslong",
      function (from) .Call(R_flint_slong_nslong, from))

setAs("slong", "double",
      function (from) .Call(R_flint_slong_double, from))
