setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_slong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "slong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_slong_vector, x), mode))

setAs("nslong", "slong",
      function (from)
          new("slong", x = from))

setAs("slong", "nslong",
      function (from)
          .Call(R_flint_slong_nflint, from))

