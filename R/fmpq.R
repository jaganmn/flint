setMethod("initialize",
          c(.Object = "fmpq", p = "numeric", q = "numeric"),
          function (.Object, p, q, ...)
              .Call(R_flint_fmpq_initialize, .Object, p, q))

setAs("numeric", "fmpq",
      function (from) new("fmpq", p = from, q = 1L))

setAs("fmpq", "integer",
      function (from) .Call(R_flint_fmpq_integer, from))

setAs("fmpq", "double",
      function (from) .Call(R_flint_fmpq_double, from))
