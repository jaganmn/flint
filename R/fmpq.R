setMethod("initialize",
          c(.Object = "fmpq", p = "numeric", q = "numeric"),
          function (.Object, p, q, ...)
              .Call(R_flint_fmpq_initialize, .Object, p, q))

setAs("numeric", "fmpq",
      function (from) new("fmpq", p = from, q = 1L))
