setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, length = 0L, x = NULL, num = NULL, den = NULL, ...)
              .Call(R_flint_fmpq_initialize, .Object, length, x, num, den))

setAs("numeric", "fmpq",
      function (from) new("fmpq", x = from))

setAs("fmpq", "nfmpq",
      function (from) .Call(R_flint_fmpq_nfmpq, from))

setAs("fmpq", "double",
      function (from) .Call(R_flint_fmpq_double, from))
