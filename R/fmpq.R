setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, length = 0L, x = NULL, num = NULL, den = NULL, ...)
              .Call(R_flint_fmpq_initialize, .Object, length, x, num, den))

setMethod("as.vector",
          c(x = "fmpq"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpq_vector, x), mode))

setMethod("length",
          c(x = "nfmpq"),
          function (x) length(x@num))

setAs("nfmpq", "fmpq",
      function (from)
          new("fmpq", num = from@num, den = from@den))

setAs("fmpq", "nfmpq",
      function (from)
          .Call(R_flint_fmpq_nflint, from))
