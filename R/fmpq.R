setMethod("initialize",
          c(.Object = "fmpq", p = "numeric", q = "numeric"),
          function (.Object, p, q, ...)
              .Call(R_flint_fmpq_initialize, .Object, p, q))
