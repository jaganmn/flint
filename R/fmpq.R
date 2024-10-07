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

setAs("vector", "fmpq",
      function (from)
          new("fmpq", x = from))

setAs("nfmpq", "fmpq",
      function (from)
          new("fmpq", num = from@num, den = from@den))

setAs("fmpq", "nfmpq",
      function (from)
          .Call(R_flint_fmpq_nfmpq, from))

Num <- function (q) .Call(R_flint_fmpq_num, q)
Den <- function (q) .Call(R_flint_fmpq_den, q)

if (FALSE) {
setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...)
              paste0("(",
                     format(Num(x), base = base, ...),
                     "/",
                     format(Den(x), base = base, ...),
                     ")"))
} else {
setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...)
              .Call(R_flint_fmpq_format, x, base))
}
