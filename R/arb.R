setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = 0L, x = NULL, mid = NULL, rad = NULL, ...)
              .Call(R_flint_arb_initialize, .Object, length, x, mid, rad))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arb_vector, x, "Z"), mode))

setMethod("length",
          c(x = "narb"),
          function (x) length(x@mid))

setAs("vector", "arb",
      function (from)
          new("arb", x = from))

setAs("narb", "arb",
      function (from)
          new("arb", mid = from@mid, rad = from@rad))

setAs("arb", "narb",
      function (from)
          .Call(R_flint_arb_narb, from, "Z"))

Mid <- function (x) .Call(R_flint_arb_mid, x)
Rad <- function (x) .Call(R_flint_arb_rad, x)

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = c("N", "A"), ...)
              paste0("(",
                     format(Mid(x), base = base, digits = digits, sep = sep,
                            rnd = rnd[[1L]], ...),
                     " +/- ",
                     format(Rad(x), base = base, digits = digits, sep = sep,
                            rnd = rnd[[2L]], ...),
                     ")"))
