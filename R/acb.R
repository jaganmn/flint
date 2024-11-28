setMethod("initialize",
          c(.Object = "acb"),
          function (.Object, length = 0L, x = NULL,
                    real.mid = NULL, real.rad = NULL,
                    imag.mid = NULL, imag.rad = NULL, ...)
              .Call(R_flint_acb_initialize, .Object, length, x,
                    real.mid, real.rad, imag.mid, imag.rad))

setMethod("as.vector",
          c(x = "acb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_acb_vector, x, "N"), mode))

setMethod("length",
          c(x = "nacb"),
          function (x) length(x@real@mid))

setAs("vector", "acb",
      function (from)
          new("acb", x = from))

setAs("nacb", "acb",
      function (from)
          new("acb",
              real.mid = from@real@mid, real.rad = from@real@rad,
              imag.mid = from@imag@mid, imag.rad = from@imag@rad))

setAs("acb", "nacb",
      function (from)
          .Call(R_flint_acb_nacb, from, "N"))

Real <- function (z) .Call(R_flint_part, z, 0L)
Imag <- function (z) .Call(R_flint_part, z, 1L)

setMethod("format",
          c(x = "acb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = c("N", "U"), ...)
              paste0(format(Real(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "+",
                     format(Imag(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "i"))

setMethod("Complex",
          c(z = "acb"),
          function (z)
              switch(.Generic, "Re" = Real(z), "Im" = Imag(z),
                     stop(gettextf("operation '%s' not yet implemented for class '%s'",
                                   .Generic, "acb"),
                          domain = NA)))
