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
              as.vector(.Call(R_flint_acb_vector, x, "Z"), mode))

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
          .Call(R_flint_acb_nacb, from, "Z"))

setMethod("Re",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_real, z))

setMethod("Im",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_imag, z))

setMethod("format",
          c(x = "acb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = c("N", "A"), ...)
              paste0(format(Re(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "+",
                     format(Im(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "i"))
