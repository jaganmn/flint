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
              as.vector(.Call(R_flint_acb_vector, x, "down"), mode))

setMethod("length",
          c(x = "nacb"),
          function (x) length(x@real@mid))

setAs("nacb", "flint",
      function (from)
          new("acb",
              real.mid = from@real@mid, real.rad = from@real@rad,
              imag.mid = from@imag@mid, imag.rad = from@imag@rad))

setAs("acb", "nflint",
      function (from)
          .Call(R_flint_acb_nflint, from, "down"))
