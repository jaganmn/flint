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
                    rnd = flintRnd(), ...)
              paste0(format(Real(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "+",
                     format(Imag(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     "i"))

setMethod("+",
          c(e1 = "acb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acb_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "acb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_acb_ops1, "-", e1, NULL))

setMethod("log",
          c(x = "acb"),
          function (x, base, ...) {
              if (missing(base))
                  base <- NULL
              else if (length(base) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "base", "log"),
                       domain = NA)
              else base <- as(base, "acb")
              .Call(R_flint_acb_ops1, "log", x, base)
          })

setMethod("Ops",
          c(e1 = "acb", e2 = "acb"),
          function (e1, e2)
              .Call(R_flint_acb_ops2, .Generic, e1, e2))

setMethod("Math",
          c(x = "acb"),
          function (x)
              .Call(R_flint_acb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "acb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_acb_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "acb"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_acb_ops1, .Generic, x, NULL))

setMethod("Complex",
          c(z = "acb"),
          function (z)
              .Call(R_flint_acb_ops1, .Generic, z, NULL))
