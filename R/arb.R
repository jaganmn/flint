setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = 0L, x = NULL, mid = NULL, rad = NULL, ...)
              .Call(R_flint_arb_initialize, .Object, length, x, mid, rad))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arb_vector, x, "N"), mode))

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
          .Call(R_flint_arb_narb, from, "N"))

Mid <- function (x) .Call(R_flint_part, x, 0L)
Rad <- function (x) .Call(R_flint_part, x, 1L)

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = c("N", "U"), ...)
              paste0("(",
                     format(Mid(x), base = base, digits = digits, sep = sep,
                            rnd = rnd[[1L]], ...),
                     " +/- ",
                     format(Rad(x), base = base, digits = digits, sep = sep,
                            rnd = rnd[[2L]], ...),
                     ")"))

setMethod("+",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "-", e1, NULL))

setMethod("log",
          c(x = "arb"),
          function (x, base, ...) {
              if (missing(base))
                  base <- NULL
              else if (length(base) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "base", "log"),
                       domain = NA)
              else base <- as(base, "arb")
              .Call(R_flint_arb_ops1, "log", x, base)
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "arb"),
          function (e1, e2)
              .Call(R_flint_arb_ops2, .Generic, e1, e2))

setMethod("Math",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_arb_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "arb"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_arb_ops1, .Generic, x, NULL))

setMethod("Complex",
          c(z = "arb"),
          function (z)
              .Call(R_flint_arb_ops1, .Generic, z, NULL))
