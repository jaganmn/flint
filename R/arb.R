setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = 0L, x = NULL, mid, rad, ...)
              .Call(R_flint_arb_initialize, .Object, length, x,
                    if (!missing(mid)) as(mid, "arf"),
                    if (!missing(rad)) as(rad, "mag")))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arb_vector, x, "N"), mode))

setAs("ANY", "arb",
      function (from)
          new("arb", x = from))

Mid <- function (x) .Call(R_flint_part, x, 0L)
Rad <- function (x) .Call(R_flint_part, x, 1L)

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
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
          c(e1 = "ANY", e2 = "arb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(new("arb", x = e1), e2),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arb"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                         g(e1, new("arb", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("arb", x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arb"),
          function (e1, e2)
              .Call(R_flint_arb_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "arb", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

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
