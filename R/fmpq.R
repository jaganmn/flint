setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, length = 0L, x = NULL, num, den, ...)
              .Call(R_flint_fmpq_initialize, .Object, length, x,
                    if (!missing(num)) as(num, "fmpz"),
                    if (!missing(den)) as(den, "fmpz")))

setMethod("as.vector",
          c(x = "fmpq"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpq_vector, x), mode))

setMethod("length",
          c(x = "nfmpq"),
          function (x) length(x@num))

setAs("ANY", "fmpq",
      function (from)
          new("fmpq", x = from))

setAs("nfmpq", "fmpq",
      function (from)
          new("fmpq", num = from@num, den = from@den))

setAs("fmpq", "nfmpq",
      function (from)
          .Call(R_flint_fmpq_nfmpq, from))

Num <- function (q) .Call(R_flint_part, q, 0L)
Den <- function (q) .Call(R_flint_part, q, 1L)

setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...)
              paste0("(",
                     format(Num(x), base = base, ...),
                     "/",
                     format(Den(x), base = base, ...),
                     ")"))

setMethod("+",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "-", e1, NULL))

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpq"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpq", x = e1), e2),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "fmpq"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(e1, new("fmpq", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpq", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("fmpq", x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("fmpq", x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, new("fmpq", x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpq"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Math",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpq"),
          function (x, digits) {
              if (missing(digits))
                  digits <- as(switch(.Generic, "round" = 0L, "signif" = 6L), "slong")
              else if (length(digits) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "digits", .Generic),
                       domain = NA)
              else digits <- as(digits, "slong")
              .Call(R_flint_fmpq_ops1, .Generic, x, digits)
          })

setMethod("Summary",
          c(x = "fmpq"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("Complex",
          c(z = "fmpq"),
          function (z)
              .Call(R_flint_fmpq_ops1, .Generic, z, NULL))
