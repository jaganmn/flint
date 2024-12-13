setMethod("initialize",
          c(.Object = "slong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_slong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "slong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_slong_vector, x), mode))

setAs("ANY", "slong",
      function (from)
          new("slong", x = from))

setMethod("format",
          c(x = "slong"),
          function (x, base = 10L, ...)
              .Call(R_flint_slong_format, x, base))

setMethod("+",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              +as(e1, "fmpz"))

setMethod("-",
          c(e1 = "slong", e2 = "missing"),
          function (e1, e2)
              -as(e1, "fmpz"))

setMethod("Ops",
          c(e1 = "ANY", e2 = "slong"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpz", x = e1), new("fmpz", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "slong"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "slong", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpz", x = e1), new("fmpz", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "slong", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "slong", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "slong", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "slong", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpq", x = e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "slong", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "slong", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Math",
          c(x = "slong"),
          function (x)
              get(.Generic, mode = "function")(as(x, "fmpz")))

setMethod("Math2",
          c(x = "slong"),
          function (x, digits) {
              if (missing(digits))
                  get(.Generic, mode = "function")(as(x, "fmpz"))
              else get(.Generic, mode = "function")(as(x, "fmpz"), digits = digits)
          })

setMethod("Summary",
          c(x = "slong"),
          function (x, ..., na.rm = FALSE)
              get(.Generic, mode = "function")(as(x, "fmpz"), ..., na.rm = na.rm))

setMethod("Complex",
          c(z = "slong"),
          function (z)
              get(.Generic, mode = "function")(as(z, "fmpz")))

setMethod("anyNA",
          c(x = "slong"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("is.na",
          c(x = "slong"),
          function (x)
              logical(length(x)))

setMethod("is.nan",
          c(x = "slong"),
          function (x)
              logical(length(x)))

setMethod("is.infinite",
          c(x = "slong"),
          function (x)
              logical(length(x)))

setMethod("is.finite",
          c(x = "slong"),
          function (x)
              rep.int(TRUE, length(x)))

setMethod("!",
          c(x = "slong"),
          function (x)
              !as(x, "fmpz"))

setMethod("mean",
          c(x = "slong"),
          function (x, ...)
              mean(as(x, "fmpz"), ...))
