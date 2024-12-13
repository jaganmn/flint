setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_ulong_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "ulong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_ulong_vector, x), mode))

setAs("ANY", "ulong",
      function (from)
          new("ulong", x = from))

setMethod("format",
          c(x = "ulong"),
          function (x, base = 10L, ...)
              .Call(R_flint_ulong_format, x, base))

setMethod("+",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              +new("fmpz", x = e1))

setMethod("-",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              -new("fmpz", x = e1))

setMethod("Ops",
          c(e1 = "ANY", e2 = "ulong"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpz", x = e1), new("fmpz", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "ulong"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "ulong", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(new("fmpz", x = e1), new("fmpz", x = e2)),
                     "double" =
                         g(new("arf", x = e1), new("arf", x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "ulong", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "ulong", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), new("fmpz", x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpz", x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("fmpq", x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arf", x = e1), new("arf", x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("arb", x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(new("acb", x = e1), e2))

setMethod("Math",
          c(x = "ulong"),
          function (x)
              get(.Generic, mode = "function")(new("fmpz", x = x)))

setMethod("Math2",
          c(x = "ulong"),
          function (x, digits) {
              g <- get(.Generic, mode = "function")
              if (missing(digits))
                  g(new("fmpz", x = x))
              else g(new("fmpz", x = x), digits = digits)
          })

setMethod("Summary",
          c(x = "ulong"),
          function (x, ..., na.rm = FALSE)
              get(.Generic, mode = "function")(new("fmpz", x = x), ..., na.rm = na.rm))

setMethod("Complex",
          c(z = "ulong"),
          function (z)
              get(.Generic, mode = "function")(new("fmpz", x = z)))

setMethod("anyNA",
          c(x = "ulong"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("is.na",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.nan",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.infinite",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.finite",
          c(x = "ulong"),
          function (x)
              rep.int(TRUE, length(x)))

setMethod("!",
          c(x = "ulong"),
          function (x)
              !new("fmpz", x = x))

setMethod("mean",
          c(x = "ulong"),
          function (x, ...)
              mean(new("fmpz", x = x), ...))
