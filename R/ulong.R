setMethod("!",
          c(x = "ulong"),
          function (x)
              !.fmpz(x = x))

setMethod("+",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              +.fmpz(x = e1))

setMethod("-",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              -.fmpz(x = e1))

setMethod("Complex",
          c(z = "ulong"),
          function (z)
              get(.Generic, mode = "function")(.fmpz(x = z)))

setMethod("Math",
          c(x = "ulong"),
          function (x)
              get(.Generic, mode = "function")(.fmpz(x = x)))

setMethod("Math2",
          c(x = "ulong"),
          function (x, digits) {
              g <- get(.Generic, mode = "function")
              if (missing(digits))
                  g(.fmpz(x = x))
              else g(.fmpz(x = x), digits = digits)
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "ulong"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpz(x = e1), .fmpz(x = e2)),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
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
                         g(.fmpz(x = e1), .fmpz(x = e2)),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "ulong", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "ulong", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpz(x = e1), .fmpz(x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpz(x = e1), .fmpz(x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpz(x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpq(x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), .arf(x = e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(x = e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), e2))

setMethod("Summary",
          c(x = "ulong"),
          function (x, ..., na.rm = FALSE)
              get(.Generic, mode = "function")(.fmpz(x = x), ..., na.rm = na.rm))

setMethod("anyNA",
          c(x = "ulong"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "ulong"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_ulong_vector, x), mode))

setAs("ANY", "ulong",
      function (from)
          .ulong(x = from))

setMethod("format",
          c(x = "ulong"),
          function (x, base = 10L, ...)
              .Call(R_flint_ulong_format, x, base))

setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, length = NULL, x = NULL, ...)
              .Call(R_flint_ulong_initialize, .Object, length, x))

setMethod("is.finite",
          c(x = "ulong"),
          function (x)
              rep.int(TRUE, length(x)))

setMethod("is.infinite",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.na",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.nan",
          c(x = "ulong"),
          function (x)
              logical(length(x)))

setMethod("is.unsorted",
          c(x = "ulong"),
          function (x, na.rm = FALSE, strictly = FALSE)
              is.unsorted(.fmpz(x = x), na.rm = na.rm, strictly = strictly))

setMethod("mean",
          c(x = "ulong"),
          function (x, ...)
              mean(.fmpz(x = x), ...))
