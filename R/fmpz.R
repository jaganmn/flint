setMethod("!",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpz", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "fmpz"),
          function (z)
              .Call(R_flint_fmpz_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpz"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_fmpz_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpz"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpz(x = e1), e2),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "fmpz"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpz", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(e1, .fmpz(x = e2)),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpz", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpz", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpz(x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpz(x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpz"),
          function (e1, e2)
              .Call(R_flint_fmpz_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpq(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), .arf(x = e2)))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpz", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), e2))

setMethod("Summary",
          c(x = "fmpz"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpz_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "fmpz"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "fmpz"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_fmpz_atomic, x), mode)))

setAs("ANY", "fmpz",
      function (from)
          .fmpz(x = from))

setMethod("format",
          c(x = "fmpz"),
          function (x, base = 10L, ...)
              .Call(R_flint_fmpz_format, x, base))

setMethod("initialize",
          c(.Object = "fmpz"),
          function (.Object, length = NULL, x = NULL, ...)
              .Call(R_flint_fmpz_initialize, .Object, length, x))

setMethod("is.finite",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "fmpz"),
          function (x)
              .Call(R_flint_fmpz_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_fmpz_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMethod("mean",
          c(x = "fmpz"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpz_ops1, "mean", x, NULL)
          })
