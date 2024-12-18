setMethod("!",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "fmpq", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "fmpq"),
          function (z)
              .Call(R_flint_fmpq_ops1, .Generic, z, NULL))

setMethod("Den",
          c(q = "fmpq"),
          function (q)
              .Call(R_flint_fmpq_part, q, 1L))

setMethod("Den<-",
          c(q = "fmpq"),
          function (q, value)
              .fmpq(num = Num(q), den = value))

setMethod("Math",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpq"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_fmpq_ops1, .Generic, x, as(digits, "slong"))
          })

setMethod("Num",
          c(q = "fmpq"),
          function (q)
              .Call(R_flint_fmpq_part, q, 0L))

setMethod("Num<-",
          c(q = "fmpq"),
          function (q, value)
              .fmpq(num = value, den = Den(q)))

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpq"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpq(x = e1), e2),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
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
                         g(e1, .fmpq(x = e2)),
                     "double" =
                         g(.arf(x = e1), .arf(x = e2)),
                     "complex" =
                         g(.acf(x = e1), .acf(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpq", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpq"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), .arf(x = e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(x = e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), e2))

setMethod("Summary",
          c(x = "fmpq"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("all.equal",
          c(target = "fmpq", current = "fmpq"),
          function (target, current, ...)
              all.equal(list(num = Num(target),
                             den = Den(target)),
                        list(num = Num(current),
                             den = Den(current)),
                        ...))

setMethod("anyNA",
          c(x = "fmpq"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "fmpq"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_fmpq_vector, x), mode))

setAs("ANY", "fmpq",
      function (from)
          .fmpq(x = from))

setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...)
              paste0(format(Num(x), base = base, ...),
                     "/",
                     format(Den(x), base = base, ...)))

setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, length = NULL, x = NULL, num, den, ...)
              .Call(R_flint_fmpq_initialize, .Object, length, x,
                    if (!missing(num)) as(num, "fmpz"),
                    if (!missing(den)) as(den, "fmpz")))

setMethod("is.finite",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, "is.nan", x, NULL))

setMethod("mean",
          c(x = "fmpq"),
          function (x, ...)
              .Call(R_flint_fmpq_ops1, "mean", x, NULL))
