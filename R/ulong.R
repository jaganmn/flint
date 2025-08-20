setMethod("!",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_ulong_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "ulong", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_ulong_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "ulong"),
          function (z)
              .Call(R_flint_ulong_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "ulong"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_ulong_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "ulong"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =
                         g(.ulong(e1), e2),
                     "logical" =, "integer" =
                         g(.fmpz(e1), .fmpz(e2)),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "ulong"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "ulong", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =
                         g(e1, .ulong(e2)),
                     "logical" =, "integer" =
                         g(.fmpz(e1), .fmpz(e2)),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "ulong", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "ulong", e2 = "ulong"),
          function (e1, e2)
              .Call(R_flint_ulong_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpz(e1), .fmpz(e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpz(e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(.fmpq(e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), .arf(e2)))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(e1), e2))

setMethod("Ops",
          c(e1 = "ulong", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(e1), e2))

setMethod("Summary",
          c(x = "ulong"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_ulong_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "ulong"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "ulong"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_ulong_atomic, x), mode)))

setAs("ANY", "ulong",
      function (from)
          new("ulong", x = from, length = NULL,
              dim = dim(from), dimnames = dimnames(from),
              names = names(from)))

setMethod("colMeans",
          c(x = "ulong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_ulong_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "ulong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_ulong_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("format",
          c(x = "ulong"),
          function (x, base = 10L, ...)
              .Call(R_flint_ulong_format, x, base))

setMethod("initialize",
          c(.Object = "ulong"),
          function (.Object, x, length, dim, dimnames, names, ...)
              .Call(R_flint_ulong_initialize,
                    .Object, x, length, dim, dimnames, names))

setMethod("is.finite",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "ulong"),
          function (x)
              .Call(R_flint_ulong_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "ulong"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_ulong_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMethod("mean",
          c(x = "ulong"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_ulong_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "ulong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_ulong_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "ulong"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_ulong_ops1, "rowSums", x, list(NULL, as.integer(dims))))
