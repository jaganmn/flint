setMethod("!",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arf", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arf_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "arf"),
          function (z)
              .Call(R_flint_arf_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arf"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_arf_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "arf"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(.arf(e1), e2),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arf"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(e1, .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arf", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arf", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arf(e2)))

setMethod("Ops",
          c(e1 = "arf", e2 = "arf"),
          function (e1, e2)
              .Call(R_flint_arf_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(e1), e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(e1), e2))

setMethod("Ops",
          c(e1 = "arf", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(e1), e2))

setMethod("Summary",
          c(x = "arf"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = FALSE)
              else .Call(R_flint_arf_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("anyNA",
          c(x = "arf"),
          function (x, recursive = FALSE)
              .Call(R_flint_arf_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, rnd = "N"), mode),
                     as.vector(.Call(R_flint_arf_atomic, x), mode)))

setAs("ANY", "arf",
      function (from)
          new("arf", x = from, length = NULL,
              dim = dim(from), dimnames = dimnames(from),
              names = names(from)))

setMethod("colMeans",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "colMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("colSums",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "colSums", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("format",
          c(x = "arf"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "@"
              .Call(R_flint_arf_format, x, base, digits, sep, rnd)
          })

setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, x, length, dim, dimnames, names, ...)
              .Call(R_flint_arf_initialize,
                    .Object, x, length, dim, dimnames, names))

setMethod("is.finite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "arf"),
          function (x)
              .Call(R_flint_arf_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "arf"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_arf_ops1, "is.unsorted", x, list(as.logical(na.rm), as.logical(strictly))))

setMethod("mean",
          c(x = "arf"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = FALSE)
              else .Call(R_flint_arf_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("rowMeans",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "rowMeans", x, list(as.logical(na.rm), as.integer(dims))))

setMethod("rowSums",
          c(x = "arf"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_arf_ops1, "rowSums", x, list(as.logical(na.rm), as.integer(dims))))
