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
          function (q, value) {
              nq <- length(q)
              nv <- length(value)
              if (nv != 1L && nv != nq)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "q", nq),
                       domain = NA)
              ans <- .fmpq(num = Num(q), den = value)
              ans@dim <- q@dim
              ans@dimnames <- q@dimnames
              ans@names <- q@names
              ans
          })

setMethod("Math",
          c(x = "fmpq"),
          function (x)
              .Call(R_flint_fmpq_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "fmpq"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_fmpq_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Num",
          c(q = "fmpq"),
          function (q)
              .Call(R_flint_fmpq_part, q, 0L))

setMethod("Num<-",
          c(q = "fmpq"),
          function (q, value) {
              nq <- length(q)
              nv <- length(value)
              if (nv != 1L && nv != nq)
                  stop(gettextf("length of '%s' [%.0f] is not equal to 1 or length of '%s' [%.0f]",
                                "value", nv, "q", nq),
                       domain = NA)
              ans <- .fmpq(num = value, den = Den(q))
              ans@dim <- q@dim
              ans@dimnames <- q@dimnames
              ans@names <- q@names
              ans
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "fmpq"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =
                         g(.fmpq(e1), e2),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
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
                         g(e1, .fmpq(e2)),
                     "double" =
                         g(.arf(e1), .arf(e2)),
                     "complex" =
                         g(.acf(e1), .acf(e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "fmpq", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "fmpq", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .fmpq(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "fmpq"),
          function (e1, e2)
              .Call(R_flint_fmpq_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), .arf(e2)))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acf(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "arb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.arb(e1), e2))

setMethod("Ops",
          c(e1 = "fmpq", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(e1), e2))

setMethod("Summary",
          c(x = "fmpq"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpq_ops1, .Generic, x, NULL)
          })

setMethod("anyNA",
          c(x = "fmpq"),
          function (x, recursive = FALSE)
              FALSE)

setMethod("as.vector",
          c(x = "fmpq"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x), mode),
                     as.vector(.Call(R_flint_fmpq_atomic, x), mode)))

setAs("ANY", "fmpq",
      function (from)
          new("fmpq", x = from, length = NULL,
              dim = dim(from), dimnames = dimnames(from),
              names = names(from), num = NULL, den = NULL))

setMethod("colMeans",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "colMeans", x, list(NULL, as.integer(dims))))

setMethod("colSums",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "colSums", x, list(NULL, as.integer(dims))))

setMethod("format",
          c(x = "fmpq"),
          function (x, base = 10L, ...) {
              p <- format(Num(x), base = base, ...)
              q <- format(Den(x), base = base, ...)
              p[] <- paste0(p, "/", q)
              p
          })

setMethod("initialize",
          c(.Object = "fmpq"),
          function (.Object, x, length, dim, dimnames, names,
                    num, den, ...)
              .Call(R_flint_fmpq_initialize,
                    .Object, x, length, dim, dimnames, names,
                    num, den))

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

setMethod("is.unsorted",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, strictly = FALSE)
              .Call(R_flint_fmpq_ops1, "is.unsorted", x, list(NULL, as.logical(strictly))))

setMethod("mean",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_fmpq_ops1, "mean", x, NULL)
          })

setMethod("rowMeans",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "rowMeans", x, list(NULL, as.integer(dims))))

setMethod("rowSums",
          c(x = "fmpq"),
          function (x, na.rm = FALSE, dims = 1, ...)
              .Call(R_flint_fmpq_ops1, "rowSums", x, list(NULL, as.integer(dims))))
