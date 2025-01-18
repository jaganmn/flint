setMethod("!",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "!", x, NULL))

setMethod("+",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "+", e1, NULL))

setMethod("-",
          c(e1 = "arb", e2 = "missing"),
          function (e1, e2)
              .Call(R_flint_arb_ops1, "-", e1, NULL))

setMethod("Complex",
          c(z = "arb"),
          function (z)
              .Call(R_flint_arb_ops1, .Generic, z, NULL))

setMethod("Math",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, .Generic, x, NULL))

setMethod("Math2",
          c(x = "arb"),
          function (x, digits) {
              if (missing(digits))
                  digits <- switch(.Generic, "round" = 0L, "signif" = 6L)
              .Call(R_flint_arb_ops1, .Generic, x, list(as(digits, "slong")))
          })

setMethod("Mid",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 0L))

setMethod("Mid<-",
          c(x = "arb"),
          function (x, value) {
              ans <- .arb(mid = value, rad = Rad(x))
              if (!is.null(nms <- names(x)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
              ans
          })

setMethod("Ops",
          c(e1 = "ANY", e2 = "arb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(.arb(x = e1), e2),
                     "complex" =
                         g(.acb(x = e1), .acb(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   if (isS4(e1)) class(e1) else typeof(e1), .Generic, "arb"),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "ANY"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e2),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(e1, .arb(x = e2)),
                     "complex" =
                         g(.acb(x = e1), .acb(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, .arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), .acb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arb"),
          function (e1, e2)
              .Call(R_flint_arb_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "arb", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(.acb(x = e1), e2))

setMethod("Summary",
          c(x = "arb"),
          function (x, ..., na.rm = FALSE) {
              if (...length())
                  get(.Generic, mode = "function")(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_arb_ops1, .Generic, x, list(as.logical(na.rm)))
          })

setMethod("Rad",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 1L))

setMethod("Rad<-",
          c(x = "arb"),
          function (x, value) {
              ans <- .arb(mid = Mid(x), rad = value)
              if (!is.null(nms <- names(x)) && (n <- length(ans)) <= 0x1p+52)
                  names(ans) <- if (length(nms) == n) nms else rep_len(nms, n)
              ans
          })

setMethod("anyNA",
          c(x = "arb"),
          function (x, recursive = FALSE)
              .Call(R_flint_arb_ops1, "anyNA", x, list(FALSE)))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              switch(mode,
                     "pairlist" =, "list" =, "expression" =
                         .Call(R_flint_list, x, mode),
                     "symbol" =, "name" =, "character" =
                         as.vector(format(x, digits = 15L, rnd = "N"), mode),
                     as.vector(.Call(R_flint_arb_atomic, x), mode)))

setAs("ANY", "arb",
      function (from)
          .arb(x = from))

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              `names<-`(paste0("(",
                               format(Mid(x), base = base, digits = digits, sep = sep, rnd = rnd, ...),
                               " +/- ",
                               format(Rad(x), base = base, digits = digits, sep = sep, rnd = "A", ...),
                               ")"),
                        names(x)))

setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = NULL, x = NULL, mid, rad, ...)
              .Call(R_flint_arb_initialize, .Object, length, x,
                    if (!missing(mid)) as(mid, "arf"),
                    if (!missing(rad)) as(rad, "mag")))

setMethod("is.finite",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.finite", x, NULL))

setMethod("is.infinite",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.infinite", x, NULL))

setMethod("is.na",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.na", x, NULL))

setMethod("is.nan",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_ops1, "is.nan", x, NULL))

setMethod("is.unsorted",
          c(x = "arb"),
          function (x, na.rm = FALSE, strictly = FALSE)
              stop(.error.notTotalOrder()))

setMethod("log",
          c(x = "arb"),
          function (x, base, ...)
              .Call(R_flint_arb_ops1, "log", x,
                    if (!missing(base)) list(as(base, "arb"))))

setMethod("mean",
          c(x = "arb"),
          function (x, na.rm = FALSE, ...) {
              if (...length())
                  mean(c(x, ...), na.rm = na.rm)
              else .Call(R_flint_arb_ops1, "mean", x, list(as.logical(na.rm)))
          })

setMethod("xtfrm",
          c(x = "arb"),
          function (x)
              stop(.error.notTotalOrder()))
