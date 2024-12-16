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
              .Call(R_flint_arb_ops1, .Generic, x, as(digits, "slong"))
          })

setMethod("Mid",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 0L))

setMethod("Mid<-",
          c(x = "arb"),
          function (x, value)
              arb(mid = value, rad = Rad(x)))

setMethod("Ops",
          c(e1 = "ANY", e2 = "arb"),
          function (e1, e2) {
              g <- get(.Generic, mode = "function")
              switch(typeof(e1),
                     "NULL" =, "raw" =, "logical" =, "integer" =, "double" =
                         g(arb(x = e1), e2),
                     "complex" =
                         g(acb(x = e1), acb(x = e2)),
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
                         g(e1, arb(x = e2)),
                     "complex" =
                         g(acb(x = e1), acb(x = e2)),
                     stop(gettextf("<%s> %s <%s> is not yet implemented",
                                   "arb", .Generic, if (isS4(e2)) class(e2) else typeof(e2)),
                          domain = NA))
          })

setMethod("Ops",
          c(e1 = "arb", e2 = "slong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "ulong"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpz"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "fmpq"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "mag"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arf"),
          function (e1, e2)
              get(.Generic, mode = "function")(e1, arb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "acf"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(x = e1), acb(x = e2)))

setMethod("Ops",
          c(e1 = "arb", e2 = "arb"),
          function (e1, e2)
              .Call(R_flint_arb_ops2, .Generic, e1, e2))

setMethod("Ops",
          c(e1 = "arb", e2 = "acb"),
          function (e1, e2)
              get(.Generic, mode = "function")(acb(x = e1), e2))

setMethod("Summary",
          c(x = "arb"),
          function (x, ..., na.rm = FALSE)
              .Call(R_flint_arb_ops1, .Generic, x, as.logical(na.rm)))

setMethod("Rad",
          c(x = "arb"),
          function (x)
              .Call(R_flint_arb_part, x, 1L))

setMethod("Rad<-",
          c(x = "arb"),
          function (x, value)
              arb(mid = Mid(x), rad = value))

setMethod("all.equal",
          c(target = "arb", current = "arb"),
          function (target, current, ...)
              all.equal(list(mid = Mid(target),
                             rad = Rad(target)),
                        list(mid = Mid(current),
                             rad = Rad(current)),
                        ...))

setMethod("anyNA",
          c(x = "arb"),
          function (x, recursive = FALSE)
              .Call(R_flint_arb_ops1, "anyNA", x, NULL))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arb_vector, x), mode))

setAs("ANY", "arb",
      function (from)
          arb(x = from))

setMethod("format",
          c(x = "arb"),
          function (x, base = 10L, digits = NULL, sep = NULL,
                    rnd = flintRnd(), ...)
              paste0("(",
                     format(Mid(x), base = base, digits = digits, sep = sep,
                            rnd = rnd, ...),
                     " +/- ",
                     format(Rad(x), base = base, digits = digits, sep = sep,
                                       ...),
                     ")"))

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

setMethod("log",
          c(x = "arb"),
          function (x, base, ...) {
              if (missing(base))
                  base <- NULL
              else if (length(base) == 0L)
                  stop(gettextf("'%s' of length zero in '%s'",
                                "base", "log"),
                       domain = NA)
              else base <- as(base, "arb")
              .Call(R_flint_arb_ops1, "log", x, base)
          })

setMethod("mean",
          c(x = "arb"),
          function (x, na.rm = FALSE, ...)
              .Call(R_flint_arb_ops1, "mean", x, as.logical(na.rm)))
