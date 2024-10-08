setMethod("initialize",
          c(.Object = "arf"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_arf_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "arf"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arf_vector, x, "N"), mode))

setAs("vector", "arf",
      function (from)
          new("arf", x = from))

setAs("narf", "arf",
      function (from)
          new("arf", x = from))

setAs("arf", "narf",
      function (from)
          .Call(R_flint_arf_narf, from, "N"))

setMethod("format",
          c(x = "arf"),
          function (x, base = 10L, digits = NULL, sep = NULL, rnd = "N", ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "|"
              .Call(R_flint_arf_format, x, base, digits, sep, rnd)
          })
