setMethod("initialize",
          c(.Object = "mag"),
          function (.Object, length = 0L, x = NULL, ...)
              .Call(R_flint_mag_initialize, .Object, length, x))

setMethod("as.vector",
          c(x = "mag"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_mag_vector, x), mode))

setAs("vector", "mag",
      function (from)
          new("mag", x = from))

setAs("nmag", "mag",
      function (from)
          new("mag", x = from))

setAs("mag", "nmag",
      function (from)
          .Call(R_flint_mag_nmag, from))

setMethod("format",
          c(x = "mag"),
          function (x, base = 10L, digits = NULL, sep = NULL, rnd = "A", ...) {
              if (is.null(digits))
                  digits <- getOption("digits")
              if (is.null(sep))
                  sep <- if (identical(base, 10L)) "e" else "|"
              .Call(R_flint_mag_format, x, base, digits, sep, rnd)
          })
