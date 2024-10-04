setMethod("as.raw",
          c(x = "flint"),
          function (x     ) as.vector(x, "raw"))

setMethod("as.logical",
          c(x = "flint"),
          function (x, ...) as.vector(x, "logical"))

setMethod("as.integer",
          c(x = "flint"),
          function (x, ...) as.vector(x, "integer"))

if (FALSE)
setMethod("as.double",
          c(x = "flint"),
          function (x, ...) as.vector(x, "double"))

setMethod("as.numeric",
          c(x = "flint"),
          function (x, ...) as.vector(x, "numeric"))

setMethod("as.complex",
          c(x = "flint"),
          function (x, ...) as.vector(x, "complex"))

setMethod("as.list",
          c(x = "flint"),
          function (x, ...) as.vector(x, "list"))

setMethod("as.expression",
          c(x = "flint"),
          function (x, ...) as.vector(x, "expression"))

setMethod("length",
          c(x = "flint"),
          function (x) flintLength(x))

setMethod("print",
          c(x = "flint"),
          function (x, quote = FALSE, max = NULL, ...) {
              s <- flintTriple(x)
              cat(gettextf("class '%s', address %s, length %s",
                           s[1L], s[2L], s[3L]),
                  "\n", sep = "")
              len <- length(x)
              if (len > 0L) {
                  if (is.null(max))
                      max <- getOption("max.print", 99999L)
                  if (len <= max)
                      print.default(format(x), quote = quote, max = max, ...)
                  else {
                      print.default(format(x[seq_len(max)]), quote = quote, max = max, ...)
                      cat(gettextf(" [ reached '%s' / getOption(\"%s\") -- omitted %f entries ]",
                                   "max", "max.print", len - trunc(max)),
                          "\n", sep = "")
                  }
              }
              invisible(x)
          })

setMethod("show",
          c(object = "flint"),
          function (object) {
              print(object, quote = FALSE)
              invisible(NULL)
          })
