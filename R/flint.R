setMethod("as.raw",
          c(x = "flint"),
          function (x     ) as.vector(x, "raw"))

setMethod("as.logical",
          c(x = "flint"),
          function (x, ...) as.vector(x, "logical"))

setMethod("as.integer",
          c(x = "flint"),
          function (x, ...) as.vector(x, "integer"))

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
          function (x) .Call(R_flint_length, x))
