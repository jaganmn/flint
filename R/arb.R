setMethod("initialize",
          c(.Object = "arb"),
          function (.Object, length = 0L, x = NULL, mid = NULL, rad = NULL, ...)
              .Call(R_flint_arb_initialize, .Object, length, x, mid, rad))

setMethod("as.vector",
          c(x = "arb"),
          function (x, mode = "any")
              as.vector(.Call(R_flint_arb_vector, x, "down"), mode))

setMethod("length",
          c(x = "narb"),
          function (x) length(x@mid))

setAs("vector", "arb",
      function (from)
          new("arb", x = from))

setAs("narb", "arb",
      function (from)
          new("arb", mid = from@mid, rad = from@rad))

setAs("arb", "narb",
      function (from)
          .Call(R_flint_arb_narb, from, "down"))
