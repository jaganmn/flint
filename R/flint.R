setMethod("length",
          c(x = "flint"),
          function(x) .Call(R_flint_length, x))
