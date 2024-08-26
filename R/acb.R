setMethod("initialize",
          c(.Object = "acb", r = "numeric", i = "numeric"),
          function (.Object, r, i, ...)
              .Call(R_flint_acb_initialize, .Object, r, i))

setAs("numeric", "acb",
      function (from) new("acb", r = from, i = 0))

setAs("complex", "acb",
      function (from) new("acb", r = Re(from), i = Im(from)))

setAs("acb", "list",
      function (from) .Call(R_flint_acb_list, from))
