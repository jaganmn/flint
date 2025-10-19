arb_integrate <-
function (func, a, b, param = NULL, rel.tol = NULL, abs.tol = NULL,
          options = NULL, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_calc_integrate, res,
          .integrate.func(func), param, as(a, "arb"), as(b, "arb"),
          if (!is.null(rel.tol)) as(-floor(log2(rel.tol)), "slong"),
          if (!is.null(abs.tol)) as(abs.tol, "mag"),
          if (!is.null(options)) do.call(.integrate.options, options),
          as(prec, "slong"))
    res
}
