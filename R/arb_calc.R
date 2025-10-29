arb_integrate <-
function (func, a, b, param = NULL, rtol = NULL, atol = NULL,
          control = NULL, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_calc_integrate, res,
          .integrate.func(func), param, as(a, "arb"), as(b, "arb"),
          if (!is.null(rtol)) as(-floor(log2(rtol)), "slong"),
          if (!is.null(atol)) as(atol, "mag"),
          if (!is.null(control)) do.call(.integrate.control, control),
          as(prec, "slong"))
    res
}
