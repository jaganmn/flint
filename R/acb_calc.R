acb_integrate <-
function (func, a, b, param = NULL, rtol = NULL, atol = NULL,
          control = NULL, prec = flintPrec()) {
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    res <- flintNew("acb")
    .Call(R_flint_acb_calc_integrate, res,
          .integrate.func(func), param, as(a, "acb"), as(b, "acb"),
          if (!is.null(rtol)) as(-floor(log2(as(rtol, "mag"))), "slong"),
          if (!is.null(atol)) as(atol, "mag"),
          if (!is.null(control)) do.call(.integrate.control, control),
          prec)
    res
}
