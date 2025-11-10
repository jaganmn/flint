.integrate.func <-
function (func) {
    if (!is.function(func) || is.primitive(func))
        stop(gettextf("'%s' is not a closure",
                      "func"),
             domain = NA)
    nf <- length(f <- formals(func))
    if (any((if (nf <= 4L) names(f) else names(f)[seq_len(4L)]) == "..."))
        stop(gettextf("'%s' has '%s' among first four formal arguments",
                      "func", "..."),
             domain = NA)
    if (nf < 4L)
        formals(func) <- c(f, alist(.__1__. =, .__2__. =, .__3__. =, .__4__. =)[seq.int(to = 4L, by = 1L, length.out = 4L - nf)])
    func
}

.integrate.control <-
function (deg.limit, eval.limit, depth.limit, use.heap, verbose) {
    l <- list(if (missing(  deg.limit)) slong(0L) else as(  deg.limit, "slong"),
              if (missing( eval.limit)) slong(0L) else as( eval.limit, "slong"),
              if (missing(depth.limit)) slong(0L) else as(depth.limit, "slong"),
              if (missing(use.heap)) 0L else as.integer(use.heap),
              if (missing( verbose)) 0L else as.integer( verbose))
    stopifnot(all(lengths(l) == 1L))
    l
}

arb_integrate <-
function (func, a, b, param = NULL, rtol = NULL, atol = NULL,
          control = NULL, prec = flintPrec()) {
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    res <- flintNew("arb")
    .Call(R_flint_arb_calc_integrate, res,
          .integrate.func(func), param, as(a, "arb"), as(b, "arb"),
          if (!is.null(rtol)) as(-floor(log2(as(rtol, "mag"))), "slong"),
          if (!is.null(atol)) as(atol, "mag"),
          if (!is.null(control)) do.call(.integrate.control, control),
          prec)
    res
}
