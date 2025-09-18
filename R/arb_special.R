arb_lambertw <-
function (x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_lambertw, res, as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}

## arb_zeta <-
arb_dirichlet_zeta <-
function (s, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_dirichlet_zeta, res, as(s, "arb"), as(prec, "slong"))
    res
}

## arb_hurwitz_zeta <-
arb_dirichlet_hurwitz <-
function (s, a = 1, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_dirichlet_hurwitz, res, as(s, "arb"), as(a, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_bessel_j <-
function (nu, x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_bessel_j, res, as(nu, "arb"), as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_bessel_y <-
function (nu, x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_bessel_y, res, as(nu, "arb"), as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_bessel_i <-
function (nu, x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_bessel_i, res, as(nu, "arb"), as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_bessel_k <-
function (nu, x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_bessel_k, res, as(nu, "arb"), as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_gamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma, res, as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_rgamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_rgamma, res, as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_lgamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_lgamma, res, as(x, "arb"), as(prec, "slong"))
    res
}

arb_hypgeom_gamma_lower <-
function (s, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma_lower, res, as(s, "arb"), as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}

arb_hypgeom_gamma_upper <-
function (s, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma_upper, res, as(s, "arb"), as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}

arb_hypgeom_beta <-
function (a, b, prec = flintPrec()) {
    if (FALSE) {
    ## FLINT bug, report this
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_beta_lower, res, as(a, "arb"), as(b, "arb"), as(1, "arb"), 0L, as(prec, "slong"))
    res
    }
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    arb_hypgeom_gamma(a) * arb_hypgeom_gamma(b) * arb_hypgeom_rgamma(a + b)
}

arb_hypgeom_beta_lower <-
function (a, b, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_beta_lower, res, as(a, "arb"), as(b, "arb"), as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}

arb_hypgeom_2f1 <-
function (a, b, c, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_2f1, res, as(a, "arb"), as(b, "arb"), as(c, "arb"), as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}
