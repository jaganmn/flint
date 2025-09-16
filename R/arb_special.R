arb_lambertw <-
function (x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_lambertw, res, arb(x), as.integer(flags), slong(prec))
    res
}

## arb_zeta <-
arb_dirichlet_zeta <-
function (s, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_dirichlet_zeta, res, arb(s), slong(prec))
    res
}

## arb_hurwitz_zeta <-
arb_dirichlet_hurwitz <-
function (s, a = 1, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_dirichlet_hurwitz, res, arb(s), arb(a), slong(prec))
    res
}

arb_hypgeom_gamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma, res, arb(x), slong(prec))
    res
}

arb_hypgeom_rgamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_rgamma, res, arb(x), slong(prec))
    res
}

arb_hypgeom_lgamma <-
function (x, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_lgamma, res, arb(x), slong(prec))
    res
}

arb_hypgeom_gamma_lower <-
function (s, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma_lower, res, arb(s), arb(x), as.integer(flags), slong(prec))
    res
}

arb_hypgeom_gamma_upper <-
function (s, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_gamma_upper, res, arb(s), arb(x), as.integer(flags), slong(prec))
    res
}

arb_hypgeom_beta <-
function (a, b, prec = flintPrec()) {
    if (FALSE) {
    ## FLINT bug, report this
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_beta_lower, res, arb(a), arb(b), arb(1), 0L, slong(prec))
    res
    }
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    arb_hypgeom_gamma(a) * arb_hypgeom_gamma(b) * arb_hypgeom_rgamma(a + b)
}

arb_hypgeom_beta_lower <-
function (a, b, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_beta_lower, res, arb(a), arb(b), arb(x), as.integer(flags), slong(prec))
    res
}

arb_hypgeom_2f1 <-
function (a, b, c, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_2f1, res, arb(a), arb(b), arb(c), arb(x), as.integer(flags), slong(prec))
    res
}
