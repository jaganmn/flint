acb_lambertw <-
function (z, k = 0L, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_lambertw, res, acb(z), fmpz(k), as.integer(flags), slong(prec))
    res
}

acb_dirichlet_zeta <-
function (s, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_zeta, res, acb(s), slong(prec))
    res
}

acb_dirichlet_hurwitz <-
function (s, a = 1, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_hurwitz, res, acb(s), acb(a), slong(prec))
    res
}

acb_dirichlet_lerch_phi <-
function (z = 1, s, a = 1, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_lerch_phi, res, acb(z), acb(s), acb(a), slong(prec))
    res
}

acb_hypgeom_gamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_gamma, res, acb(z), slong(prec))
    res
}

acb_hypgeom_rgamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_rgamma, res, acb(z), slong(prec))
    res
}

acb_hypgeom_lgamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_lgamma, res, acb(z), slong(prec))
    res
}

## acb_polygamma <-
acb_hypgeom_polygamma <-
function (s = 0, z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_polygamma, res, acb(s), acb(z), slong(prec))
    res
}

acb_hypgeom_gamma_lower <-
function (s, z, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_gamma_lower, res, acb(s), acb(z), as.integer(flags), slong(prec))
    res
}

acb_hypgeom_gamma_upper <-
function (s, z, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_gamma_upper, res, acb(s), acb(z), as.integer(flags), slong(prec))
    res
}

acb_hypgeom_beta <-
function (a, b, prec = flintPrec()) {
    if (FALSE) {
    ## FLINT bug, report this
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_beta_lower, res, acb(a), acb(b), acb(1), 0L, slong(prec))
    res
    }
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    acb_hypgeom_gamma(a) * acb_hypgeom_gamma(b) * acb_hypgeom_rgamma(a + b)
}

acb_hypgeom_beta_lower <-
function (a, b, z, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_beta_lower, res, acb(a), acb(b), acb(z), as.integer(flags), slong(prec))
    res
}

acb_hypgeom_2f1 <-
function (a, b, c, z, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_2f1, res, acb(a), acb(b), acb(c), acb(z), as.integer(flags), slong(prec))
    res
}
