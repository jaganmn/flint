acb_lambertw <-
function (z, k = 0L, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_lambertw, res, as(z, "acb"), as(k, "fmpz"), as.integer(flags), as(prec, "slong"))
    res
}

acb_dirichlet_zeta <-
function (s, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_zeta, res, as(s, "acb"), as(prec, "slong"))
    res
}

acb_dirichlet_hurwitz <-
function (s, a = 1, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_hurwitz, res, as(s, "acb"), as(a, "acb"), as(prec, "slong"))
    res
}

acb_dirichlet_lerch_phi <-
function (z = 1, s, a = 1, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_lerch_phi, res, as(z, "acb"), as(s, "acb"), as(a, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_gamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_gamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_rgamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_rgamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_lgamma <-
function (z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_lgamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

## acb_polygamma <-
acb_hypgeom_polygamma <-
function (s = 0, z, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_polygamma, res, as(s, "acb"), as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_2f1 <-
function (a, b, c, z, flags = 0L, prec = flintPrec()) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_2f1, res, as(a, "acb"), as(b, "acb"), as(c, "acb"), as(z, "acb"), as.integer(flags), as(prec, "slong"))
    res
}
