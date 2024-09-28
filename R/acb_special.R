acb_lambertw <-
function (z, k, flags = 0L, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_lambertw, res, as(z, "acb"), as(k, "fmpz"), flags, as(prec, "slong"))
    res
}

acb_dirichlet_hurwitz <-
function (s, a, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_hurwitz, res, as(s, "acb"), as(a, "acb"), as(prec, "slong"))
    res
}

acb_dirichlet_lerch_phi <-
function (z, s, a, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_dirichlet_lerch_phi, res, as(z, "acb"), as(s, "acb"), as(a, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_gamma <-
function (z, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_gamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_rgamma <-
function (z, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_rgamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_lgamma <-
function (z, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_lgamma, res, as(z, "acb"), as(prec, "slong"))
    res
}

acb_hypgeom_2f1 <-
function (a, b, c, z, flags = 0L, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_2f1, res, as(a, "acb"), as(b, "acb"), as(c, "acb"), as(z, "acb"), flags, as(prec, "slong"))
    res
}
