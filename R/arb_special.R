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

arb_hypgeom_2f1 <-
function (a, b, c, x, flags = 0L, prec = flintPrec()) {
    res <- flintNew("arb")
    .Call(R_flint_arb_hypgeom_2f1, res, as(a, "arb"), as(b, "arb"), as(c, "arb"), as(x, "arb"), as.integer(flags), as(prec, "slong"))
    res
}
