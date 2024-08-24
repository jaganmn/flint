acb_hypgeom_2f1 <-
function (a, b, c, z, flags, prec) {
    res <- new("acb")
    .Call(R_flint_acb_hypgeom_2f1, res, as(a, "acb"), as(b, "acb"), as(c, "acb"), as(c, "acb"), flags, prec)
    res
}

acb_hypgeom_2f1_continuation <-
function (a, b, c, z0, z1, f0, f1, prec) {
    res0 <- new("acb")
    res1 <- new("acb")
    .Call(R_flint_acb_hypgeom_2f1, res0, res1, as(a, "acb"), as(b, "acb"), as(c, "acb"), as(z0, "acb"), as(z1, "acb"), as(f0, "acb"), as(f1, "acb"), prec)
    res0
}
