acb_hypgeom_2f1 <-
function (a, b, c, z, flags = 0L, prec = 53L) {
    res <- flintNew("acb")
    .Call(R_flint_acb_hypgeom_2f1, res, as(a, "acb"), as(b, "acb"), as(c, "acb"), as(z, "acb"), flags, as(prec, "slong"))
    res
}
