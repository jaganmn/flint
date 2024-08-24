acb_dirichlet_lerch_phi <-
function (z, s, a, prec) {
    res <- new("acb")
	.Call(R_flint_acb_dirichlet_lerch_phi, res, as(z, "acb"), as(s, "acb"), as(a, "acb"), prec)
    res
}
