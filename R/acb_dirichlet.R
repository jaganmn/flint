acb_dirichlet_lerch_phi <-
function(z, s, a, prec)
	.Call(R_flint_acb_dirichlet_lerch_phi, z, s, a, prec)
