acb_hypgeom_2f1 <-
function(a, b, c, z, flags, prec)
	.Call(R_flint_acb_hypgeom_2f1, a, b, c, z, flags, prec)

acb_hypgeom_2f1_continuation <-
function(a, b, c, z0, z1, f0, f1, prec)
	.Call(R_flint_acb_hypgeom_2f1, a, b, c, z0, z1, f0, f1, prec)
