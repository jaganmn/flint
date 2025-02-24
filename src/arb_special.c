#include "flint.h"
#include "arb_template.h"
#include <flint/arb_hypgeom.h>

SEXP R_flint_arb_lambertw(SEXP s_res, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	return R_flint_arb_1ary_flags(&arb_lambertw, s_res, s_x, s_flags, s_prec);
}

SEXP R_flint_arb_dirichlet_zeta(SEXP s_res, SEXP s_s, SEXP s_prec)
{
	return R_flint_arb_1ary(&arb_zeta, s_res, s_s, s_prec);
}

SEXP R_flint_arb_dirichlet_hurwitz(SEXP s_res, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	return R_flint_arb_2ary(&arb_hurwitz_zeta, s_res, s_s, s_a, s_prec);
}

SEXP R_flint_arb_hypgeom_gamma(SEXP s_res, SEXP s_x, SEXP s_prec)
{
	return R_flint_arb_1ary(&arb_hypgeom_gamma, s_res, s_x, s_prec);
}

SEXP R_flint_arb_hypgeom_rgamma(SEXP s_res, SEXP s_x, SEXP s_prec)
{
	return R_flint_arb_1ary(&arb_hypgeom_rgamma, s_res, s_x, s_prec);
}

SEXP R_flint_arb_hypgeom_lgamma(SEXP s_res, SEXP s_x, SEXP s_prec)
{
	return R_flint_arb_1ary(&arb_hypgeom_lgamma, s_res, s_x, s_prec);
}

SEXP R_flint_arb_hypgeom_gamma_lower(SEXP s_res, SEXP s_s, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	return R_flint_arb_2ary_flags(&arb_hypgeom_gamma_lower, s_res, s_s, s_x, s_flags, s_prec);
}

SEXP R_flint_arb_hypgeom_gamma_upper(SEXP s_res, SEXP s_s, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	return R_flint_arb_2ary_flags(&arb_hypgeom_gamma_upper, s_res, s_s, s_x, s_flags, s_prec);
}

SEXP R_flint_arb_hypgeom_beta_lower(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	return R_flint_arb_3ary_flags(&arb_hypgeom_beta_lower, s_res, s_a, s_b, s_x, s_flags, s_prec);
}

SEXP R_flint_arb_hypgeom_2f1(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	return R_flint_arb_4ary_flags(&arb_hypgeom_2f1, s_res, s_a, s_b, s_c, s_x, s_flags, s_prec);
}
