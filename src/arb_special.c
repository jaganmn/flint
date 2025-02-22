#include "flint.h"
#include "arb_template.h"
#include <flint/arb_hypgeom.h>

SEXP R_flint_arb_lambertw(SEXP s_res, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		nx = R_flint_get_length(s_x),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	arb_srcptr x = R_flint_get_pointer(s_x);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE3(nx, nflags, nprec);
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		arb_lambertw(res + j, x + j % nx, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
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

SEXP R_flint_arb_hypgeom_2f1(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_x, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na = R_flint_get_length(s_a),
		nb = R_flint_get_length(s_b),
		nc = R_flint_get_length(s_c),
		nx = R_flint_get_length(s_x),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	arb_srcptr
		a = R_flint_get_pointer(s_a),
		b = R_flint_get_pointer(s_b),
		c = R_flint_get_pointer(s_c),
		x = R_flint_get_pointer(s_x);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE6(na, nb, nc, nx, nflags, nprec);
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		arb_hypgeom_2f1(res + j, a + j % na, b + j % nb, c + j % nc, x + j % nx, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}
