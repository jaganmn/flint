#include <flint/flint.h>
#include <flint/acb.h>
#include <flint/acb_dirichlet.h>
#include <flint/acb_hypgeom.h>
#include "flint.h"
#include "acb_template.h"

SEXP R_flint_acb_lambertw(SEXP s_res, SEXP s_z, SEXP s_k, SEXP s_flags, SEXP s_prec)
{
	if (TYPEOF(s_flags) != INTSXP)
		Rf_error(_("type of '%s' is not \"%s\""), "flags", "integer");

	unsigned long long int
		nz = R_flint_get_length(s_z),
		nk = R_flint_get_length(s_k),
		nflags = (unsigned long long int) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr z = (acb_ptr) R_flint_get_pointer(s_z);
	const fmpz *k = (fmpz *) R_flint_get_pointer(s_k);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int j, n = RECYCLE4(nz, nk, nflags, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_malloc(n * sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		acb_lambertw(res + j, z + j % nz, k + j % nk, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_dirichlet_zeta(SEXP s_res, SEXP s_s, SEXP s_prec)
{
	return R_flint_acb_1ary(&acb_dirichlet_zeta, s_res, s_s, s_prec);
}

SEXP R_flint_acb_dirichlet_hurwitz(SEXP s_res, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	return R_flint_acb_2ary(&acb_dirichlet_hurwitz, s_res, s_s, s_a, s_prec);
}

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP s_res, SEXP s_z, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	return R_flint_acb_3ary(&acb_dirichlet_lerch_phi, s_res, s_z, s_s, s_a, s_prec);
}

SEXP R_flint_acb_hypgeom_gamma(SEXP s_res, SEXP s_z, SEXP s_prec)
{
	return R_flint_acb_1ary(&acb_hypgeom_gamma, s_res, s_z, s_prec);
}

SEXP R_flint_acb_hypgeom_rgamma(SEXP s_res, SEXP s_z, SEXP s_prec)
{
	return R_flint_acb_1ary(&acb_hypgeom_rgamma, s_res, s_z, s_prec);
}

SEXP R_flint_acb_hypgeom_lgamma(SEXP s_res, SEXP s_z, SEXP s_prec)
{
	return R_flint_acb_1ary(&acb_hypgeom_lgamma, s_res, s_z, s_prec);
}

SEXP R_flint_acb_hypgeom_polygamma(SEXP s_res, SEXP s_s, SEXP s_z, SEXP s_prec)
{
	return R_flint_acb_2ary(&acb_polygamma, s_res, s_s, s_z, s_prec);
}

SEXP R_flint_acb_hypgeom_2f1(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_z, SEXP s_flags, SEXP s_prec)
{
	if (TYPEOF(s_flags) != INTSXP)
		Rf_error(_("type of '%s' is not \"%s\""), "flags", "integer");

	unsigned long long int
		na = R_flint_get_length(s_a),
		nb = R_flint_get_length(s_b),
		nc = R_flint_get_length(s_c),
		nz = R_flint_get_length(s_z),
		nflags = (unsigned long long int) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a = (acb_ptr) R_flint_get_pointer(s_a),
		b = (acb_ptr) R_flint_get_pointer(s_b),
		c = (acb_ptr) R_flint_get_pointer(s_c),
		z = (acb_ptr) R_flint_get_pointer(s_z);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int j, n = RECYCLE6(na, nb, nc, nz, nflags, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_malloc(n * sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		acb_hypgeom_2f1(res + j, a + j % na, b + j % nb, c + j % nc, z + j % nz, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}
