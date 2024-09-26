#include <flint/acb_hypgeom.h>
#include "R_flint.h"

SEXP R_flint_acb_hypgeom_2f1(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_z, SEXP s_flags, SEXP s_prec)
{
	unsigned long long int
		na = R_flint_get_length(s_a),
		nb = R_flint_get_length(s_b),
		nc = R_flint_get_length(s_c),
		nz = R_flint_get_length(s_z),
		nprec = R_flint_get_length(s_prec);
	acb_ptr
		a = (acb_ptr) R_flint_get_pointer(s_a),
		b = (acb_ptr) R_flint_get_pointer(s_b),
		c = (acb_ptr) R_flint_get_pointer(s_c),
		z = (acb_ptr) R_flint_get_pointer(s_z);
	int flags = asFlags(s_flags, __func__);
	slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int i, n = RECYCLE5(na, nb, nc, nz, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_calloc(n, sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_hypgeom_2f1(res + i, a + i % na, b + i % nb, c + i % nc, z + i % nz,
		                flags, prec[i % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_hypgeom_2f1_continuation(SEXP s_res0, SEXP s_res1, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_z0, SEXP s_z1, SEXP s_f0, SEXP s_f1, SEXP s_prec)
{
	unsigned long long int
		na = R_flint_get_length(s_a),
		nb = R_flint_get_length(s_b),
		nc = R_flint_get_length(s_c),
		nz0 = R_flint_get_length(s_z0),
		nz1 = R_flint_get_length(s_z1),
		nf0 = R_flint_get_length(s_f0),
		nf1 = R_flint_get_length(s_f1),
		nprec = R_flint_get_length(s_prec);
	acb_ptr
		a = (acb_ptr) R_flint_get_pointer(s_a),
		b = (acb_ptr) R_flint_get_pointer(s_b),
		c = (acb_ptr) R_flint_get_pointer(s_c),
		z0 = (acb_ptr) R_flint_get_pointer(s_z0),
		z1 = (acb_ptr) R_flint_get_pointer(s_z1),
		f0 = (acb_ptr) R_flint_get_pointer(s_f0),
		f1 = (acb_ptr) R_flint_get_pointer(s_f1);
	slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int i,
		n = RECYCLE5(na, nb, nc, RECYCLE4(nz0, nz1, nf0, nf1), nprec);
	acb_ptr
		res0 = (acb_ptr) ((n) ? flint_calloc(n, sizeof(acb_t)) : 0),
		res1 = (acb_ptr) ((n) ? flint_calloc(n, sizeof(acb_t)) : 0);
	R_flint_set(s_res0, res0, n, (R_CFinalizer_t) &R_flint_acb_finalize);
	R_flint_set(s_res1, res1, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_hypgeom_2f1_continuation(res0 + i, res1 + i,
		                             a + i % na, b + i % nb, c + i % nc,
		                             z0 + i % nz0, z1 + i % nz1,
		                             f0 + i % nf0, f1 + i % nf1,
		                             prec[i % nprec]);
	return R_NilValue;
}
