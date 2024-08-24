#include <flint/acb_hypgeom.h>
#include "R_flint.h"

SEXP R_flint_acb_hypgeom_2f1(SEXP s_res, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_z, SEXP s_flags, SEXP s_prec)
{
	int flags = asFlags(s_flags);
	slong prec = asPrec(s_prec);

	unsigned long long int i,
		na = _R_flint_length_get(s_a),
		nb = _R_flint_length_get(s_b),
		nc = _R_flint_length_get(s_c),
		nz = _R_flint_length_get(s_z),
		n  = RECYCLE4(na, nb, nc, nz);
	_R_flint_length_set(s_res, n);

	acb
		*pa = (acb *) _R_flint_x_get(s_a),
		*pb = (acb *) _R_flint_x_get(s_b),
		*pc = (acb *) _R_flint_x_get(s_c),
		*pz = (acb *) _R_flint_x_get(s_z),
		*p  = (acb *) flint_calloc(n, sizeof(acb));
	_R_flint_x_set(s_res, p, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_hypgeom_2f1(p[i], pa[i % na], pb[i % nb], pc[i % nc], pz[i % nz],
		                prec);
	return R_NilValue;
}

SEXP R_flint_acb_hypgeom_2f1_continuation(SEXP s_res0, SEXP s_res1, SEXP s_a, SEXP s_b, SEXP s_c, SEXP s_z0, SEXP s_z1, SEXP s_f0, SEXP s_f1, SEXP s_prec)
{
	slong prec = asPrec(s_prec);

	unsigned long long int i,
		na  = _R_flint_length_get(s_a),
		nb  = _R_flint_length_get(s_b),
		nc  = _R_flint_length_get(s_c),
		nz0 = _R_flint_length_get(s_z0),
		nz1 = _R_flint_length_get(s_z1),
		nf0 = _R_flint_length_get(s_f0),
		nf1 = _R_flint_length_get(s_f1),
		n   = RECYCLE4(na, nb, nc, RECYCLE4(nz0, nz1, nf0, nf1));
	_R_flint_length_set(s_res0, n);
	_R_flint_length_set(s_res1, n);

	acb
		*res0 = (acb *) flint_calloc(n, sizeof(acb)),
		*res1 = (acb *) flint_calloc(n, sizeof(acb)),
		*a    = (acb *) _R_flint_x_get(s_a),
		*b    = (acb *) _R_flint_x_get(s_b),
		*c    = (acb *) _R_flint_x_get(s_c),
		*z0   = (acb *) _R_flint_x_get(s_z0),
		*z1   = (acb *) _R_flint_x_get(s_z1),
		*f0   = (acb *) _R_flint_x_get(s_f0),
		*f1   = (acb *) _R_flint_x_get(s_f1);
	_R_flint_x_set(s_res0, res0, (R_CFinalizer_t) &R_flint_acb_finalize);
	_R_flint_x_set(s_res1, res1, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_hypgeom_2f1_continuation(res0[i], res1[i],
		                             a[i % na], b[i % nb], c[i % nc],
		                             z0[i % nz0], z1[i % nz1],
		                             f0[i % nf0], f1[i % nf1],
		                             prec);
	return R_NilValue;
}
