#include <flint/acb_dirichlet.h>
#include "R_flint.h"

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP s_res, SEXP s_z, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	unsigned long long int i,
		nz = _R_flint_length_get(s_z),
		ns = _R_flint_length_get(s_s),
		na = _R_flint_length_get(s_a),
		nprec = _R_flint_length_get(s_prec),
		n = RECYCLE4(nz, ns, na, nprec);
	_R_flint_length_set(s_res, n);

	acb_ptr res = (acb_ptr) flint_calloc(n, sizeof(acb_t)),
		z = (acb_ptr) _R_flint_x_get(s_z),
		s = (acb_ptr) _R_flint_x_get(s_s),
		a = (acb_ptr) _R_flint_x_get(s_a);
	slong *prec = (slong *) _R_flint_x_get(s_prec);
	_R_flint_x_set(s_res, res, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_dirichlet_lerch_phi(res + i, z + i % nz, s + i % ns, a + i % na,
		                        prec[i % nprec]);
	return R_NilValue;
}
