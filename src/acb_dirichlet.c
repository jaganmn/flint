#include <flint/acb_dirichlet.h>
#include "R_flint.h"

SEXP R_acb_dirichlet_lerch_phi(SEXP s_res, SEXP s_z, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	slong prec = asPrec(s_prec);

	unsigned long long int i,
		nz = _R_flint_length_get(s_z),
		ns = _R_flint_length_get(s_s),
		na = _R_flint_length_get(s_a),
		n = RECYCLE3(nz, ns, na);
	_R_flint_length_set(s_res, n);

	acb
		*res = (acb *) flint_calloc(n, sizeof(acb)),
		*z = (acb *) _R_flint_x_get(s_z),
		*s = (acb *) _R_flint_x_get(s_s),
		*a = (acb *) _R_flint_x_get(s_a);
	_R_flint_x_set(s_res, res, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_dirichlet_lerch_phi(res[i], z[i % nz], s[i % ns], a[i % na],
		                        prec);
	return R_NilValue;
}
