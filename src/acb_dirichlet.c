#include <flint/flint.h>
#include <flint/acb.h>
#include <flint/acb_dirichlet.h>
#include "flint.h"

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP s_res, SEXP s_z, SEXP s_s, SEXP s_a, SEXP s_prec)
{
	unsigned long long int
		nz = R_flint_get_length(s_z),
		ns = R_flint_get_length(s_s),
		na = R_flint_get_length(s_a),
		nprec = R_flint_get_length(s_prec);
	acb_ptr
		z = (acb_ptr) R_flint_get_pointer(s_z),
		s = (acb_ptr) R_flint_get_pointer(s_s),
		a = (acb_ptr) R_flint_get_pointer(s_a);
	slong
		*prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int i, n = RECYCLE4(nz, ns, na, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_calloc(n, sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (i = 0; i < n; ++i)
		acb_dirichlet_lerch_phi(res + i, z + i % nz, s + i % ns, a + i % na,
		                        prec[i % nprec]);
	return R_NilValue;
}
