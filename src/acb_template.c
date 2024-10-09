#include <flint/flint.h>
#include <flint/acb.h>
#include "flint.h"
#include "acb_template.h"

SEXP R_flint_acb_1ary(void (*fn)(acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_prec)
{
	unsigned long long int
		na0 = R_flint_get_length(s_a0),
		nprec = R_flint_get_length(s_prec);
	acb_ptr a0 = (acb_ptr) R_flint_get_pointer(s_a0);
	slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int j, n = RECYCLE2(na0, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_2ary(void (*fn)(acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_prec)
{
	unsigned long long int
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		nprec = R_flint_get_length(s_prec);
	acb_ptr
		a0 = (acb_ptr) R_flint_get_pointer(s_a0),
		a1 = (acb_ptr) R_flint_get_pointer(s_a1);
	slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int j, n = RECYCLE3(na0, na1, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_3ary(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_prec)
{
	unsigned long long int
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		nprec = R_flint_get_length(s_prec);
	acb_ptr
		a0 = (acb_ptr) R_flint_get_pointer(s_a0),
		a1 = (acb_ptr) R_flint_get_pointer(s_a1),
		a2 = (acb_ptr) R_flint_get_pointer(s_a2);
	slong *prec = (slong *) R_flint_get_pointer(s_prec);

	unsigned long long int j, n = RECYCLE4(na0, na1, na2, nprec);
	acb_ptr res = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, prec[j % nprec]);
	return R_NilValue;
}
