#include <gmp.h>
#include <flint/flint.h>
#include <flint/arb.h>
#include "flint.h"
#include "arb_template.h"

SEXP R_flint_arb_0ary(void (*fn)(arb_t, slong),
                      SEXP s_res, SEXP s_prec)
{
	mp_limb_t
		nprec = R_flint_get_length(s_prec);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = nprec;
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, prec[j]);
	return R_NilValue;
}

SEXP R_flint_arb_1ary(void (*fn)(arb_t, const arb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		nprec = R_flint_get_length(s_prec);
	arb_srcptr a0 = R_flint_get_pointer(s_a0);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE2(na0, nprec);
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_arb_2ary(void (*fn)(arb_t, const arb_t, const arb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		nprec = R_flint_get_length(s_prec);
	arb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE3(na0, na1, nprec);
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_arb_3ary(void (*fn)(arb_t, const arb_t, const arb_t, const arb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		nprec = R_flint_get_length(s_prec);
	arb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE4(na0, na1, na2, nprec);
	arb_ptr res = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_arb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, prec[j % nprec]);
	return R_NilValue;
}
