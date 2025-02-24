#include "flint.h"
#include "acb_template.h"

SEXP R_flint_acb_0ary(void (*fn)(acb_t, slong),
                      SEXP s_res, SEXP s_prec)
{
	mp_limb_t
		nprec = R_flint_get_length(s_prec);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = nprec;
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, prec[j]);
	return R_NilValue;
}

SEXP R_flint_acb_1ary(void (*fn)(acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr a0 = R_flint_get_pointer(s_a0);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE2(na0, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_2ary(void (*fn)(acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE3(na0, na1, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_3ary(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE4(na0, na1, na2, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_4ary(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_a3, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		na3 = R_flint_get_length(s_a3),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2),
		a3 = R_flint_get_pointer(s_a3);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE5(na0, na1, na2, na3, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, a3 + j % na3, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_5ary(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_a3, SEXP s_a4, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		na3 = R_flint_get_length(s_a3),
		na4 = R_flint_get_length(s_a4),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2),
		a3 = R_flint_get_pointer(s_a3),
		a4 = R_flint_get_pointer(s_a4);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE6(na0, na1, na2, na3, na4, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, a3 + j % na3, a4 + j % na4, prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_0ary_flags(void (*fn)(acb_t, int, slong),
                            SEXP s_res, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE2(nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_1ary_flags(void (*fn)(acb_t, const acb_t, int, slong),
                            SEXP s_res, SEXP s_a0, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr a0 = R_flint_get_pointer(s_a0);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE3(na0, nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_2ary_flags(void (*fn)(acb_t, const acb_t, const acb_t, int, slong),
                            SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE4(na0, na1, nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_3ary_flags(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE5(na0, na1, na2, nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_4ary_flags(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_a3, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		na3 = R_flint_get_length(s_a3),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2),
		a3 = R_flint_get_pointer(s_a3);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE6(na0, na1, na2, na3, nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, a3 + j % na3, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}

SEXP R_flint_acb_5ary_flags(void (*fn)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP s_res, SEXP s_a0, SEXP s_a1, SEXP s_a2, SEXP s_a3, SEXP s_a4, SEXP s_flags, SEXP s_prec)
{
	mp_limb_t
		na0 = R_flint_get_length(s_a0),
		na1 = R_flint_get_length(s_a1),
		na2 = R_flint_get_length(s_a2),
		na3 = R_flint_get_length(s_a3),
		na4 = R_flint_get_length(s_a4),
		nflags = (mp_limb_t) XLENGTH(s_flags),
		nprec = R_flint_get_length(s_prec);
	acb_srcptr
		a0 = R_flint_get_pointer(s_a0),
		a1 = R_flint_get_pointer(s_a1),
		a2 = R_flint_get_pointer(s_a2),
		a3 = R_flint_get_pointer(s_a3),
		a4 = R_flint_get_pointer(s_a4);
	const int *flags = INTEGER_RO(s_flags);
	const slong *prec = R_flint_get_pointer(s_prec);

	mp_limb_t j, n = RECYCLE7(na0, na1, na2, na3, na4, nflags, nprec);
	acb_ptr res = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
	R_flint_set(s_res, res, n, (R_CFinalizer_t) &R_flint_acb_finalize);

	for (j = 0; j < n; ++j)
		fn(res + j, a0 + j % na0, a1 + j % na1, a2 + j % na2, a3 + j % na3, a4 + j % na4, flags[j % nflags], prec[j % nprec]);
	return R_NilValue;
}
