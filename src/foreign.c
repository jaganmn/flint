#include "flint.h"

SEXP R_flint_coerce_bigz_fmpz(SEXP from)
{
	const int *x = (void *) RAW(from);
	int h = *(x++);
	if (h < 0)
		Rf_error(_("invalid"));
	mp_limb_t j, n = (mp_limb_t) h;
	fmpz *y = (n) ? flint_calloc(n, sizeof(fmpz)) : 0;
	SEXP to = PROTECT(newObject("fmpz"));
	R_flint_set(to, y, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		if (x[0] <= 0) {
			mpz_clear(t);
			Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpz");
		}
		mpz_import(t, (size_t) x[0], 1, sizeof(int), 0, 0, x + 2);
		if (x[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(y + j, t);
		x += 2LL + x[0];
	}
	mpz_clear(t);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_coerce_fmpz_bigz(SEXP from)
{
	mp_limb_t j, n = R_flint_get_length(from);
	if (n > INT_MAX)
		Rf_error(_("value length would exceed maximum %d"),
		         INT_MAX);
	const fmpz *x = R_flint_get_pointer(from);
	size_t words, count = 0;
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, x + j);
		words = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		if (words > INT_MAX) {
			mpz_clear(t);
			Rf_error(_("raw length of element would exceed maximum %lld"),
			         (long long int) INT_MAX + 2);
		}
		count += words;
	}
	mpz_clear(t);
	count += 1 + 2 * n;
	if (count > R_XLEN_T_MAX / sizeof(int))
		Rf_error(_("raw length would exceed maximum %lld"),
		         (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count * sizeof(int))));
	int *y = (void *) RAW(to);
	*(y++) = (int) n;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, x + j);
		words = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(y++) = (int) words;
		*(y++) = mpz_sgn(t);
		mpz_export(y, 0, 1, sizeof(int), 0, 0, t);
		y += words;
	}
	mpz_clear(t);
	SEXP class = PROTECT(Rf_mkString("bigz"));
	Rf_setAttrib(to, R_ClassSymbol, class);
	UNPROTECT(2);
	return to;
}

SEXP R_flint_coerce_bigq_fmpq(SEXP from)
{
	SEXP
		sn = from,
		sd = PROTECT(Rf_getAttrib(sn, Rf_install("denominator")));
	const int *xn = (void *) RAW(sn), *xd = (void *) RAW(sd);
	int h = *(xn++);
	if (h < 0 || h != *(xd++))
		Rf_error(_("invalid"));
	mp_limb_t j, n = (mp_limb_t) h;
	fmpq *y = (n) ? flint_calloc(n, sizeof(fmpq)) : 0;
	SEXP to = PROTECT(newObject("fmpq"));
	R_flint_set(to, y, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		if (xn[0] <= 0 || xd[0] <= 0) {
			mpz_clear(t);
			Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpz");
		}

		mpz_import(t, (size_t) xn[0], 1, sizeof(int), 0, 0, xn + 2);
		if (xn[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(fmpq_numref(y + j), t);
		xn += 2LL + xn[0];

		mpz_import(t, (size_t) xd[0], 1, sizeof(int), 0, 0, xd + 2);
		if (xd[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(fmpq_denref(y + j), t);
		xd += 2LL + xd[0];

		if (mpz_sgn(t) == 0) {
			mpz_clear(t);
			Rf_error(_("zero denominator not valid in canonical \"%s\""), "fmpq");
		}
		fmpq_canonicalise(y + j);
	}
	mpz_clear(t);
	UNPROTECT(2);
	return to;

}

SEXP R_flint_coerce_fmpq_bigq(SEXP from)
{
	mp_limb_t j, n = R_flint_get_length(from);
	if (n > INT_MAX)
		Rf_error(_("value length would exceed maximum %d"),
		         INT_MAX);
	const fmpq *x = R_flint_get_pointer(from);
	size_t words[2], count[2] = { 0, 0 };
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, fmpq_numref(x + j));
		words[0] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		fmpz_get_mpz(t, fmpq_denref(x + j));
		words[1] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		if (words[0] > INT_MAX || words[1] > INT_MAX) {
			mpz_clear(t);
			Rf_error(_("raw length of vector element would exceed maximum %d"),
			         INT_MAX);
		}
		count[0] += words[0];
		count[1] += words[1];
	}
	mpz_clear(t);
	count[0] += 1 + 2 * n;
	count[1] += 1 + 2 * n;
	if (count[0] > R_XLEN_T_MAX / sizeof(int) ||
	    count[1] > R_XLEN_T_MAX / sizeof(int))
		Rf_error(_("raw length would exceed maximum %lld"),
		         (long long int) R_XLEN_T_MAX);
	SEXP
		sn = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count[0] * sizeof(int)))),
		sd = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count[1] * sizeof(int)))),
		to = sn;
	int *yn = (void *) RAW(sn), *yd = (void *) RAW(sd);
	*(yn++) = *(yd++) = (int) n;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, fmpq_numref(x + j));
		words[0] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(yn++) = (int) words[0];
		*(yn++) = mpz_sgn(t);
		mpz_export(yn, 0, 1, sizeof(int), 0, 0, t);
		yn += words[0];

		fmpz_get_mpz(t, fmpq_denref(x + j));
		words[1] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(yd++) = (int) words[1];
		*(yd++) = mpz_sgn(t);
		mpz_export(yd, 0, 1, sizeof(int), 0, 0, t);
		yd += words[1];
	}
	mpz_clear(t);
	SEXP class = PROTECT(Rf_mkString("bigq"));
	Rf_setAttrib(to, R_ClassSymbol, class);
	Rf_setAttrib(to, Rf_install("denominator"), sd);
	UNPROTECT(3);
	return to;
}

SEXP R_flint_coerce_mpfr_arf(SEXP from)
{
	return R_NilValue;
}

SEXP R_flint_coerce_arf_mpfr(SEXP from)
{
	return R_NilValue;
}
