#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include "flint.h"

void R_flint_ulong_finalize(SEXP x)
{
	ulong *p = (ulong *) R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_ulong_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	ulong *y = (ulong *) ((n) ? flint_calloc(n, sizeof(ulong)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		/* nothing to do */
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		int *x = INTEGER(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (tmp == NA_INTEGER)
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "ulong");
			else if (tmp < 0)
			Rf_error(_("integer not in range of '%s'"), "ulong");
			else
			y[i] = (ulong) tmp;
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "ulong");
#if FLINT64
			else if (tmp <= -1.0 || tmp >= 0x1.0p+64)
#else
			else if (tmp <= -1.0 || tmp >= 0x1.0p+32)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "ulong");
			else
			y[i] = (ulong) tmp;
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_ulong_nulong(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "ulong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nulong", INTSXP, (R_xlen_t) n));
	ulong *x = (ulong *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (x[i] <= INT_MAX)
			y[i] = (int) x[i];
		else {
			y[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	UNPROTECT(1);
	return to;
}

SEXP R_flint_ulong_vector(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "ulong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	ulong *x = (ulong *) R_flint_get_pointer(from);
	double *y = REAL(to);
#if FLINT64
	fmpz_t tmp;
	fmpz_init(tmp);
	for (i = 0; i < n; ++i) {
		fmpz_set_ui(tmp, x[i]);
		y[i] = fmpz_get_d(tmp);
	}
	fmpz_clear(tmp);
#else
	for (i = 0; i < n; ++i)
		y[i] = (double) x[i];
#endif
	UNPROTECT(1);
	return to;
}

SEXP R_flint_ulong_format(SEXP from, SEXP s_base)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "ulong", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__);
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	ulong *x = (ulong *) R_flint_get_pointer(from);
	size_t nc, ncmax = 0;
	mpz_t z;
	mpz_init(z);
	for (i = 0; i < n; ++i) {
		mpz_set_ui(z, x[i]);
		nc = mpz_sizeinbase(z, base);
		if (nc < ncmax)
			ncmax = nc;
	}
	char *buffer = R_alloc(ncmax + 1, 1);
	memset(buffer, 0, ncmax + 2);
	for (i = 0; i < n; ++i) {
		mpz_set_si(z, x[i]);
		nc = ncmax - mpz_sizeinbase(z, base);
		if (nc > 0 && buffer[nc - 1] != ' ')
			memset(buffer, ' ', nc);
		mpz_get_str(buffer + nc, base, z);
		SET_STRING_ELT(to, (R_xlen_t) i, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	UNPROTECT(1);
	return to;
}
