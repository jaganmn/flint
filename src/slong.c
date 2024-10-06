#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include "flint.h"

void R_flint_slong_finalize(SEXP x)
{
	slong *p = (slong *) R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_slong_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	slong *y = (slong *) ((n) ? flint_calloc(n, sizeof(slong)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
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
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "slong");
			else
			y[i] = (slong) tmp;
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "slong");
#if FLINT64
			else if (tmp <  -0x1.0p+63       || tmp >= 0x1.0p+63)
#else
			else if (tmp <= -0x1.0p+31 - 1.0 || tmp >= 0x1.0p+31)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "slong");
			else
			y[i] = (slong) tmp;
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_slong_nslong(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "slong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nslong", INTSXP, (R_xlen_t) n));
	slong *x = (slong *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (x[i] > INT_MIN && x[i] <= INT_MAX)
			y[i] = (int) x[i];
		else {
			y[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	UNPROTECT(1);
	return to;
}

SEXP R_flint_slong_vector(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "slong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	slong *x = (slong *) R_flint_get_pointer(from);
	double *y = REAL(to);
#if FLINT64
	fmpz_t tmp;
	fmpz_init(tmp);
	for (i = 0; i < n; ++i) {
		fmpz_set_si(tmp, x[i]);
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

/* NB: mpz_sizeinbase() can be 1 too big for bases not equal to power of 2 */
SEXP R_flint_slong_format(SEXP from, SEXP s_base)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "slong", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	slong *x = (slong *) R_flint_get_pointer(from), xmin = 0, xmax = 0;
	for (i = 0; i < n; ++i) {
		if (x[i] > xmax)
			xmax = x[i];
		else if (x[i] < xmin)
			xmin = x[i];
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init(z);
	mpz_set_si(z, (xmin < -xmax) ? xmin : xmax);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	mpz_set_si(z, (xmin < -xmax) ? xmax : xmin);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (i = 0; i < n; ++i) {
		mpz_set_si(z, x[i]);
		nc = mpz_sizeinbase(z, abase) + (mpz_sgn(z) < 0);
		if (nc > ncmax)
			nc = ncmax;
		ns = ncmax - nc;
		if (ns > 0 && buffer[ns - 1] != ' ')
			memset(buffer, ' ', ns);
		mpz_get_str(buffer + ns, base, z);
		if (buffer[ncmax - 1] == '\0') {
			memmove(buffer + ns + 1, buffer + ns, nc);
			buffer[ns] = ' ';
		}
		SET_STRING_ELT(to, (R_xlen_t) i, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	UNPROTECT(1);
	return to;
}
