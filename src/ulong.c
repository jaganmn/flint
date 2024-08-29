#include "R_flint.h"

void R_flint_ulong_finalize(SEXP object)
{
	ulong *x = (ulong *) _R_flint_x_get(object);
	flint_free(x);
	return;
}

SEXP R_flint_ulong_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	ulong *x = (ulong *) flint_calloc(n, sizeof(ulong));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_ulong_finalize);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "ulong");
			else if (tmp < 0)
			Rf_error("integer not in range of '%s'", "ulong");
			else
			x[i] = (ulong) tmp;
		}
	} else {
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "ulong");
#if FLINT64
			else if (tmp <= -1.0 || tmp > UWORD_MAX)
#else
			else if (tmp <= -1.0 || tmp >= UWORD_MAX + 1.0)
#endif
			Rf_error("floating-point number not in range of '%s'", "ulong");
			else
			x[i] = (ulong) tmp;
		}
	}
	return object;
}

SEXP R_flint_ulong_integer(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "ulong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(INTSXP, (R_xlen_t) n));
	ulong *x = (ulong *) _R_flint_x_get(from);
	int *y = INTEGER(to);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (x[i] <= INT_MAX)
			y[i] = (int) x[i];
		else {
			warning(_("NAs introduced by coercion to integer range"));
			y[i] = NA_INTEGER;
			OOB_INTEGER(w);
		}
	}
	UNPROTECT(1);
	return to;
}

SEXP R_flint_ulong_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "ulong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(REALSXP, (R_xlen_t) n));
	ulong *x = (ulong *) _R_flint_x_get(from);
	double *y = REAL(to);
	for (i = 0; i < n; ++i)
		y[i] = (double) x[i];
	UNPROTECT(1);
	return to;
}
