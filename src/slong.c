#include "R_flint.h"

void R_flint_slong_finalize(SEXP object)
{
	slong *x = (slong *) _R_flint_get_x(object);
	flint_free(x);
	return;
}

SEXP R_flint_slong_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_set_length(object, n);
	slong *x = (slong *) flint_calloc(n, sizeof(slong));
	_R_flint_set_x(object, x, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(value)) {
	case INTSXP:
	{
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, -Inf, Inf not representable by '%s'", "slong");
			else
			x[i] = (slong) tmp;
		}
		break;
	}
	case REALSXP:
	{
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (!R_FINITE(tmp))
			Rf_error("NaN, -Inf, Inf not representable by '%s'", "slong");
#if FLINT64
			else if (tmp < WORD_MIN || tmp > WORD_MAX)
#else
			else if (tmp <= WORD_MIN - 1.0 || tmp >= WORD_MAX + 1.0)
#endif
			Rf_error("floating-point number not in range of '%s'", "slong");
			else
			x[i] = (slong) tmp;
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(value, __func__);
		break;
	}
	return object;
}

SEXP R_flint_slong_integer(SEXP from)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "slong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) n));
	slong *x = (slong *) _R_flint_get_x(from);
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

SEXP R_flint_slong_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "slong", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	slong *x = (slong *) _R_flint_get_x(from);
	double *y = REAL(to);
	for (i = 0; i < n; ++i)
		y[i] = (double) x[i];
	UNPROTECT(1);
	return to;
}
