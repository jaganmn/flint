#include <flint/fmpz.h>
#include "R_flint.h"

void R_flint_fmpz_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_get_length(object);
	fmpz *x = (fmpz *) _R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		fmpz_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_set_length(object, n);
	fmpz *x = (fmpz *) flint_calloc(n, sizeof(fmpz));
	_R_flint_set_x(object, x, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	switch (TYPEOF(value)) {
	case INTSXP:
	{
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
			else
			fmpz_set_si(x + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (!R_FINITE(tmp))
			Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
			else
			fmpz_set_d (x + i, (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(value, __func__);
		break;
	}
	return object;
}

SEXP R_flint_fmpz_integer(SEXP from)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) _R_flint_get_x(from);
	int *y = INTEGER(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = (int) fmpz_get_si(x + i);
		else {
			y[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_fmpz_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) _R_flint_get_x(from);
	double *y = REAL(to);
	int w = 1;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_one_2exp(ub, DBL_MAX_EXP);
	fmpz_neg(lb, ub);
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = fmpz_get_d(x + i);
		else {
			y[i] = (fmpz_sgn(x + i) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}
