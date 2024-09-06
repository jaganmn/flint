#include <flint/mag.h>
#include "R_flint.h"

void R_flint_mag_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_get_length(object);
	mag_ptr x = (mag_ptr) _R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		mag_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_set_length(object, n);
	mag_ptr x = (mag_ptr) flint_calloc(n, sizeof(mag_t));
	_R_flint_set_x(object, x, (R_CFinalizer_t) &R_flint_mag_finalize);
	switch (TYPEOF(value)) {
	case INTSXP:
	{
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN not representable by '%s'", "mag");
			else
			mag_set_ui(x + i, (ulong) ((tmp < 0) ? -tmp : tmp));
		}
		break;
	}
	case REALSXP:
	{
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (ISNAN(tmp))
			Rf_error("NaN not representable by '%s'", "mag");
			else
			mag_set_d (x + i, tmp);
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(value, __func__);
		break;
	}
	return object;
}

SEXP R_flint_mag_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "mag", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	mag_ptr x = (mag_ptr) _R_flint_get_x(from);
	double *y = REAL(to);
	mag_t ub;
	mag_init(ub);
	mag_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (mag_cmp(x + i, ub) < 0)
			y[i] = mag_get_d(x + i);
		else {
			y[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	mag_clear(ub);
	UNPROTECT(1);
	return to;
}
