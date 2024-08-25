#include <flint/mag.h>
#include "R_flint.h"

void R_flint_mag_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	mag *x = (mag *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		mag_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	mag *x = (mag *) flint_calloc(n, sizeof(mag));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_mag_finalize);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN not representable by 'mag'");
			else
			mag_set_ui(x[i], (tmp < 0) ? -tmp : tmp);
		}
	} else {
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (ISNAN(tmp))
			Rf_error("NaN not representable by 'mag'");
			else
			mag_set_d (x[i], tmp);
		}
	}
	return object;
}

SEXP R_flint_mag_integer(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "mag", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(INTSXP, (R_xlen_t) n));
	mag *x = (mag *) _R_flint_x_get(from);
	int *y = INTEGER(to);
	int w = 1;
	mag_t ub;
	mag_init(ub);
	mag_set_ui(ub, (unsigned int) INT_MAX + 1U);
	for (i = 0; i < n; ++i) {
		if (mag_cmp(x[i], ub) < 0)
			y[i] = (int) mag_get_d(x[i]);
		else {
			y[i] = NA_INTEGER;
			if (w) {
				Rf_warning("NA introduced by coercion to range of \"%s\"",
				           "integer");
				w = 0;
			}
		}
	}
	mag_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_mag_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "mag", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(REALSXP, (R_xlen_t) n));
	mag *x = (mag *) _R_flint_x_get(from);
	double *y = REAL(to);
	int w = 1;
	mag_t ub;
	mag_init(ub);
	mag_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	for (i = 0; i < n; ++i) {
		if (mag_cmp(x[i], ub) < 0)
			y[i] = mag_get_d(x[i]);
		else {
			y[i] = R_PosInf;
			if (w) {
				Rf_warning("-Inf or Inf introduced by coercion to range of \"%s\"",
				           "double");
				w = 0;
			}
		}
	}
	mag_clear(ub);
	UNPROTECT(1);
	return to;
}
