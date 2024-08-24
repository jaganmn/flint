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
