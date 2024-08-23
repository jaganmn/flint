#include <flint/mag.h>
#include "R_flint.h"

static
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
	/* FIXME: handle NA_INTEGER, NaN */
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	mag *x = flint_calloc(n, mag);
	R_CFinalizer_t f = (R_CFinalizer_t) &R_flint_mag_finalize;
	_R_flint_x_set(object, x, f);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value);
		for (i = 0; i < n; ++i)
			mag_set_ui(x[i], (y[i] < 0) ? -y[i] : y[i]);
	} else {
		double *y = REAL(value);
		for (i = 0; i < n; ++i)
			mag_set_d (x[i], y[i]);
	}
	return object;
}
