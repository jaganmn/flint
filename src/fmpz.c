#include <flint/fmpz.h>
#include "R_flint.h"

static
void R_flint_fmpz_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	fmpz *x = (fmpz *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		fmpz_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP value)
{
	/* FIXME: handle NA_INTEGER, NaN */
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	fmpz *x = flint_calloc(n, fmpz);
	R_CFinalizer_t f = (R_CFinalizer_t) &R_flint_fmpz_finalize;
	_R_flint_x_set(object, x, f);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value);
		for (i = 0; i < n; ++i)
			fmpz_set_si(x[i], y[i]);
	} else {
		double *y = REAL(value);
		for (i = 0; i < n; ++i)
			fmpz_set_d (x[i], y[i]);
	}
	return object;
}
