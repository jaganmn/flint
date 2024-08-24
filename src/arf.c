#include <flint/arf.h>
#include "R_flint.h"

void R_flint_arf_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	arf *x = (arf *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		arf_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	arf *x = (arf *) flint_calloc(n, sizeof(arf));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_arf_finalize);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value);
		for (i = 0; i < n; ++i)
			if (y[i] == NA_INTEGER)
			arf_set_d (x[i], R_NaN);
			else
			arf_set_si(x[i], y[i]);
	} else {
		double *y = REAL(value);
		for (i = 0; i < n; ++i)
			arf_set_d (x[i], y[i]);
	}
	return object;
}
