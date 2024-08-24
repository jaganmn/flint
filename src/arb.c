#include <flint/arb.h>
#include "R_flint.h"

void R_flint_arb_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	arb *x = (arb *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		arb_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	arb *x = (arb *) flint_calloc(n, sizeof(arb));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_arb_finalize);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			arb_set_d (x[i], R_NaN);
			else
			arb_set_si(x[i], tmp);
		}
	} else {
		double *y = REAL(value);
		for (i = 0; i < n; ++i)
			arb_set_d (x[i], y[i]);
	}
	return object;
}
