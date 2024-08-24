#include <flint/acb.h>
#include "R_flint.h"

void R_flint_acb_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	acb *x = (acb *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		acb_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP real, SEXP imaginary)
{
	unsigned long long int i,
		na = (unsigned long long int) XLENGTH(real),
		nb = (unsigned long long int) XLENGTH(imaginary),
		n  = RECYCLE2(na, nb);
	_R_flint_length_set(object, n);
	acb *x = (acb *) flint_calloc(n, sizeof(acb));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (TYPEOF(real) == INTSXP) {
		int *a = INTEGER(real), tmp;
		for (i = 0; i < n; ++i) {
			tmp = a[i % na];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_realref(x[i]), R_NaN);
			else
			arb_set_si(acb_realref(x[i]), tmp);
		}
	} else {
		double *a = REAL(real);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_realref(x[i]), a[i % na]);
	}
	if (TYPEOF(imaginary) == INTSXP) {
		int *b = INTEGER(imaginary), tmp;
		for (i = 0; i < n; ++i) {
			tmp = b[i % nb];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_imagref(x[i]), R_NaN);
			else
			arb_set_si(acb_imagref(x[i]), tmp);
		}
	} else {
		double *b = REAL(imaginary);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_imagref(x[i]), b[i % nb]);
	}
	return object;
}
