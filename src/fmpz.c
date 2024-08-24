#include <flint/fmpz.h>
#include "R_flint.h"

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
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_length_set(object, n);
	fmpz *x = (fmpz *) flint_calloc(n, sizeof(fmpz));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	if (TYPEOF(value) == INTSXP) {
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_si(x[i], tmp);
		}
	} else {
		double *y = REAL(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_d (x[i], (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
	}
	return object;
}
