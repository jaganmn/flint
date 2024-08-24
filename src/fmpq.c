#include <flint/fmpq.h>
#include "R_flint.h"

void R_flint_fmpq_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	fmpq *x = (fmpq *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		fmpq_clear(x[i]);
	flint_free(x);
	return;
}

SEXP R_flint_fmpq_initialize(SEXP object, SEXP numerator, SEXP denominator)
{
	unsigned long long int i,
		np = (unsigned long long int) XLENGTH(numerator),
		nq = (unsigned long long int) XLENGTH(denominator),
		n  = RECYCLE2(np, nq);
	_R_flint_length_set(object, n);
	fmpq *x = (fmpq *) flint_calloc(n, sizeof(fmpq));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (TYPEOF(numerator) == INTSXP) {
		int *p = INTEGER(numerator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = p[i % np];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_si(fmpq_numref(x[i]), tmp);
		}
	} else {
		double *p = REAL(numerator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = p[i % np];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_d (fmpq_numref(x[i]), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
	}
	if (TYPEOF(denominator) == INTSXP) {
		int *q = INTEGER(denominator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = q[i % nq];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_si(fmpq_denref(x[i]), tmp);
		}
	} else {
		double *q = REAL(denominator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = q[i % nq];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by 'fmpz'");
			else
			fmpz_set_d (fmpq_denref(x[i]), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
	}
	for (i = 0; i < n; ++i) {
		if (fmpz_is_zero(fmpq_denref(x[i])))
		Rf_error("zero denominator not valid in canonical 'fmpq'");
		else
		fmpq_canonicalise(x[i]);
	}
	return object;
}
