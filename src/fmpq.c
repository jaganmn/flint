#include <flint/fmpq.h>
#include "R_flint.h"

static
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
	/* FIXME: handle NA_INTEGER, NaN */
	unsigned long long int i,
		np = (unsigned long long int) XLENGTH(numerator),
		nq = (unsigned long long int) XLENGTH(denominator),
		n  = (np == 0 || nq == 0) ? 0 : (np < nq) ? nq : np;
	_R_flint_length_set(object, n);
	fmpq *x = flint_calloc(n, fmpq);
	R_CFinalizer_t f = (R_CFinalizer_t) &R_flint_fmpq_finalize;
	_R_flint_x_set(object, x, f);
	if (TYPEOF(numerator) == INTSXP) {
		int *p = INTEGER(numerator);
		for (i = 0; i < n; ++i)
			fmpz_set_si(fmpq_numref(x[i]), p[i % np]);
	} else {
		double *p = REAL(numerator);
		for (i = 0; i < n; ++i)
			fmpz_set_d (fmpq_numref(x[i]), p[i % np]);
	}
	if (TYPEOF(denominator) == INTSXP) {
		int *q = INTEGER(denominator);
		for (i = 0; i < n; ++i)
			fmpz_set_si(fmpq_denref(x[i]), q[i % nq]);
	} else {
		double *q = REAL(denominator);
		for (i = 0; i < n; ++i)
			fmpz_set_d (fmpq_denref(x[i]), q[i % nq]);
	}
	for (i = 0; i < n; ++i)
		fmpq_canonicalise(x[i]);
	return object;
}
