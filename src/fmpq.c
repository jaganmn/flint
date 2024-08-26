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
		int *y_p = INTEGER(numerator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y_p[i % np];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "fmpq");
			else
			fmpz_set_si(fmpq_numref(x[i]), tmp);
		}
	} else {
		double *y_p = REAL(numerator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y_p[i % np];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "fmpq");
			else
			fmpz_set_d (fmpq_numref(x[i]), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
	}
	if (TYPEOF(denominator) == INTSXP) {
		int *y_q = INTEGER(denominator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y_q[i % nq];
			if (tmp == NA_INTEGER)
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "fmpq");
			else
			fmpz_set_si(fmpq_denref(x[i]), tmp);
		}
	} else {
		double *y_q = REAL(denominator), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y_q[i % nq];
			if (!R_FINITE(tmp))
			Rf_error("NaN, Inf, -Inf not representable by '%s'", "fmpq");
			else
			fmpz_set_d (fmpq_denref(x[i]), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
	}
	for (i = 0; i < n; ++i) {
		if (fmpz_is_zero(fmpq_denref(x[i])))
		Rf_error("zero denominator not valid in canonical '%s'", "fmpq");
		else
		fmpq_canonicalise(x[i]);
	}
	return object;
}

SEXP R_flint_fmpq_integer(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(INTSXP, (R_xlen_t) n));
	fmpq *x = (fmpq *) _R_flint_x_get(from);
	int *y = INTEGER(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpq_cmp_fmpz(x[i], lb) > 0 && fmpq_cmp_fmpz(x[i], ub) < 0)
			y[i] = (int) fmpq_get_d(x[i]);
		else {
			y[i] = NA_INTEGER;
			OOB_INTEGER(w);
		}
	}
	fmpq_clear(lb);
	fmpq_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_fmpq_double(SEXP from)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(allocVector(REALSXP, (R_xlen_t) n));
	fmpq *x = (fmpq *) _R_flint_x_get(from);
	double *y = REAL(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_one_2exp(ub, DBL_MAX_EXP);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpq_cmp_fmpz(x[i], lb) > 0 && fmpq_cmp_fmpz(x[i], ub) < 0)
			y[i] = fmpq_get_d(x[i]);
		else {
			y[i] = (fmpq_sgn(x[i]) < 0) ? R_NegInf : R_PosInf;
			OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}
