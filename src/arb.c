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

SEXP R_flint_arb_double(SEXP from, SEXP mode)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(mode);
	SEXP to = PROTECT(allocVector(REALSXP, (R_xlen_t) n));
	arb *x = (arb *) _R_flint_x_get(from);
	double *y = REAL(to);
	int w = 1;
	arf_t lb, ub, m;
	arf_init(lb);
	arf_init(ub);
	arf_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	arf_neg(lb, ub);
	for (i = 0; i < n; ++i) {
		m = arb_midref(x[i]);
		if (arf_is_nan(m))
			y[i] = R_NaN;
		else if (arf_cmp(m, lb) > 0 && arf_cmp(m, ub) < 0)
			y[i] = arf_get_d(m, rnd);
		else {
			y[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			if (w) {
				Rf_warning("-Inf or Inf introduced by coercion to range of \"%s\"",
				           "double");
				w = 0;
			}
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return to;
}
