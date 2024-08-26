#include <flint/arf.h>
#include "R_flint.h"

int asRnd(SEXP x)
{

#define ARF_RND_VALID(c) \
	switch (c) { \
	case ARF_RND_DOWN: \
	case ARF_RND_UP: \
	case ARF_RND_FLOOR: \
	case ARF_RND_CEIL: \
	case ARF_RND_NEAR: \
		return c; \
	default: \
		break; \
	}

	switch (TYPEOF(x)) {
	case INTSXP:
	{
		int tmp;
		if (XLENGTH(x) > 0 && (tmp = INTEGER(x)[0]) != NA_INTEGER)
			ARF_RND_VALID(tmp);
		break;
	}
	case REALSXP:
	{
		double tmp;
		if (XLENGTH(x) > 0 && !ISNAN(tmp = REAL(x)[0]) &&
		    tmp > INT_MIN - 1.0 && tmp < INT_MAX + 1.0)
			ARF_RND_VALID((int) tmp);
		break;
	}
	default:
		break;
	}

#undef ARF_RND_VALID

	Rf_error("invalid rounding mode");
	return 0;
}

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

SEXP R_flint_arf_double(SEXP from, SEXP mode)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arf", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(mode);
	SEXP to = PROTECT(allocVector(REALSXP, (R_xlen_t) n));
	arf *x = (arf *) _R_flint_x_get(from);
	double *y = REAL(to);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	arf_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (arf_is_nan(x[i]))
			y[i] = R_NaN;
		else if (arf_cmp(x[i], lb) > 0 && arf_cmp(x[i], ub) < 0)
			y[i] = arf_get_d(x[i], rnd);
		else {
			y[i] = (arf_sgn(x[i]) < 0) ? R_NegInf : R_PosInf;
			OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return to;
}
