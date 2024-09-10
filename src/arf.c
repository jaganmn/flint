#include <flint/arf.h>
#include "R_flint.h"

int asRnd(SEXP rnd, const char *where)
{
	if (TYPEOF(rnd) == STRSXP && XLENGTH(rnd) > 0 &&
	    (rnd = STRING_ELT(rnd, 0)) != NA_STRING) {
		const char *s = CHAR(rnd);
		if (strcmp(s, "down") == 0)
			return ARF_RND_DOWN;
		else if (strcmp(s, "up") == 0)
			return ARF_RND_UP;
		else if (strcmp(s, "floor") == 0)
			return ARF_RND_FLOOR;
		else if (strcmp(s, "ceil") == 0)
			return ARF_RND_CEIL;
		else if (strcmp(s, "near") == 0)
			return ARF_RND_NEAR;
	}
	Rf_error("invalid '%s' in '%s'", "rnd", where);
	return 0;
}

void R_flint_arf_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_get_length(object);
	arf_ptr x = (arf_ptr) _R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		arf_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n = asLength(s_length, s_x, __func__);
	_R_flint_set_length(object, n);
	arf_ptr y = (arf_ptr) flint_calloc(n, sizeof(arf_t));
	_R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_arf_finalize);
	switch (TYPEOF(s_x)) {
	case INTSXP:
	{
		int *x = INTEGER(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (tmp == NA_INTEGER)
			arf_set_d(y + i, R_NaN);
			else
			arf_set_si(y + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			arf_set_d(y + i, tmp);
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(s_x, __func__);
		break;
	}
	return object;
}

SEXP R_flint_arf_narf(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arf", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, __func__);
	SEXP to = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n));
	arf_ptr x = (arf_ptr) _R_flint_get_x(from);
	double *y = REAL(to);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	arf_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (arf_is_nan(x + i))
			y[i] = R_NaN;
		else if (arf_cmp(x + i, lb) > 0 && arf_cmp(x + i, ub) < 0)
			y[i] = arf_get_d(x + i, rnd);
		else {
			y[i] = (arf_sgn(x + i) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return to;
}
