#include <flint/arb.h>
#include "R_flint.h"

void R_flint_arb_finalize(SEXP object)
{
	unsigned long long int i, n = R_flint_get_length(object);
	arb_ptr x = (arb_ptr) R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		arb_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n = asLength(s_length, s_x, __func__);
	R_flint_set_length(object, n);
	arb_ptr y = (arb_ptr) flint_calloc(n, sizeof(arb_t));
	R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_arb_finalize);
	switch (TYPEOF(s_x)) {
	case INTSXP:
	{
		int *x = INTEGER(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (tmp == NA_INTEGER)
			arb_set_d(y + i, R_NaN);
			else
			arb_set_si(y + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x);
		for (i = 0; i < n; ++i)
			arb_set_d(y + i, x[i]);
		break;
	}
	default:
		ERROR_INVALID_TYPE(s_x, __func__);
		break;
	}
	return object;
}

SEXP R_flint_arb_narb(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, __func__);
	SEXP to = PROTECT(newObject("narb")),
		mid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		rad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	R_do_slot_assign(to, R_flint_symbol_mid, mid);
	R_do_slot_assign(to, R_flint_symbol_rad, rad);
	arb_ptr x = (arb_ptr) R_flint_get_x(from);
	double *ym = REAL(mid), *yr = REAL(rad);
	arf_t lbm, ubm;
	arf_ptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	mag_t ubr;
	mag_ptr r;
	mag_init(ubr);
	mag_set_ui_2exp_si(ubr, 1U, DBL_MAX_EXP);
	int w = 1;
	for (i = 0; i < n; ++i) {
		m = arb_midref(x + i);
		if (arf_is_nan(m))
			ym[i] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			ym[i] = arf_get_d(m, rnd);
		else {
			ym[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(x + i);
		if (mag_cmp(r, ubr) < 0)
			yr[i] = mag_get_d(r);
		else {
			yr[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	mag_clear(ubr);
	UNPROTECT(3);
	return to;
}
