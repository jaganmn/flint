#include <flint/arb.h>
#include "R_flint.h"

void R_flint_arb_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_get_length(object);
	arb_ptr x = (arb_ptr) _R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		arb_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP value)
{
	unsigned long long int i, n = (unsigned long long int) XLENGTH(value);
	_R_flint_set_length(object, n);
	arb_ptr x = (arb_ptr) flint_calloc(n, sizeof(arb_t));
	_R_flint_set_x(object, x, (R_CFinalizer_t) &R_flint_arb_finalize);
	switch (TYPEOF(value)) {
	case INTSXP:
	{
		int *y = INTEGER(value), tmp;
		for (i = 0; i < n; ++i) {
			tmp = y[i];
			if (tmp == NA_INTEGER)
			arb_set_d (x + i, R_NaN);
			else
			arb_set_si(x + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *y = REAL(value);
		for (i = 0; i < n; ++i)
			arb_set_d (x + i, y[i]);
		break;
	}
	default:
		ERROR_INVALID_TYPE(value, __func__);
		break;
	}
	return object;
}

SEXP R_flint_arb_list(SEXP from, SEXP mode)
{
	unsigned long long int i, n = _R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(mode, __func__);
	SEXP to = PROTECT(Rf_allocVector(VECSXP, 2)),
		nms = PROTECT(Rf_allocVector(STRSXP, 2)),
		mid = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n)),
		rad = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	SET_VECTOR_ELT(to, 0, mid);
	SET_VECTOR_ELT(to, 1, rad);
	SET_STRING_ELT(nms, 0, Rf_mkChar("mid"));
	SET_STRING_ELT(nms, 1, Rf_mkChar("rad"));
	Rf_setAttrib(to, R_NamesSymbol, nms);
	arb_ptr x = (arb_ptr) _R_flint_get_x(from);
	double *y_m = REAL(mid), *y_r = REAL(rad);
	arf_t mlb, mub;
	arf_ptr m;
	arf_init(mlb);
	arf_init(mub);
	arf_set_ui_2exp_si(mub, 1U, DBL_MAX_EXP);
	arf_neg(mlb, mub);
	mag_t rub;
	mag_ptr r;
	mag_init(rub);
	mag_set_ui_2exp_si(rub, 1U, DBL_MAX_EXP);
	int w = 1;
	for (i = 0; i < n; ++i) {
		m = arb_midref(x + i);
		if (arf_is_nan(m))
			y_m[i] = R_NaN;
		else if (arf_cmp(m, mlb) > 0 && arf_cmp(m, mub) < 0)
			y_m[i] = arf_get_d(m, rnd);
		else {
			y_m[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(x + i);
		if (mag_cmp(r, rub) < 0)
			y_r[i] = mag_get_d(r);
		else {
			y_r[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(mlb);
	arf_clear(mub);
	mag_clear(rub);
	UNPROTECT(4);
	return to;
}
