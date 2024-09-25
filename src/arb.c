#include <flint/arb.h>
#include "R_flint.h"

void R_flint_arb_finalize(SEXP x)
{
	unsigned long long int i, n;
	uconv(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	arb_ptr p = (arb_ptr) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		arb_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_mid, SEXP s_rad)
{
	unsigned long long int i, n, nm = 1, nr = 1;
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		if (s_mid != R_NilValue) {
			checkType(s_mid, R_flint_sexptypes + 1, __func__);
			nm = (unsigned long long int) XLENGTH(s_mid);
		}
		if (s_rad != R_NilValue) {
			checkType(s_rad, R_flint_sexptypes + 1, __func__);
			nr = (unsigned long long int) XLENGTH(s_rad);
		}
		n = RECYCLE2(nm, nr);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
		s_mid = s_x;
		nm = n;
	} else
		n = asLength(s_length, __func__);
	R_flint_set_length(object, n);
	arb_ptr y = (arb_ptr) ((n) ? flint_calloc(n, sizeof(arb_t)) : 0);
	R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		switch (TYPEOF(s_mid)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				arf_zero(arb_midref(y + i));
			break;
		case RAWSXP:
		case LGLSXP:
			s_mid = Rf_coerceVector(s_mid, INTSXP);
		case INTSXP:
		{
			int *xm = INTEGER(s_mid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xm[i % nm];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(y + i));
				else
				arf_set_si(arb_midref(y + i), tmp);
			}
			break;
		}
		case REALSXP:
		{
			double *xm = REAL(s_mid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xm[i % nm];
				arf_set_d(arb_midref(y + i), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_rad)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				mag_zero(arb_radref(y + i));
			break;
		case RAWSXP:
		case LGLSXP:
			s_rad = Rf_coerceVector(s_rad, INTSXP);
		case INTSXP:
		{
			int *x = INTEGER(s_rad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = x[i];
				if (tmp == NA_INTEGER)
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_ui(arb_radref(y + i), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			double *x = REAL(s_rad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = x[i];
				if (ISNAN(tmp))
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_d(arb_radref(y + i), tmp);
			}
			break;
		}
		}
	} else
		for (i = 0; i < n; ++i)
			arb_zero(y + i);
	return object;
}

SEXP R_flint_arb_nflint(SEXP from, SEXP s_rnd)
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

SEXP R_flint_arb_vector(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "arb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, __func__);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arb_ptr x = (arb_ptr) R_flint_get_x(from);
	double *y = REAL(to);
	arf_t lbm, ubm;
	arf_ptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	int w = 1;
	for (i = 0; i < n; ++i) {
		m = arb_midref(x + i);
		if (arf_is_nan(m))
			y[i] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[i] = arf_get_d(m, rnd);
		else {
			y[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	UNPROTECT(1);
	return to;
}
