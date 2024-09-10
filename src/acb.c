#include <flint/acb.h>
#include "R_flint.h"

void R_flint_acb_finalize(SEXP object)
{
	unsigned long long int i, n = R_flint_get_length(object);
	acb_ptr x = (acb_ptr) R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		acb_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_real, SEXP s_imag)
{
	unsigned long long int i, n, na, nb;
	if (s_real != R_NilValue && s_imag != R_NilValue) {
		na = (unsigned long long int) XLENGTH(s_real),
		nb = (unsigned long long int) XLENGTH(s_imag),
		n = RECYCLE2(na, nb);
	} else if (TYPEOF(s_x) == INTSXP || TYPEOF(s_x) == REALSXP) {
		na = (unsigned long long int) XLENGTH(s_x);
		n = na;
		s_real = s_x;
		s_imag = NULL;
	} else {
		n = asLength(s_length, s_x, __func__);
		s_real = NULL;
		s_imag = NULL;
	}
	R_flint_set_length(object, n);
	acb_ptr y = (acb_ptr) flint_calloc(n, sizeof(acb_t));
	R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (s_real)
	switch (TYPEOF(s_real)) {
	case INTSXP:
	{
		int *xa = INTEGER(s_real), tmp;
		for (i = 0; i < n; ++i) {
			tmp = xa[i % na];
			if (tmp == NA_INTEGER)
			arb_set_d(acb_realref(y + i), R_NaN);
			else
			arb_set_si(acb_realref(y + i), tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *xa = REAL(s_real), tmp;
		for (i = 0; i < n; ++i) {
			tmp = xa[i % na];
			arb_set_d(acb_realref(y + i), tmp);
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(s_real, __func__);
		break;
	}
	else
	switch (TYPEOF(s_x)) {
	case CPLXSXP:
	{
		Rcomplex *x = COMPLEX(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			arb_set_d(acb_realref(y + i), tmp.r);
			arb_set_d(acb_imagref(y + i), tmp.i);
		}
	}
	case NILSXP:
	case INTSXP:
	case REALSXP:
		break;
	default:
		ERROR_INVALID_TYPE(s_x, __func__);
		break;
	}
	if (s_imag)
	switch (TYPEOF(s_imag)) {
	case INTSXP:
	{
		int *xb = INTEGER(s_imag), tmp;
		for (i = 0; i < n; ++i) {
			tmp = xb[i % nb];
			if (tmp == NA_INTEGER)
			arb_set_d(acb_imagref(y + i), R_NaN);
			else
			arb_set_si(acb_imagref(y + i), tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *xb = REAL(s_imag), tmp;
		for (i = 0; i < n; ++i) {
			tmp = xb[i % nb];
			arb_set_d(acb_imagref(y + i), tmp);
		}
		break;
	}
	default:
		ERROR_INVALID_TYPE(s_imag, __func__);
		break;
	}
	else
	switch (TYPEOF(s_x)) {
	case NILSXP:
	case INTSXP:
	case REALSXP:
		for (i = 0; i < n; ++i)
			arb_zero(acb_imagref(y + i));
	case CPLXSXP:
		break;
	default:
		ERROR_INVALID_TYPE(s_x, __func__);
		break;
	}
	return object;
}

SEXP R_flint_acb_nacb(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, __func__);
	SEXP to = PROTECT(newObject("nacb")),
		real = PROTECT(R_do_slot(to, R_flint_symbol_real)),
		imag = PROTECT(R_do_slot(to, R_flint_symbol_imag)),
		realmid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		imagmid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		realrad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n)),
		imagrad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	R_do_slot_assign(real, R_flint_symbol_mid, realmid);
	R_do_slot_assign(imag, R_flint_symbol_mid, imagrad);
	R_do_slot_assign(real, R_flint_symbol_rad, realmid);
	R_do_slot_assign(imag, R_flint_symbol_rad, imagrad);
	acb_ptr x = (acb_ptr) R_flint_get_x(from);
	double *yrm = REAL(realmid), *yim = REAL(imagmid),
		*yrr = REAL(realrad), *yir = REAL(imagrad);
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
		m = arb_midref(acb_realref(x + i));
		if (arf_is_nan(m))
			yrm[i] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			yrm[i] = arf_get_d(m, rnd);
		else {
			yrm[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = arb_midref(acb_imagref(x + i));
		if (arf_is_nan(m))
			yim[i] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			yim[i] = arf_get_d(m, rnd);
		else {
			yim[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_realref(x + i));
		if (mag_cmp(r, ubr) < 0)
			yrr[i] = mag_get_d(r);
		else {
			yrr[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_imagref(x + i));
		if (mag_cmp(r, ubr) < 0)
			yir[i] = mag_get_d(r);
		else {
			yir[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	mag_clear(ubr);
	UNPROTECT(7);
	return to;
}
