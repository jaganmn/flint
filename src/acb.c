#include <flint/acb.h>
#include "R_flint.h"

void R_flint_acb_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	acb_ptr x = (acb_ptr) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		acb_clear(x + i);
	flint_free(x);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP real, SEXP imaginary)
{
	unsigned long long int i,
		na = (unsigned long long int) XLENGTH(real),
		nb = (unsigned long long int) XLENGTH(imaginary),
		n  = RECYCLE2(na, nb);
	_R_flint_length_set(object, n);
	acb_ptr x = (acb_ptr) flint_calloc(n, sizeof(acb_t));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_acb_finalize);
	switch (TYPEOF(real)) {
	case INTSXP:
	{
		int *a = INTEGER(real), tmp;
		for (i = 0; i < n; ++i) {
			tmp = a[i % na];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_realref(x + i), R_NaN);
			else
			arb_set_si(acb_realref(x + i), tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *a = REAL(real);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_realref(x + i), a[i % na]);
		break;
	}
	default:
		ERROR_INVALID_TYPE(real, __func__);
		break;
	}
	switch (TYPEOF(imaginary)) {
	case INTSXP:
	{
		int *b = INTEGER(imaginary), tmp;
		for (i = 0; i < n; ++i) {
			tmp = b[i % nb];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_imagref(x + i), R_NaN);
			else
			arb_set_si(acb_imagref(x + i), tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *b = REAL(imaginary);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_imagref(x + i), b[i % nb]);
		break;
	}
	default:
		ERROR_INVALID_TYPE(imaginary, __func__);
		break;
	}
	return object;
}

SEXP R_flint_acb_list(SEXP from, SEXP mode)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(mode);
	SEXP to = PROTECT(Rf_allocVector(VECSXP, 3)),
		nms = PROTECT(Rf_allocVector(STRSXP, 3)),
		real_mid = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n)),
		real_rad = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n)),
		imag_mid = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n)),
		imag_rad = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	SET_VECTOR_ELT(to, 0, real_mid);
	SET_VECTOR_ELT(to, 1, real_rad);
	SET_VECTOR_ELT(to, 2, imag_mid);
	SET_VECTOR_ELT(to, 3, imag_rad);
	SET_STRING_ELT(nms, 0, Rf_mkChar("real.mid"));
	SET_STRING_ELT(nms, 1, Rf_mkChar("real.rad"));
	SET_STRING_ELT(nms, 2, Rf_mkChar("imag.mid"));
	SET_STRING_ELT(nms, 3, Rf_mkChar("imag.rad"));
	Rf_setAttrib(to, R_NamesSymbol, nms);
	acb_ptr x = (acb_ptr) _R_flint_x_get(from);
	double
		*y_rm = REAL(real_mid), *y_rr = REAL(real_rad),
		*y_im = REAL(imag_mid), *y_ir = REAL(imag_rad);
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
		m = arb_midref(acb_realref(x + i));
		if (arf_is_nan(m))
			y_rm[i] = R_NaN;
		else if (arf_cmp(m, mlb) > 0 && arf_cmp(m, mub) < 0)
			y_rm[i] = arf_get_d(m, rnd);
		else {
			y_rm[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_realref(x + i));
		if (mag_cmp(r, rub) < 0)
			y_rr[i] = mag_get_d(r);
		else {
			y_rr[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = arb_midref(acb_imagref(x + i));
		if (arf_is_nan(m))
			y_im[i] = R_NaN;
		else if (arf_cmp(m, mlb) > 0 && arf_cmp(m, mub) < 0)
			y_im[i] = arf_get_d(m, rnd);
		else {
			y_im[i] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_imagref(x + i));
		if (mag_cmp(r, rub) < 0)
			y_ir[i] = mag_get_d(r);
		else {
			y_ir[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(mlb);
	arf_clear(mub);
	mag_clear(rub);
	UNPROTECT(6);
	return to;
}
