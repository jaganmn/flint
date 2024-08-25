#include <flint/acb.h>
#include "R_flint.h"

void R_flint_acb_finalize(SEXP object)
{
	unsigned long long int i, n = _R_flint_length_get(object);
	acb *x = (acb *) _R_flint_x_get(object);
	for (i = 0; i < n; ++i)
		acb_clear(x[i]);
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
	acb *x = (acb *) flint_calloc(n, sizeof(acb));
	_R_flint_x_set(object, x, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (TYPEOF(real) == INTSXP) {
		int *a = INTEGER(real), tmp;
		for (i = 0; i < n; ++i) {
			tmp = a[i % na];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_realref(x[i]), R_NaN);
			else
			arb_set_si(acb_realref(x[i]), tmp);
		}
	} else {
		double *a = REAL(real);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_realref(x[i]), a[i % na]);
	}
	if (TYPEOF(imaginary) == INTSXP) {
		int *b = INTEGER(imaginary), tmp;
		for (i = 0; i < n; ++i) {
			tmp = b[i % nb];
			if (tmp == NA_INTEGER)
			arb_set_d (acb_imagref(x[i]), R_NaN);
			else
			arb_set_si(acb_imagref(x[i]), tmp);
		}
	} else {
		double *b = REAL(imaginary);
		for (i = 0; i < n; ++i)
			arb_set_d (acb_imagref(x[i]), b[i % nb]);
	}
	return object;
}

SEXP R_flint_acb_complex(SEXP from, SEXP mode)
{
	unsigned long long int i, n = _R_flint_length_get(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(mode);
	SEXP to = PROTECT(allocVector(CPLXSXP, (R_xlen_t) n));
	acb *x = (acb *) _R_flint_x_get(from);
	Rcomplex *y = COMPLEX(to);
	int w = 1;
	arf_t lb, ub, m;
	arf_init(lb);
	arf_init(ub);
	arf_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	arf_neg(lb, ub);
	for (i = 0; i < n; ++i) {
		m = arb_midref(acb_realref(x[i]));
		if (arf_is_nan(m))
			y[i].r = R_NaN;
		else if (arf_cmp(m, lb) > 0 && arf_cmp(m, ub) < 0)
			y[i].r = arf_get_d(m, rnd);
		else {
			y[i].r = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			if (w) {
				Rf_warning("-Inf or Inf introduced by coercion to range of \"%s\"",
				           "double");
				w = 0;
			}
		}
		m = arb_midref(acb_imagref(x[i]));
		if (arf_is_nan(m))
			y[i].i = R_NaN;
		else if (arf_cmp(m, lb) > 0 && arf_cmp(m, ub) < 0)
			y[i].i = arf_get_d(m, rnd);
		else {
			y[i].i = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
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
