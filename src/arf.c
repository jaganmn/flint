#include <mpfr.h>
#include <flint/flint.h>
#include <flint/arf.h>
#include "flint.h"

int asRnd(SEXP rnd, int gnu, const char *where)
{
	if (TYPEOF(rnd) == STRSXP && XLENGTH(rnd) > 0 &&
	    (rnd = STRING_ELT(rnd, 0)) != NA_STRING) {
		switch (CHAR(rnd)[0]) {
		case 'N': case 'n':
			return (gnu) ? MPFR_RNDN : ARF_RND_NEAR;
		case 'Z': case 'z':
			return (gnu) ? MPFR_RNDZ : ARF_RND_DOWN;
		case 'U': case 'u':
			return (gnu) ? MPFR_RNDU : ARF_RND_CEIL;
		case 'D': case 'd':
			return (gnu) ? MPFR_RNDD : ARF_RND_FLOOR;
		case 'A': case 'a':
			return (gnu) ? MPFR_RNDA : ARF_RND_UP;
		}
	}
	Rf_error(_("invalid '%s' in '%s'"), "rnd", where);
	return 0;
}

void R_flint_arf_finalize(SEXP x)
{
	unsigned long long int i, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	arf_ptr p = (arf_ptr) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		arf_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	arf_ptr y = (arf_ptr) ((n) ? flint_calloc(n, sizeof(arf_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (i = 0; i < n; ++i)
			arf_zero(y + i);
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		int *x = INTEGER(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (tmp == NA_INTEGER)
			arf_nan(y + i);
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
	}
	return object;
}

SEXP R_flint_arf_narf(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "arf", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n));
	arf_ptr x = (arf_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
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

SEXP R_flint_arf_vector(SEXP from, SEXP s_rnd)
{
	SEXP to = R_flint_arf_narf(from, s_rnd);
	CLEAR_ATTRIB(to);
	return to;
}
