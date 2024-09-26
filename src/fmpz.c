#include <flint/fmpz.h>
#include "R_flint.h"

void R_flint_fmpz_finalize(SEXP x)
{
	unsigned long long int i, n;
	uconv(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	fmpz *p = (fmpz *) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		fmpz_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	fmpz *y = (fmpz *) ((n) ? flint_calloc(n, sizeof(fmpz)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		/* nothing to do */
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
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_si(y + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_d(y + i, (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_fmpz_nfmpz(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = (int) fmpz_get_si(x + i);
		else {
			y[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_fmpz_vector(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) R_flint_get_pointer(from);
	double *y = REAL(to);
	int w = 1;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = fmpz_get_d(x + i);
		else {
			y[i] = (fmpz_sgn(x + i) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}
