#include <flint/flint.h>
#include <flint/mag.h>
#include "flint.h"

void R_flint_mag_finalize(SEXP x)
{
	unsigned long long int i, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	mag_ptr p = (mag_ptr) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		mag_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	mag_ptr y = (mag_ptr) ((n) ? flint_calloc(n, sizeof(mag_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (i = 0; i < n; ++i)
			mag_zero(y + i);
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
			Rf_error(_("NaN not representable by '%s'"), "mag");
			else
			mag_set_ui(y + i, (ulong) ((tmp < 0) ? -tmp : tmp));
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (ISNAN(tmp))
			Rf_error(_("NaN not representable by '%s'"), "mag");
			else
			mag_set_d(y + i, tmp);
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_mag_nmag(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "mag", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	mag_ptr x = (mag_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
	mag_t ub;
	mag_init(ub);
	mag_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (mag_cmp(x + i, ub) < 0)
			y[i] = mag_get_d(x + i);
		else {
			y[i] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	mag_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_mag_vector(SEXP from)
{
	SEXP to = R_flint_mag_nmag(from);
	CLEAR_ATTRIB(to);
	return to;
}
