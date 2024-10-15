#include <flint/flint.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

void R_flint_acb_finalize(SEXP x)
{
	unsigned long long int j, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	acb_ptr p = (acb_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		acb_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_realmid, SEXP s_realrad,
                            SEXP s_imagmid, SEXP s_imagrad)
{
	unsigned long long int j, n, nrm = 1, nrr = 1, nim = 1, nir = 1;
	if (s_realmid != R_NilValue || s_realrad != R_NilValue ||
	    s_imagmid != R_NilValue || s_imagrad != R_NilValue) {
		if (s_realmid != R_NilValue) {
			checkType(s_realmid, R_flint_sexptypes + 1, __func__);
			nrm = (unsigned long long int) XLENGTH(s_realmid);
		}
		if (s_realrad != R_NilValue) {
			checkType(s_realrad, R_flint_sexptypes + 1, __func__);
			nrr = (unsigned long long int) XLENGTH(s_realrad);
		}
		if (s_imagmid != R_NilValue) {
			checkType(s_imagmid, R_flint_sexptypes + 1, __func__);
			nim = (unsigned long long int) XLENGTH(s_imagmid);
		}
		if (s_imagrad != R_NilValue) {
			checkType(s_imagrad, R_flint_sexptypes + 1, __func__);
			nir = (unsigned long long int) XLENGTH(s_imagrad);
		}
		n = RECYCLE4(nrm, nrr, nim, nir);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
		if (TYPEOF(s_x) != CPLXSXP) {
			s_realmid = s_x;
			nrm = n;
		}
	} else
		n = asLength(s_length, __func__);
	acb_ptr y = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	if (s_realmid != R_NilValue || s_realrad != R_NilValue ||
	    s_imagmid != R_NilValue || s_imagrad != R_NilValue) {
		switch (TYPEOF(s_realmid)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				arf_zero(arb_midref(acb_realref(y + j)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_realmid = Rf_coerceVector(s_realmid, INTSXP);
		case INTSXP:
		{
			const int *xrm = INTEGER_RO(s_realmid);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xrm[j % nrm];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(acb_realref(y + j)));
				else
				arf_set_si(arb_midref(acb_realref(y + j)), tmp);
			}
			break;
		}
		case REALSXP:
		{
			const double *xrm = REAL_RO(s_realmid);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xrm[j % nrm];
				arf_set_d(arb_midref(acb_realref(y + j)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_realrad)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				mag_zero(arb_radref(acb_realref(y + j)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_realrad = Rf_coerceVector(s_realrad, INTSXP);
		case INTSXP:
		{
			const int *xrr = INTEGER_RO(s_realrad);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xrr[j % nrr];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_ui(arb_radref(acb_realref(y + j)), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			const double *xrr = REAL_RO(s_realrad);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xrr[j % nrr];
				if (ISNAN(tmp))
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_d(arb_radref(acb_realref(y + j)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_imagmid)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				arf_zero(arb_midref(acb_imagref(y + j)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_imagmid = Rf_coerceVector(s_imagmid, INTSXP);
		case INTSXP:
		{
			const int *xim = INTEGER_RO(s_imagmid);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xim[j % nim];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(acb_imagref(y + j)));
				else
				arf_set_si(arb_midref(acb_imagref(y + j)), tmp);
			}
			break;
		}
		case REALSXP:
		{
			const double *xim = REAL_RO(s_imagmid);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xim[j % nim];
				arf_set_d(arb_midref(acb_imagref(y + j)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_imagrad)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				mag_zero(arb_radref(acb_imagref(y + j)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_imagrad = Rf_coerceVector(s_imagrad, INTSXP);
		case INTSXP:
		{
			const int *xir = INTEGER_RO(s_imagrad);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xir[j % nir];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_ui(arb_radref(acb_imagref(y + j)), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			const double *xir = REAL_RO(s_imagrad);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xir[j % nir];
				if (ISNAN(tmp))
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_d(arb_radref(acb_imagref(y + j)), tmp);
			}
			break;
		}
		}
	} else if (s_x != R_NilValue) {
		const Rcomplex *x = COMPLEX_RO(s_x);
		Rcomplex tmp;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			arf_set_d(arb_midref(acb_realref(y + j)), tmp.r);
			mag_zero(arb_radref(acb_realref(y + j)));
			arf_set_d(arb_midref(acb_imagref(y + j)), tmp.i);
			mag_zero(arb_radref(acb_imagref(y + j)));
		}
	} else
		for (j = 0; j < n; ++j)
			acb_zero(y + j);
	return object;
}

SEXP R_flint_acb_nacb(SEXP from, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(newObject("nacb")),
		real = PROTECT(R_do_slot(to, R_flint_symbol_real)),
		imag = PROTECT(R_do_slot(to, R_flint_symbol_imag)),
		realmid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		imagmid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		realrad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n)),
		imagrad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	R_do_slot_assign(real, R_flint_symbol_mid, realmid);
	R_do_slot_assign(imag, R_flint_symbol_mid, imagmid);
	R_do_slot_assign(real, R_flint_symbol_rad, realrad);
	R_do_slot_assign(imag, R_flint_symbol_rad, imagrad);
	acb_srcptr x = (acb_ptr) R_flint_get_pointer(from);
	double *yrm = REAL(realmid), *yim = REAL(imagmid),
		*yrr = REAL(realrad), *yir = REAL(imagrad);
	arf_t lbm, ubm;
	arf_srcptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	mag_t ubr;
	mag_srcptr r;
	mag_init(ubr);
	mag_set_ui_2exp_si(ubr, 1U, DBL_MAX_EXP);
	int w = 1;
	for (j = 0; j < n; ++j) {
		m = arb_midref(acb_realref(x + j));
		if (arf_is_nan(m))
			yrm[j] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			yrm[j] = arf_get_d(m, rnd);
		else {
			yrm[j] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = arb_midref(acb_imagref(x + j));
		if (arf_is_nan(m))
			yim[j] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			yim[j] = arf_get_d(m, rnd);
		else {
			yim[j] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_realref(x + j));
		if (mag_cmp(r, ubr) < 0)
			yrr[j] = mag_get_d(r);
		else {
			yrr[j] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(acb_imagref(x + j));
		if (mag_cmp(r, ubr) < 0)
			yir[j] = mag_get_d(r);
		else {
			yir[j] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	mag_clear(ubr);
	UNPROTECT(7);
	return to;
}

SEXP R_flint_acb_vector(SEXP from, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acb_srcptr x = (acb_ptr) R_flint_get_pointer(from);
	Rcomplex *y = COMPLEX(to);
	arf_t lbm, ubm;
	arf_srcptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	int w = 1;
	for (j = 0; j < n; ++j) {
		m = arb_midref(acb_realref(x + j));
		if (arf_is_nan(m))
			y[j].r = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[j].r = arf_get_d(m, rnd);
		else {
			y[j].r = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = arb_midref(acb_imagref(x + j));
		if (arf_is_nan(m))
			y[j].i = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[j].i = arf_get_d(m, rnd);
		else {
			y[j].i = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	UNPROTECT(1);
	return to;
}
