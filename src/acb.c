#include <flint/acb.h>
#include "R_flint.h"

void R_flint_acb_finalize(SEXP x)
{
	unsigned long long int i, n;
	uconv(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	acb_ptr p = (acb_ptr) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		acb_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_realmid, SEXP s_realrad,
                            SEXP s_imagmid, SEXP s_imagrad)
{
	unsigned long long int i, n, nrm = 1, nrr = 1, nim = 1, nir = 1;
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
	R_flint_set_length(object, n);
	acb_ptr y = (acb_ptr) ((n) ? flint_calloc(n, sizeof(acb_t)) : 0);
	R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (s_realmid != R_NilValue || s_realrad != R_NilValue ||
	    s_imagmid != R_NilValue || s_imagrad != R_NilValue) {
		switch (TYPEOF(s_realmid)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				arf_zero(arb_midref(acb_realref(y + i)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_realmid = Rf_coerceVector(s_realmid, INTSXP);
		case INTSXP:
		{
			int *xrm = INTEGER(s_realmid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xrm[i % nrm];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(acb_realref(y + i)));
				else
				arf_set_si(arb_midref(acb_realref(y + i)), tmp);
			}
			break;
		}
		case REALSXP:
		{
			double *xrm = REAL(s_realmid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xrm[i % nrm];
				arf_set_d(arb_midref(acb_realref(y + i)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_realrad)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				mag_zero(arb_radref(acb_realref(y + i)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_realrad = Rf_coerceVector(s_realrad, INTSXP);
		case INTSXP:
		{
			int *xrr = INTEGER(s_realrad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xrr[i % nrr];
				if (tmp == NA_INTEGER)
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_ui(arb_radref(acb_realref(y + i)), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			double *xrr = REAL(s_realrad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xrr[i % nrr];
				if (ISNAN(tmp))
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_d(arb_radref(acb_realref(y + i)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_imagmid)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				arf_zero(arb_midref(acb_imagref(y + i)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_imagmid = Rf_coerceVector(s_imagmid, INTSXP);
		case INTSXP:
		{
			int *xim = INTEGER(s_imagmid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xim[i % nim];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(acb_imagref(y + i)));
				else
				arf_set_si(arb_midref(acb_imagref(y + i)), tmp);
			}
			break;
		}
		case REALSXP:
		{
			double *xim = REAL(s_imagmid), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xim[i % nim];
				arf_set_d(arb_midref(acb_imagref(y + i)), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_imagrad)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				mag_zero(arb_radref(acb_imagref(y + i)));
			break;
		case RAWSXP:
		case LGLSXP:
			s_imagrad = Rf_coerceVector(s_imagrad, INTSXP);
		case INTSXP:
		{
			int *xir = INTEGER(s_imagrad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xir[i % nir];
				if (tmp == NA_INTEGER)
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_ui(arb_radref(acb_imagref(y + i)), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			double *xir = REAL(s_imagrad), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xir[i % nir];
				if (ISNAN(tmp))
				Rf_error("NaN not representable by '%s'", "mag");
				else
				mag_set_d(arb_radref(acb_imagref(y + i)), tmp);
			}
			break;
		}
		}
	} else if (s_x != R_NilValue) {
		Rcomplex *x = COMPLEX(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			arf_set_d(arb_midref(acb_realref(y + i)), tmp.r);
			mag_zero(arb_radref(acb_realref(y + i)));
			arf_set_d(arb_midref(acb_imagref(y + i)), tmp.i);
			mag_zero(arb_radref(acb_imagref(y + i)));
		}
	} else
		for (i = 0; i < n; ++i)
			acb_zero(y + i);
	return object;
}

SEXP R_flint_acb_nflint(SEXP from, SEXP s_rnd)
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
	R_do_slot_assign(imag, R_flint_symbol_mid, imagmid);
	R_do_slot_assign(real, R_flint_symbol_rad, realrad);
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

SEXP R_flint_acb_vector(SEXP from, SEXP s_rnd)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "acb", (long long int) R_XLEN_T_MAX);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, __func__);
	SEXP to = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acb_ptr x = (acb_ptr) R_flint_get_x(from);
	Rcomplex *y = COMPLEX(to);
	arf_t lbm, ubm;
	arf_ptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	int w = 1;
	for (i = 0; i < n; ++i) {
		m = arb_midref(acb_realref(x + i));
		if (arf_is_nan(m))
			y[i].r = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[i].r = arf_get_d(m, rnd);
		else {
			y[i].r = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = arb_midref(acb_imagref(x + i));
		if (arf_is_nan(m))
			y[i].i = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[i].i = arf_get_d(m, rnd);
		else {
			y[i].i = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	UNPROTECT(1);
	return to;
}
