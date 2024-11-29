#include <flint/flint.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

void R_flint_acb_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
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
	ERROR_TOO_LONG(n);
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
	ERROR_TOO_LONG(n);
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

SEXP R_flint_acb_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	acb_srcptr
		x = (acb_ptr) R_flint_get_pointer(s_x),
		y = (acb_ptr) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long long int j, n = RECYCLE2(nx, ny);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	case  7: /*   "/" */
	{
		SEXP ans = newObject("acb");
		acb_ptr z = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				acb_add(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				acb_sub(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				acb_mul(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				acb_div(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 7: /*   "^" */
			for (j = 0; j < n; ++j)
				acb_pow(z + j, x + j % nx, y + j % ny, prec);
			break;
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acb_finalize);
		return ans;
	}
	case  8: /*  "==" */
	case  9: /*  "!=" */
	case 14: /*   "&" */
	case 15: /*   "|" */
	{
		ERROR_TOO_LONG(n);
		SEXP ans = Rf_allocVector(LGLSXP, (R_xlen_t) n);
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ACB_CONTAINS_NAN(x + j % nx) ||
				 ACB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: acb_eq(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ACB_CONTAINS_NAN(x + j % nx) ||
				 ACB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: acb_ne(x + j % nx, y + j % ny) != 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ACB_CONTAINS_ZERO(x + j % nx) ||
				 ACB_CONTAINS_ZERO(y + j % ny))
				? 0
				:
				(ACB_CONTAINS_NAN(x + j % nx) ||
				 ACB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ACB_CONTAINS_NONZERO(x + j % nx) ||
				 ACB_CONTAINS_NONZERO(y + j % ny))
				? 1
				:
				(ACB_CONTAINS_NAN(x + j % nx) ||
				 ACB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: 0;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
}

SEXP R_flint_acb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	acb_srcptr x = (acb_ptr) R_flint_get_pointer(s_x);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	switch (op) {
	case  1: /*        "+" */
	case  2: /*        "-" */
	case  4: /*     "sign" */
	case  5: /*     "sqrt" */
	case 11: /*   "cumsum" */
	case 12: /*  "cumprod" */
	case 13: /*      "log" */
	case 14: /*    "log10" */
	case 15: /*     "log2" */
	case 16: /*    "log1p" */
	case 17: /*      "exp" */
	case 18: /*    "expm1" */
	case 19: /*      "cos" */
	case 20: /*    "cospi" */
	case 21: /*     "acos" */
	case 22: /*     "cosh" */
	case 23: /*    "acosh" */
	case 24: /*      "sin" */
	case 25: /*    "sinpi" */
	case 26: /*     "asin" */
	case 27: /*     "sinh" */
	case 28: /*    "asinh" */
	case 29: /*      "tan" */
	case 30: /*    "tanpi" */
	case 31: /*     "atan" */
	case 32: /*     "tanh" */
	case 33: /*    "atanh" */
	case 34: /*    "gamma" */
	case 35: /*   "lgamma" */
	case 36: /*  "digamma" */
	case 37: /* "trigamma" */
#if 0 /* TODO */
	case 38: /*    "round" */
	case 39: /*   "signif" */
#endif
	case 47: /*     "Conj" */
	{
		SEXP ans = newObject("acb");
		acb_ptr z = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
		switch (op) {
		case  1: /*        "+" */
			for (j = 0; j < n; ++j)
				acb_set(z + j, x + j);
			break;
		case  2: /*        "-" */
			for (j = 0; j < n; ++j)
				acb_neg(z + j, x + j);
			break;
		case  4: /*     "sign" */
			for (j = 0; j < n; ++j)
				acb_sgn(z + j, x + j, prec);
			break;
		case  5: /*     "sqrt" */
			for (j = 0; j < n; ++j)
				acb_sqrt(z + j, x + j, prec);
			break;
		case 11: /*   "cumsum" */
			if (n) {
			acb_set(z, x);
			for (j = 1; j < n; ++j)
				acb_add(z + j, z + j - 1, x + j, prec);
			}
			break;
		case 12: /*  "cumprod" */
			if (n)
			acb_set(z, x);
			for (j = 1; j < n; ++j)
				acb_mul(z + j, z + j - 1, x + j, prec);
			break;
		case 13: /*      "log" */
		case 14: /*    "log10" */
		case 15: /*     "log2" */
			for (j = 0; j < n; ++j)
				acb_log(z + j, x + j, prec);
			if (op != 13 || s_dots != R_NilValue) {
			acb_t tmp;
			acb_init(tmp);
			if (op != 13)
				acb_set_ui(tmp, (op == 14) ? 10 : 2);
			else {
				acb_srcptr base = (acb_ptr) R_flint_get_pointer(s_dots);
				acb_set(tmp, base);
			}
			acb_log(tmp, tmp, prec);
			for (j = 0; j < n; ++j)
				acb_div(z + j, z + j, tmp, prec);
			acb_clear(tmp);
			}
			break;
		case 16: /*    "log1p" */
			for (j = 0; j < n; ++j)
				acb_log1p(z + j, x + j, prec);
			break;
		case 17: /*      "exp" */
			for (j = 0; j < n; ++j)
				acb_exp(z + j, x + j, prec);
			break;
		case 18: /*    "expm1" */
			for (j = 0; j < n; ++j)
				acb_expm1(z + j, x + j, prec);
			break;
		case 19: /*      "cos" */
			for (j = 0; j < n; ++j)
				acb_cos(z + j, x + j, prec);
			break;
		case 20: /*    "cospi" */
			for (j = 0; j < n; ++j)
				acb_cos_pi(z + j, x + j, prec);
			break;
		case 21: /*     "acos" */
			for (j = 0; j < n; ++j)
				acb_acos(z + j, x + j, prec);
			break;
		case 22: /*     "cosh" */
			for (j = 0; j < n; ++j)
				acb_cosh(z + j, x + j, prec);
			break;
		case 23: /*    "acosh" */
			for (j = 0; j < n; ++j)
				acb_acosh(z + j, x + j, prec);
			break;
		case 24: /*      "sin" */
			for (j = 0; j < n; ++j)
				acb_sin(z + j, x + j, prec);
			break;
		case 25: /*    "sinpi" */
			for (j = 0; j < n; ++j)
				acb_sin_pi(z + j, x + j, prec);
			break;
		case 26: /*     "asin" */
			for (j = 0; j < n; ++j)
				acb_asin(z + j, x + j, prec);
			break;
		case 27: /*     "sinh" */
			for (j = 0; j < n; ++j)
				acb_sinh(z + j, x + j, prec);
			break;
		case 28: /*    "asinh" */
			for (j = 0; j < n; ++j)
				acb_asinh(z + j, x + j, prec);
			break;
		case 29: /*      "tan" */
			for (j = 0; j < n; ++j)
				acb_tan(z + j, x + j, prec);
			break;
		case 30: /*    "tanpi" */
			for (j = 0; j < n; ++j)
				acb_tan_pi(z + j, x + j, prec);
			break;
		case 31: /*     "atan" */
			for (j = 0; j < n; ++j)
				acb_atan(z + j, x + j, prec);
			break;
		case 32: /*     "tanh" */
			for (j = 0; j < n; ++j)
				acb_tanh(z + j, x + j, prec);
			break;
		case 33: /*    "atanh" */
			for (j = 0; j < n; ++j)
				acb_atanh(z + j, x + j, prec);
			break;
		case 34: /*    "gamma" */
			for (j = 0; j < n; ++j)
				acb_gamma(z + j, x + j, prec);
			break;
		case 35: /*   "lgamma" */
			for (j = 0; j < n; ++j)
				acb_lgamma(z + j, x + j, prec);
			break;
		case 36: /*  "digamma" */
			for (j = 0; j < n; ++j)
				acb_digamma(z + j, x + j, prec);
			break;
		case 37: /* "trigamma" */
		{
			acb_t tmp;
			acb_init(tmp);
			acb_set_si(tmp, 1);
			for (j = 0; j < n; ++j)
				acb_polygamma(z + j, tmp, x + j, prec);
			acb_clear(tmp);
			break;
		}
#if 0 /* TODO */
		case 38: /*    "round" */
			for (j = 0; j < n; ++j)
				;
			break;
		case 39: /*   "signif" */
			for (j = 0; j < n; ++j)
				;
			break;
#endif
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acb_finalize);
		return ans;
	}
	case 43: /*     "sum" */
	case 44: /*    "prod" */
	{
		SEXP ans = newObject("acb");
		size_t s = (op == 42) ? 2 : 1;
		acb_ptr z = (acb_ptr) flint_calloc(s, sizeof(acb_t));
		int narm = LOGICAL_RO(s_dots)[0];
		switch (op) {
		case 43: /*     "sum" */
			acb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ACB_CONTAINS_NAN(x + j)))
				acb_add(z, z, x + j, prec);
			break;
		case 44: /*    "prod" */
			acb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ACB_CONTAINS_NAN(x + j)))
				acb_mul(z, z, x + j, prec);
			break;
		}
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_acb_finalize);
		return ans;
	}
	case 45: /*     "any" */
	case 46: /*     "all" */
	{
		SEXP ans = Rf_allocVector(LGLSXP, 1);
		int *z = LOGICAL(ans);
		int narm = LOGICAL_RO(s_dots)[0], anyna = 0;
		switch (op) {
		case 45: /*     "any" */
			/* Return 1 if and only if any does not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(acb_realref(x + j))) ||
					arf_is_nan(arb_midref(acb_imagref(x + j))))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(acb_realref(x + j)), arb_radref(acb_realref(x + j))) >  0 ||
						 arf_cmpabs_mag(arb_midref(acb_imagref(x + j)), arb_radref(acb_imagref(x + j))) >  0)
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 46: /*     "all" */
			/* Return 1 if and only if all do   not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(acb_realref(x + j))) ||
					arf_is_nan(arb_midref(acb_imagref(x + j))))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(acb_realref(x + j)), arb_radref(acb_realref(x + j))) <= 0 ||
						 arf_cmpabs_mag(arb_midref(acb_imagref(x + j)), arb_radref(acb_imagref(x + j))) <= 0)
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		}
		return ans;
	}
	case  3: /*      "abs" */
	case 48: /*       "Re" */
	case 49: /*       "Im" */
	case 50: /*      "Mod" */
	case 51: /*      "Arg" */
	{
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		switch (op) {
		case 48: /*       "Re" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, acb_realref(x + j));
			break;
		case 49: /*       "Im" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, acb_imagref(x + j));
			break;
		case  3: /*      "abs" */
		case 50: /*      "Mod" */
			for (j = 0; j < n; ++j)
				acb_abs(z + j, x + j, prec);
			break;
		case 51: /*      "Arg" */
			for (j = 0; j < n; ++j)
				acb_arg(z + j, x + j, prec);
			break;
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
}
