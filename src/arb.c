#include <flint/flint.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

void R_flint_arb_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
	arb_ptr p = (arb_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		arb_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_mid, SEXP s_rad)
{
	unsigned long long int j, n, nm = 1, nr = 1;
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
	arb_ptr y = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		switch (TYPEOF(s_mid)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				arf_zero(arb_midref(y + j));
			break;
		case RAWSXP:
		case LGLSXP:
			s_mid = Rf_coerceVector(s_mid, INTSXP);
		case INTSXP:
		{
			const int *xm = INTEGER_RO(s_mid);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xm[j % nm];
				if (tmp == NA_INTEGER)
				arf_nan(arb_midref(y + j));
				else
				arf_set_si(arb_midref(y + j), tmp);
			}
			break;
		}
		case REALSXP:
		{
			const double *xm = REAL_RO(s_mid);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xm[j % nm];
				arf_set_d(arb_midref(y + j), tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_rad)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				mag_zero(arb_radref(y + j));
			break;
		case RAWSXP:
		case LGLSXP:
			s_rad = Rf_coerceVector(s_rad, INTSXP);
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_rad);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = x[j % nr];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_ui(arb_radref(y + j), (ulong) ((tmp < 0) ? -tmp : tmp));
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_rad);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = x[j % nr];
				if (ISNAN(tmp))
				Rf_error(_("NaN not representable by '%s'"), "mag");
				else
				mag_set_d(arb_radref(y + j), tmp);
			}
			break;
		}
		}
	} else
		for (j = 0; j < n; ++j)
			arb_zero(y + j);
	return object;
}

SEXP R_flint_arb_narb(SEXP from, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(newObject("narb")),
		mid = PROTECT(newBasic("narf", REALSXP, (R_xlen_t) n)),
		rad = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	R_do_slot_assign(to, R_flint_symbol_mid, mid);
	R_do_slot_assign(to, R_flint_symbol_rad, rad);
	arb_srcptr x = (arb_ptr) R_flint_get_pointer(from);
	double *ym = REAL(mid), *yr = REAL(rad);
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
		m = arb_midref(x + j);
		if (arf_is_nan(m))
			ym[j] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			ym[j] = arf_get_d(m, rnd);
		else {
			ym[j] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		r = arb_radref(x + j);
		if (mag_cmp(r, ubr) < 0)
			yr[j] = mag_get_d(r);
		else {
			yr[j] = R_PosInf;
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
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arb_srcptr x = (arb_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
	arf_t lbm, ubm;
	arf_srcptr m;
	arf_init(lbm);
	arf_init(ubm);
	arf_set_ui_2exp_si(ubm, 1U, DBL_MAX_EXP);
	arf_neg(lbm, ubm);
	int w = 1;
	for (j = 0; j < n; ++j) {
		m = arb_midref(x + j);
		if (arf_is_nan(m))
			y[j] = R_NaN;
		else if (arf_cmp(m, lbm) > 0 && arf_cmp(m, ubm) < 0)
			y[j] = arf_get_d(m, rnd);
		else {
			y[j] = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lbm);
	arf_clear(ubm);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_arb_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	arb_srcptr
		x = (arb_ptr) R_flint_get_pointer(s_x),
		y = (arb_ptr) R_flint_get_pointer(s_y);
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
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				arb_add(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				arb_sub(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				arb_mul(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				arb_div(z + j, x + j % nx, y + j % ny, prec);
			break;
		case 7: /*   "^" */
			for (j = 0; j < n; ++j)
				arb_pow(z + j, x + j % nx, y + j % ny, prec);
			break;
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		return ans;
	}
	case  8: /*  "==" */
	case  9: /*  "!=" */
	case 10: /*   "<" */
	case 11: /*   ">" */
	case 12: /*  "<=" */
	case 13: /*  ">=" */
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
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_eq(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_ne(x + j % nx, y + j % ny) != 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_lt(x + j % nx, y + j % ny) != 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_gt(x + j % nx, y + j % ny) != 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_le(x + j % nx, y + j % ny) != 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: arb_ge(x + j % nx, y + j % ny) != 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] =
				((!arf_is_nan(arb_midref(x + j % nx)) &&
				  arb_contains_zero(x + j % nx)) ||
				 (!arf_is_nan(arb_midref(y + j % ny)) &&
				  arb_contains_zero(y + j % ny)))
				? 0
				:
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				((!arf_is_nan(arb_midref(x + j % nx)) &&
				  !arb_contains_zero(x + j % nx)) &&
				 (!arf_is_nan(arb_midref(y + j % ny)) &&
				  !arb_contains_zero(y + j % ny)))
				? 1
				:
				(arf_is_nan(arb_midref(x + j % nx)) ||
				 arf_is_nan(arb_midref(y + j % ny)))
				? NA_LOGICAL
				: 0;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}

SEXP R_flint_arb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	arb_srcptr x = (arb_ptr) R_flint_get_pointer(s_x);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	switch (op) {
	case  1: /*        "+" */
	case  2: /*        "-" */
	case  3: /*      "abs" */
	case  4: /*     "sign" */
	case  5: /*     "sqrt" */
	case  6: /*    "floor" */
	case  7: /*  "ceiling" */
	case  8: /*    "trunc" */
	case  9: /*   "cummin" */
	case 10: /*   "cummax" */
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
	{
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		switch (op) {
		case  1: /*        "+" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, x + j);
			break;
		case  2: /*        "-" */
			for (j = 0; j < n; ++j)
				arb_neg(z + j, x + j);
			break;
		case  3: /*      "abs" */
			for (j = 0; j < n; ++j)
				arb_nonnegative_abs(z + j, x + j);
			break;
		case  4: /*     "sign" */
			for (j = 0; j < n; ++j)
				arb_sgn(z + j, x + j);
			break;
		case  5: /*     "sqrt" */
			for (j = 0; j < n; ++j)
				arb_sqrt(z + j, x + j, prec);
			break;
		case  6: /*    "floor" */
			for (j = 0; j < n; ++j)
				arb_floor(z + j, x + j, prec);
			break;
		case  7: /*  "ceiling" */
			for (j = 0; j < n; ++j)
				arb_ceil(z + j, x + j, prec);
			break;
		case  8: /*    "trunc" */
			for (j = 0; j < n; ++j)
				arb_trunc(z + j, x + j, prec);
			break;
		case  9: /*   "cummin" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(arb_midref(x + j)); ++j)
				arb_min(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 10: /*   "cummax" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(arb_midref(x + j)); ++j)
				arb_max(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 11: /*   "cumsum" */
			if (n) {
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_add(z + j, z + j - 1, x + j, prec);
			}
			break;
		case 12: /*  "cumprod" */
			if (n)
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_mul(z + j, z + j - 1, x + j, prec);
			break;
		case 13: /*      "log" */
		case 14: /*    "log10" */
		case 15: /*     "log2" */
			for (j = 0; j < n; ++j)
				arb_log(z + j, x + j, prec);
			if (op != 13 || s_dots != R_NilValue) {
			arb_t tmp;
			arb_init(tmp);
			if (op != 13)
				arb_set_ui(tmp, (op == 14) ? 10 : 2);
			else {
				arb_srcptr base = (arb_ptr) R_flint_get_pointer(s_dots);
				arb_set(tmp, base);
			}
			arb_log(tmp, tmp, prec);
			for (j = 0; j < n; ++j)
				arb_div(z + j, z + j, tmp, prec);
			arb_clear(tmp);
			}
			break;
		case 16: /*    "log1p" */
			for (j = 0; j < n; ++j)
				arb_log1p(z + j, x + j, prec);
			break;
		case 17: /*      "exp" */
			for (j = 0; j < n; ++j)
				arb_exp(z + j, x + j, prec);
			break;
		case 18: /*    "expm1" */
			for (j = 0; j < n; ++j)
				arb_expm1(z + j, x + j, prec);
			break;
		case 19: /*      "cos" */
			for (j = 0; j < n; ++j)
				arb_cos(z + j, x + j, prec);
			break;
		case 20: /*    "cospi" */
			for (j = 0; j < n; ++j)
				arb_cos_pi(z + j, x + j, prec);
			break;
		case 21: /*     "acos" */
			for (j = 0; j < n; ++j)
				arb_acos(z + j, x + j, prec);
			break;
		case 22: /*     "cosh" */
			for (j = 0; j < n; ++j)
				arb_cosh(z + j, x + j, prec);
			break;
		case 23: /*    "acosh" */
			for (j = 0; j < n; ++j)
				arb_acosh(z + j, x + j, prec);
			break;
		case 24: /*      "sin" */
			for (j = 0; j < n; ++j)
				arb_sin(z + j, x + j, prec);
			break;
		case 25: /*    "sinpi" */
			for (j = 0; j < n; ++j)
				arb_sin_pi(z + j, x + j, prec);
			break;
		case 26: /*     "asin" */
			for (j = 0; j < n; ++j)
				arb_asin(z + j, x + j, prec);
			break;
		case 27: /*     "sinh" */
			for (j = 0; j < n; ++j)
				arb_sinh(z + j, x + j, prec);
			break;
		case 28: /*    "asinh" */
			for (j = 0; j < n; ++j)
				arb_asinh(z + j, x + j, prec);
			break;
		case 29: /*      "tan" */
			for (j = 0; j < n; ++j)
				arb_tan(z + j, x + j, prec);
			break;
		case 30: /*    "tanpi" */
			for (j = 0; j < n; ++j)
				arb_tan_pi(z + j, x + j, prec);
			break;
		case 31: /*     "atan" */
			for (j = 0; j < n; ++j)
				arb_atan(z + j, x + j, prec);
			break;
		case 32: /*     "tanh" */
			for (j = 0; j < n; ++j)
				arb_tanh(z + j, x + j, prec);
			break;
		case 33: /*    "atanh" */
			for (j = 0; j < n; ++j)
				arb_atanh(z + j, x + j, prec);
			break;
		case 34: /*    "gamma" */
			for (j = 0; j < n; ++j)
				arb_gamma(z + j, x + j, prec);
			break;
		case 35: /*   "lgamma" */
			for (j = 0; j < n; ++j)
				arb_lgamma(z + j, x + j, prec);
			break;
		case 36: /*  "digamma" */
			for (j = 0; j < n; ++j)
				arb_digamma(z + j, x + j, prec);
			break;
		case 37: /* "trigamma" */
		{
			acb_t tmp0, tmp1, tmp2;
			acb_init(tmp0);
			acb_init(tmp1);
			acb_init(tmp2);
			acb_set_si(tmp1, 1);
			arb_zero(acb_imagref(tmp2));
			for (j = 0; j < n; ++j) {
				arb_set(acb_realref(tmp2), x + j);
				acb_polygamma(tmp0, tmp1, tmp2, prec);
				arb_set(z + j, acb_realref(tmp0));
			}
			acb_clear(tmp0);
			acb_clear(tmp1);
			acb_clear(tmp2);
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
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		return ans;
	}
	case 40: /*     "min" */
	case 41: /*     "max" */
	case 42: /*   "range" */
	case 43: /*     "sum" */
	case 44: /*    "prod" */
	{
		SEXP ans = newObject("arb");
		size_t s = (op == 42) ? 2 : 1;
		arb_ptr z = (arb_ptr) flint_calloc(s, sizeof(arb_t));
		int narm = LOGICAL_RO(s_dots)[0];
		switch (op) {
		case 40: /*     "min" */
			arb_pos_inf(z);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(arb_midref(x + j)))
					arb_min(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 41: /*     "max" */
			arb_neg_inf(z);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(arb_midref(x + j)))
					arb_max(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 42: /*   "range" */
			arb_pos_inf(z);
			arb_neg_inf(z + 1);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(arb_midref(x + j))) {
					arb_min(z, z, x + j, prec);
					arb_max(z + 1, z + 1, x + j, prec);
				} else if (!narm) {
					arb_indeterminate(z);
					arb_indeterminate(z + 1);
					break;
				}
			break;
		case 43: /*     "sum" */
			arb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(arb_midref(x + j))))
				arb_add(z, z, x + j, prec);
			break;
		case 44: /*    "prod" */
			arb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(arb_midref(x + j))))
				arb_mul(z, z, x + j, prec);
			break;
		}
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_arb_finalize);
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
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (!arb_contains_zero(x + j))
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 46: /*     "all" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arb_contains_zero(x + j))
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}
