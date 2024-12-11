#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/mag.h>
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
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		if (s_mid != R_NilValue)
			nm = R_flint_get_length(s_mid);
		if (s_rad != R_NilValue)
			nr = R_flint_get_length(s_rad);
		n = RECYCLE2(nm, nr);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
		n = (unsigned long long int) XLENGTH(s_x);
		else if ((class = R_flint_get_class(s_x)) != R_FLINT_CLASS_INVALID)
		n = R_flint_get_length(s_x);
		else
		n = 0;
	} else
		n = asLength(s_length, __func__);
	arb_ptr y = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_arb_finalize);
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		if (s_mid != R_NilValue) {
			arf_srcptr xm = (arf_ptr) R_flint_get_pointer(s_mid);
			if (s_rad != R_NilValue) {
				mag_srcptr xr = (mag_ptr) R_flint_get_pointer(s_rad);
				for (j = 0; j < n; ++j) {
					arf_set(arb_midref(y + j), xm + j % nm);
					mag_set(arb_radref(y + j), xr + j % nr);
				}
			} else {
				for (j = 0; j < n; ++j) {
					arf_set(arb_midref(y + j), xm + j);
					mag_zero(arb_radref(y + j));
				}
			}
		} else {
			if (s_rad != R_NilValue) {
				mag_srcptr xr = (mag_ptr) R_flint_get_pointer(s_rad);
				for (j = 0; j < n; ++j) {
					arf_zero(arb_midref(y + j));
					mag_set(arb_radref(y + j), xr + j);
				}
			}
		}
	} else {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				arb_zero(y + j);
			break;
		case RAWSXP:
		case LGLSXP:
			s_x = Rf_coerceVector(s_x, INTSXP);
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < n; ++j)
				arb_set_si(y + j, x[j]);
			break;
		}
		case CPLXSXP:
			s_x = Rf_coerceVector(s_x, REALSXP);
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (j = 0; j < n; ++j)
				arb_set_d(y + j, x[j]);
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = (slong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					arb_set_si(y + j, x[j]);
				break;
			}
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = (ulong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					arb_set_ui(y + j, x[j]);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					arb_set_fmpz(y + j, x + j);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
				int prec = asPrec(R_NilValue, __func__);
				for (j = 0; j < n; ++j)
					arb_fmpz_div_fmpz(y + j, fmpq_numref(x + j), fmpq_denref(x + j), prec);
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set(arb_midref(y + j), x + j);
					mag_zero(arb_radref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_mag(arb_midref(y + j), x + j);
					mag_zero(arb_radref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = (arb_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					arb_set(y + j, x + j);
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = (acb_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					arb_set(y + j, acb_realref(x + j));
				break;
			}
			case R_FLINT_CLASS_INVALID:
				Rf_error(_("foreign external pointer"));
				break;
			}
			break;
		}
	}
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
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_eq(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_ne(x + j % nx, y + j % ny) != 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_lt(x + j % nx, y + j % ny) != 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_gt(x + j % nx, y + j % ny) != 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_le(x + j % nx, y + j % ny) != 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: arb_ge(x + j % nx, y + j % ny) != 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_ZERO(x + j % nx) ||
				 ARB_CONTAINS_ZERO(y + j % ny))
				? 0
				:
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				(ARB_CONTAINS_NONZERO(x + j % nx) ||
				 ARB_CONTAINS_NONZERO(y + j % ny))
				? 1
				:
				(ARB_CONTAINS_NAN(x + j % nx) ||
				 ARB_CONTAINS_NAN(y + j % ny))
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
	case 38: /*    "round" */
	case 39: /*   "signif" */
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
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
				arb_min(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 10: /*   "cummax" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
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
		case 38: /*    "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				prec = asPrec(R_NilValue, __func__);
			fmpz_t p, q;
			arf_t s;
			mag_t d;
			arf_srcptr xm;
			mag_srcptr xr;
			arf_ptr zm;
			mag_ptr zr;
			fmpz_init(p);
			fmpz_init(q);
			arf_init(s);
			mag_init(d);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* f ~ c/10^+digits   <=>   c ~ f*10^+digits */
			fmpz_pow_ui(p, p, (ulong) digits);
			fmpz_mul_si(q, p, 2);
			arf_one(s);
			arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP);
			arf_get_mag(d, s);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, ARF_RND_NEAR);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(zm, q, p, prec, ARF_RND_NEAR);
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			fmpz_divexact_si(q, p, 2);
			mag_set_fmpz(d, q);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_div_fmpz(s, xm, p, prec, ARF_RND_NEAR);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				arf_set_fmpz(zm, q);
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			arf_clear(s);
			mag_clear(d);
			break;
		}
		case 39: /*   "signif" */
		{
			slong fmpq_clog_ui(const fmpq_t, ulong);
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				prec = asPrec(R_NilValue, __func__),
				clog;
			if (digits <= 0)
				digits = 1;
			fmpq_t a;
			fmpz_t p, q, r;
			arf_t s;
			mag_t d;
			arf_srcptr xm;
			mag_srcptr xr;
			arf_ptr zm;
			mag_ptr zr;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			arf_init(s);
			mag_init(d);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_get_fmpq(a, xm);
				fmpq_abs(a, a);
				clog = fmpq_clog_ui(a, 10);
				if (arf_sgn(xm) < 0)
					fmpq_neg(a, a);
				fmpz_set_si(p, 10);
				if (clog <= digits) {
				if (clog >= 0)
				fmpz_pow_ui(p, p, (ulong) (digits - clog));
				else
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1));
				fmpz_mul_si(q, p, 2);
				arf_one(s);
				arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP);
				arf_get_mag(d, s);
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				arf_fmpz_div_fmpz(zm, q, p, prec, ARF_RND_NEAR);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_divexact_si(q, p, 2);
				mag_set_fmpz(d, q);
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				arf_set_fmpz(zm, q);
				}
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			arf_clear(s);
			mag_clear(d);
			break;
		}
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
				if (!ARB_CONTAINS_NAN(x + j))
					arb_min(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 41: /*     "max" */
			arb_neg_inf(z);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j))
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
				if (!ARB_CONTAINS_NAN(x + j)) {
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
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_add(z, z, x + j, prec);
			break;
		case 44: /*    "prod" */
			arb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
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
			/* Return 1 if and only if any does not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) >  0)
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 46: /*     "all" */
			/* Return 1 if and only if all do   not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) <= 0)
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
