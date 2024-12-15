#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"
#include "acf.h"

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
				if (x[j] == NA_INTEGER)
				arb_set_d(y + j, R_NaN);
				else
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
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_mag(arb_midref(y + j), x + j);
					mag_zero(arb_radref(y + j));
				}
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
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set(arb_midref(y + j), acf_realref(x + j));
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

SEXP R_flint_arb_vector(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arb_srcptr x = (arb_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = arb_midref(x + j);
		if (arf_is_nan(p))
			y[j] = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j] = arf_get_d(p, rnd);
		else {
			y[j] = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
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
	case  7: /*   "^" */
	{
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
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
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	switch (op) {
	case  1: /*        "+" */
	case  2: /*        "-" */
	case  8: /*     "Conj" */
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
	case 14: /*     "sign" */
	case 15: /*     "sqrt" */
	case 16: /*    "floor" */
	case 17: /*  "ceiling" */
	case 18: /*    "trunc" */
	case 19: /*   "cummin" */
	case 20: /*   "cummax" */
	case 21: /*   "cumsum" */
	case 22: /*  "cumprod" */
	case 23: /*      "log" */
	case 24: /*    "log10" */
	case 25: /*     "log2" */
	case 26: /*    "log1p" */
	case 27: /*      "exp" */
	case 28: /*    "expm1" */
	case 29: /*      "cos" */
	case 30: /*    "cospi" */
	case 31: /*     "acos" */
	case 32: /*     "cosh" */
	case 33: /*    "acosh" */
	case 34: /*      "sin" */
	case 35: /*    "sinpi" */
	case 36: /*     "asin" */
	case 37: /*     "sinh" */
	case 38: /*    "asinh" */
	case 39: /*      "tan" */
	case 40: /*    "tanpi" */
	case 41: /*     "atan" */
	case 42: /*     "tanh" */
	case 43: /*    "atanh" */
	case 44: /*    "gamma" */
	case 45: /*   "lgamma" */
	case 46: /*  "digamma" */
	case 47: /* "trigamma" */
	case 48: /*    "round" */
	case 49: /*   "signif" */
	{
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case  1: /*        "+" */
		case  8: /*     "Conj" */
		case  9: /*       "Re" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, x + j);
			break;
		case  2: /*        "-" */
			for (j = 0; j < n; ++j)
				arb_neg(z + j, x + j);
			break;
		case 10: /*       "Im" */
			for (j = 0; j < n; ++j)
				arb_zero(z + j);
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (j = 0; j < n; ++j)
				arb_nonnegative_abs(z + j, x + j);
			break;
		case 12: /*      "Arg" */
		{
			arb_t pi;
			arb_init(pi);
			arb_const_pi(pi, prec);
			for (j = 0; j < n; ++j) {
				if (arf_is_nan(arb_midref(x + j)) ||
				    mag_is_inf(arb_radref(x + j)) ||
				    arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) < 0) {
					arf_zero(arb_midref(z + j));
					mag_const_pi(arb_radref(z + j));
				}
				else if (arf_is_zero(arb_midref(x + j)))
					arb_zero(z + j);
				else {
					arb_set(z + j, pi);
					if (arf_sgn(arb_midref(x + j)) < 0)
					arb_neg(z + j, z + j);
				}
			}
			arb_clear(pi);
			break;
		}
		case 14: /*     "sign" */
			for (j = 0; j < n; ++j)
				arb_sgn(z + j, x + j);
			break;
		case 15: /*     "sqrt" */
			for (j = 0; j < n; ++j)
				arb_sqrt(z + j, x + j, prec);
			break;
		case 16: /*    "floor" */
			for (j = 0; j < n; ++j)
				arb_floor(z + j, x + j, prec);
			break;
		case 17: /*  "ceiling" */
			for (j = 0; j < n; ++j)
				arb_ceil(z + j, x + j, prec);
			break;
		case 18: /*    "trunc" */
			for (j = 0; j < n; ++j)
				arb_trunc(z + j, x + j, prec);
			break;
		case 19: /*   "cummin" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
				arb_min(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 20: /*   "cummax" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
				arb_max(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 21: /*   "cumsum" */
			if (n) {
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_add(z + j, z + j - 1, x + j, prec);
			}
			break;
		case 22: /*  "cumprod" */
			if (n)
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_mul(z + j, z + j - 1, x + j, prec);
			break;
		case 23: /*      "log" */
		case 24: /*    "log10" */
		case 25: /*     "log2" */
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
		case 26: /*    "log1p" */
			for (j = 0; j < n; ++j)
				arb_log1p(z + j, x + j, prec);
			break;
		case 27: /*      "exp" */
			for (j = 0; j < n; ++j)
				arb_exp(z + j, x + j, prec);
			break;
		case 28: /*    "expm1" */
			for (j = 0; j < n; ++j)
				arb_expm1(z + j, x + j, prec);
			break;
		case 29: /*      "cos" */
			for (j = 0; j < n; ++j)
				arb_cos(z + j, x + j, prec);
			break;
		case 30: /*    "cospi" */
			for (j = 0; j < n; ++j)
				arb_cos_pi(z + j, x + j, prec);
			break;
		case 31: /*     "acos" */
			for (j = 0; j < n; ++j)
				arb_acos(z + j, x + j, prec);
			break;
		case 32: /*     "cosh" */
			for (j = 0; j < n; ++j)
				arb_cosh(z + j, x + j, prec);
			break;
		case 33: /*    "acosh" */
			for (j = 0; j < n; ++j)
				arb_acosh(z + j, x + j, prec);
			break;
		case 34: /*      "sin" */
			for (j = 0; j < n; ++j)
				arb_sin(z + j, x + j, prec);
			break;
		case 35: /*    "sinpi" */
			for (j = 0; j < n; ++j)
				arb_sin_pi(z + j, x + j, prec);
			break;
		case 36: /*     "asin" */
			for (j = 0; j < n; ++j)
				arb_asin(z + j, x + j, prec);
			break;
		case 37: /*     "sinh" */
			for (j = 0; j < n; ++j)
				arb_sinh(z + j, x + j, prec);
			break;
		case 38: /*    "asinh" */
			for (j = 0; j < n; ++j)
				arb_asinh(z + j, x + j, prec);
			break;
		case 39: /*      "tan" */
			for (j = 0; j < n; ++j)
				arb_tan(z + j, x + j, prec);
			break;
		case 40: /*    "tanpi" */
			for (j = 0; j < n; ++j)
				arb_tan_pi(z + j, x + j, prec);
			break;
		case 41: /*     "atan" */
			for (j = 0; j < n; ++j)
				arb_atan(z + j, x + j, prec);
			break;
		case 42: /*     "tanh" */
			for (j = 0; j < n; ++j)
				arb_tanh(z + j, x + j, prec);
			break;
		case 43: /*    "atanh" */
			for (j = 0; j < n; ++j)
				arb_atanh(z + j, x + j, prec);
			break;
		case 44: /*    "gamma" */
			for (j = 0; j < n; ++j)
				arb_gamma(z + j, x + j, prec);
			break;
		case 45: /*   "lgamma" */
			for (j = 0; j < n; ++j)
				arb_lgamma(z + j, x + j, prec);
			break;
		case 46: /*  "digamma" */
			for (j = 0; j < n; ++j)
				arb_digamma(z + j, x + j, prec);
			break;
		case 47: /* "trigamma" */
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
		case 48: /*    "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0];
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
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd);
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
				arf_div_fmpz(s, xm, p, prec, rnd);
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
		case 49: /*   "signif" */
		{
			slong fmpq_clog_ui(const fmpq_t, ulong);
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
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
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd);
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
		return ans;
	}
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP ans = newObject("arb");
		size_t s = (op == 52) ? 2 : 1;
		arb_ptr z = (arb_ptr) flint_calloc(s, sizeof(arb_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_arb_finalize);
		int narm = LOGICAL_RO(s_dots)[0];
		switch (op) {
		case 50: /*     "min" */
			arb_pos_inf(z);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j))
					arb_min(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 51: /*     "max" */
			arb_neg_inf(z);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j))
					arb_max(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 52: /*   "range" */
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
		case 53: /*     "sum" */
			arb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_add(z, z, x + j, prec);
			break;
		case 54: /*    "prod" */
			arb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_mul(z, z, x + j, prec);
			break;
		case 55: /*    "mean" */
		{
			unsigned long long int c = n;
			arb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_add(z, z, x + j, prec);
				else
				--c;
			if (c == 0)
			arb_indeterminate(z);
			else {
			fmpz_t p;
			fmpz_init(p);
			unsigned int uu[2];
			ucopy(uu, &c);
			fmpz_set_uiui(p, uu[1], uu[0]);
			arb_div_fmpz(z, z, p, prec);
			fmpz_clear(p);
			}
			break;
		}
		}
		return ans;
	}
	case 56: /*     "any" */
	case 57: /*     "all" */
	case 58: /*   "anyNA" */
	{
		SEXP ans = Rf_allocVector(LGLSXP, 1);
		int *z = LOGICAL(ans);
		int narm = LOGICAL_RO(s_dots)[0], anyna = 0;
		switch (op) {
		case 56: /*     "any" */
			/* Return 1 if and only if any does not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) >  0)
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*     "all" */
			/* Return 1 if and only if all do   not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) <= 0)
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*   "anyNA" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					break;
			z[0] = j < n;
			break;
		}
		return ans;
	}
	case  3: /*       "is.na" */
	case  4: /*      "is.nan" */
	case  5: /* "is.infinite" */
	case  6: /*   "is.finite" */
	case  7: /*           "!" */
	{
		ERROR_TOO_LONG(n);
		SEXP ans = Rf_allocVector(LGLSXP, (R_xlen_t) n);
		int *z = LOGICAL(ans);
		switch (op) {
		case  3: /*       "is.na" */
		case  4: /*      "is.nan" */
			for (j = 0; j < n; ++j)
				z[j] = arf_is_nan(arb_midref(x + j)) != 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_inf(arb_midref(x + j)) != 0 ||
					mag_is_inf(arb_radref(x + j)) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_finite(arb_midref(x + j)) != 0 &&
					mag_is_finite(arb_radref(x + j)) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
				z[j] = NA_LOGICAL;
				else
				z[j] =
					arf_is_zero(arb_midref(x + j)) != 0 &&
					mag_is_zero(arb_radref(x + j)) != 0;
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
