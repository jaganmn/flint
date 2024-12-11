#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/mag.h>
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
                            SEXP s_real, SEXP s_imag)
{
	unsigned long long int j, n, nr = 1, ni = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue)
			nr = R_flint_get_length(s_real);
		if (s_imag != R_NilValue)
			ni = R_flint_get_length(s_imag);
		n = RECYCLE2(nr, ni);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != EXTPTRSXP)
		n = (unsigned long long int) XLENGTH(s_x);
		else if ((class = R_flint_get_class(s_x)) != R_FLINT_CLASS_INVALID)
		n = R_flint_get_length(s_x);
		else
		n = 0;
	} else
		n = asLength(s_length, __func__);
	acb_ptr y = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arb_srcptr xr = (arb_ptr) R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arb_srcptr xi = (arb_ptr) R_flint_get_pointer(s_imag);
				for (j = 0; j < n; ++j) {
					arb_set(acb_realref(y + j), xr + j % nr);
					arb_set(acb_imagref(y + j), xi + j % ni);
				}
			} else {
				for (j = 0; j < n; ++j) {
					arb_set(acb_realref(y + j), xr + j);
					arb_zero(acb_imagref(y + j));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arb_srcptr xi = (arb_ptr) R_flint_get_pointer(s_imag);
				for (j = 0; j < n; ++j) {
					arb_zero(acb_realref(y + j));
					arb_set(acb_imagref(y + j), xi + j);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				acb_zero(y + j);
			break;
		case RAWSXP:
		case LGLSXP:
			s_x = Rf_coerceVector(s_x, INTSXP);
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < n; ++j)
				acb_set_si(y + j, x[j]);
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (j = 0; j < n; ++j)
				acb_set_d(y + j, x[j]);
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (j = 0; j < n; ++j)
				acb_set_d_d(y + j, x[j].r, x[j].i);
			break;
		}
		case EXTPTRSXP:
			switch (class) {
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = (slong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					acb_set_si(y + j, x[j]);
				break;
			}
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = (ulong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					acb_set_ui(y + j, x[j]);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					acb_set_fmpz(y + j, x + j);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
				int prec = asPrec(R_NilValue, __func__);
				for (j = 0; j < n; ++j) {
					arb_fmpz_div_fmpz(acb_realref(y + j), fmpq_numref(x + j), fmpq_denref(x + j), prec);
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set(arb_midref(acb_realref(y + j)), x + j);
					mag_zero(arb_radref(acb_realref(y + j)));
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_mag(arb_midref(acb_realref(y + j)), x + j);
					mag_zero(arb_radref(acb_realref(y + j)));
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = (arb_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arb_set(acb_realref(y + j), x + j);
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = (acb_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					acb_set(y + j, x + j);
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
	case 38: /*    "round" */
	case 39: /*   "signif" */
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
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + j)); \
				xr = arb_radref(acb_partref(x + j)); \
				zm = arb_midref(acb_partref(z + j)); \
				zr = arb_radref(acb_partref(z + j)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, ARF_RND_NEAR); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				arf_fmpz_div_fmpz(zm, q, p, prec, ARF_RND_NEAR); \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
#undef TEMPLATE
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			fmpz_divexact_si(q, p, 2);
			mag_set_fmpz(d, q);
			for (j = 0; j < n; ++j) {
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + j)); \
				xr = arb_radref(acb_partref(x + j)); \
				zm = arb_midref(acb_partref(z + j)); \
				zr = arb_radref(acb_partref(z + j)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_div_fmpz(s, xm, p, prec, ARF_RND_NEAR); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
#undef TEMPLATE
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
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + j)); \
				xr = arb_radref(acb_partref(x + j)); \
				zm = arb_midref(acb_partref(z + j)); \
				zr = arb_radref(acb_partref(z + j)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_get_fmpq(a, xm); \
				fmpq_abs(a, a); \
				clog = fmpq_clog_ui(a, 10); \
				if (arf_sgn(xm) < 0) \
					fmpq_neg(a, a); \
				fmpz_set_si(p, 10); \
				if (clog <= digits) { \
				if (clog >= 0) \
				fmpz_pow_ui(p, p, (ulong) (digits - clog)); \
				else \
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1)); \
				fmpz_mul_si(q, p, 2); \
				arf_one(s); \
				arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP); \
				arf_get_mag(d, s); \
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				arf_fmpz_div_fmpz(zm, q, p, prec, ARF_RND_NEAR); \
				} else { \
				fmpz_pow_ui(p, p, (ulong) (clog - digits)); \
				fmpz_divexact_si(q, p, 2); \
				mag_set_fmpz(d, q); \
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				} \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
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
