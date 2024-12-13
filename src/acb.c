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
		if (TYPEOF(s_x) != OBJSXP)
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
		case OBJSXP:
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
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acb_finalize);
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
	case  8: /*     "Conj" */
	case 14: /*     "sign" */
	case 15: /*     "sqrt" */
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
		SEXP ans = newObject("acb");
		acb_ptr z = (acb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acb_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acb_finalize);
		switch (op) {
		case  1: /*        "+" */
			for (j = 0; j < n; ++j)
				acb_set(z + j, x + j);
			break;
		case  2: /*        "-" */
			for (j = 0; j < n; ++j)
				acb_neg(z + j, x + j);
			break;
		case  8: /*     "Conj" */
			for (j = 0; j < n; ++j)
				acb_conj(z + j, x + j);
			break;
		case 14: /*     "sign" */
			for (j = 0; j < n; ++j)
				acb_sgn(z + j, x + j, prec);
			break;
		case 15: /*     "sqrt" */
			for (j = 0; j < n; ++j)
				acb_sqrt(z + j, x + j, prec);
			break;
		case 21: /*   "cumsum" */
			if (n) {
			acb_set(z, x);
			for (j = 1; j < n; ++j)
				acb_add(z + j, z + j - 1, x + j, prec);
			}
			break;
		case 22: /*  "cumprod" */
			if (n)
			acb_set(z, x);
			for (j = 1; j < n; ++j)
				acb_mul(z + j, z + j - 1, x + j, prec);
			break;
		case 23: /*      "log" */
		case 24: /*    "log10" */
		case 25: /*     "log2" */
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
		case 26: /*    "log1p" */
			for (j = 0; j < n; ++j)
				acb_log1p(z + j, x + j, prec);
			break;
		case 27: /*      "exp" */
			for (j = 0; j < n; ++j)
				acb_exp(z + j, x + j, prec);
			break;
		case 28: /*    "expm1" */
			for (j = 0; j < n; ++j)
				acb_expm1(z + j, x + j, prec);
			break;
		case 29: /*      "cos" */
			for (j = 0; j < n; ++j)
				acb_cos(z + j, x + j, prec);
			break;
		case 30: /*    "cospi" */
			for (j = 0; j < n; ++j)
				acb_cos_pi(z + j, x + j, prec);
			break;
		case 31: /*     "acos" */
			for (j = 0; j < n; ++j)
				acb_acos(z + j, x + j, prec);
			break;
		case 32: /*     "cosh" */
			for (j = 0; j < n; ++j)
				acb_cosh(z + j, x + j, prec);
			break;
		case 33: /*    "acosh" */
			for (j = 0; j < n; ++j)
				acb_acosh(z + j, x + j, prec);
			break;
		case 34: /*      "sin" */
			for (j = 0; j < n; ++j)
				acb_sin(z + j, x + j, prec);
			break;
		case 35: /*    "sinpi" */
			for (j = 0; j < n; ++j)
				acb_sin_pi(z + j, x + j, prec);
			break;
		case 36: /*     "asin" */
			for (j = 0; j < n; ++j)
				acb_asin(z + j, x + j, prec);
			break;
		case 37: /*     "sinh" */
			for (j = 0; j < n; ++j)
				acb_sinh(z + j, x + j, prec);
			break;
		case 38: /*    "asinh" */
			for (j = 0; j < n; ++j)
				acb_asinh(z + j, x + j, prec);
			break;
		case 39: /*      "tan" */
			for (j = 0; j < n; ++j)
				acb_tan(z + j, x + j, prec);
			break;
		case 40: /*    "tanpi" */
			for (j = 0; j < n; ++j)
				acb_tan_pi(z + j, x + j, prec);
			break;
		case 41: /*     "atan" */
			for (j = 0; j < n; ++j)
				acb_atan(z + j, x + j, prec);
			break;
		case 42: /*     "tanh" */
			for (j = 0; j < n; ++j)
				acb_tanh(z + j, x + j, prec);
			break;
		case 43: /*    "atanh" */
			for (j = 0; j < n; ++j)
				acb_atanh(z + j, x + j, prec);
			break;
		case 44: /*    "gamma" */
			for (j = 0; j < n; ++j)
				acb_gamma(z + j, x + j, prec);
			break;
		case 45: /*   "lgamma" */
			for (j = 0; j < n; ++j)
				acb_lgamma(z + j, x + j, prec);
			break;
		case 46: /*  "digamma" */
			for (j = 0; j < n; ++j)
				acb_digamma(z + j, x + j, prec);
			break;
		case 47: /* "trigamma" */
		{
			acb_t tmp;
			acb_init(tmp);
			acb_set_si(tmp, 1);
			for (j = 0; j < n; ++j)
				acb_polygamma(z + j, tmp, x + j, prec);
			acb_clear(tmp);
			break;
		}
		case 48: /*    "round" */
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
		case 49: /*   "signif" */
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
		return ans;
	}
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		SEXP ans = newObject("acb");
		size_t s = (op == 42) ? 2 : 1;
		acb_ptr z = (acb_ptr) flint_calloc(s, sizeof(acb_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_acb_finalize);
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
		return ans;
	}
	case 55: /*     "any" */
	case 56: /*     "all" */
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
				z[j] =
					arf_is_nan(arb_midref(acb_realref(x + j))) != 0 ||
					arf_is_nan(arb_midref(acb_imagref(x + j))) != 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_inf(arb_midref(acb_realref(x + j))) != 0 ||
					mag_is_inf(arb_radref(acb_realref(x + j))) != 0 ||
					arf_is_inf(arb_midref(acb_imagref(x + j))) != 0 ||
					mag_is_inf(arb_radref(acb_imagref(x + j))) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_finite(arb_midref(acb_realref(x + j))) != 0 &&
					mag_is_finite(arb_radref(acb_realref(x + j))) != 0 &&
					arf_is_finite(arb_midref(acb_imagref(x + j))) != 0 &&
					mag_is_finite(arb_radref(acb_imagref(x + j))) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(acb_imagref(x + j))) ||
				    arf_is_nan(arb_midref(acb_imagref(x + j))))
				z[j] = NA_LOGICAL;
				else
				z[j] =
					arf_is_zero(arb_midref(acb_realref(x + j))) != 0 &&
					mag_is_zero(arb_radref(acb_realref(x + j))) != 0 &&
					arf_is_zero(arb_midref(acb_imagref(x + j))) != 0 &&
					mag_is_zero(arb_radref(acb_imagref(x + j))) != 0;
			break;
		}
		return ans;
	}
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
	{
		SEXP ans = newObject("arb");
		arb_ptr z = (arb_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arb_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case  9: /*       "Re" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, acb_realref(x + j));
			break;
		case 10: /*       "Im" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, acb_imagref(x + j));
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (j = 0; j < n; ++j)
				acb_abs(z + j, x + j, prec);
			break;
		case 12: /*      "Arg" */
			for (j = 0; j < n; ++j)
				acb_arg(z + j, x + j, prec);
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
