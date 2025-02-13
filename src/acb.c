#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

arf_rnd_t remapRnd(mpfr_rnd_t);

void R_flint_acb_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	acb_ptr p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		acb_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP s_length, SEXP s_x,
                            SEXP s_real, SEXP s_imag)
{
	mp_limb_t j, nr = 1, ni = 1, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "real", "imag");
		if (s_real != R_NilValue)
			nr = R_flint_get_length(s_real);
		if (s_imag != R_NilValue)
			ni = R_flint_get_length(s_imag);
		if (s_length == R_NilValue)
			ny = RECYCLE2(nr, ni);
		else {
			ny = asLength(s_length, __func__);
			if (ny > 0 && (nr == 0 || ni == 0))
				Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
				         (nr == 0) ? "real" : "imag");
		}
	}
	else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
			nx = (mp_limb_t) XLENGTH(s_x);
		else {
			class = R_flint_get_class(s_x);
			if (class == R_FLINT_CLASS_INVALID)
				Rf_error(_("foreign external pointer"));
			nx = R_flint_get_length(s_x);
		}
		if (s_length == R_NilValue)
			ny = nx;
		else {
			ny = asLength(s_length, __func__);
			if (ny > 0 && nx == 0)
				Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
				         "x");
		}
	}
	else if (s_length != R_NilValue)
		ny = asLength(s_length, __func__);
	else
		ny = 0;
	acb_ptr y = (ny) ? flint_calloc(ny, sizeof(acb_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arb_srcptr xr = R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arb_srcptr xi = R_flint_get_pointer(s_imag);
				for (j = 0; j < ny; ++j) {
					arb_set(acb_realref(y + j), xr + j % nr);
					arb_set(acb_imagref(y + j), xi + j % ni);
				}
			} else {
				for (j = 0; j < ny; ++j) {
					arb_set(acb_realref(y + j), xr + j % nr);
					arb_zero(acb_imagref(y + j));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arb_srcptr xi = R_flint_get_pointer(s_imag);
				for (j = 0; j < ny; ++j) {
					arb_zero(acb_realref(y + j));
					arb_set(acb_imagref(y + j), xi + j % ni);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < ny; ++j)
				acb_zero(y + j);
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (j = 0; j < ny; ++j)
				acb_set_ui(y + j, x[j % nx]);
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (j = 0; j < ny; ++j)
				if (x[j % nx] == NA_LOGICAL)
				acb_set_d(y + j, R_NaN);
				else
				acb_set_si(y + j, x[j % nx]);
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < ny; ++j)
				if (x[j % nx] == NA_INTEGER)
				acb_set_d(y + j, R_NaN);
				else
				acb_set_si(y + j, x[j % nx]);
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (j = 0; j < ny; ++j)
				acb_set_d(y + j, x[j % nx]);
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (j = 0; j < ny; ++j)
				acb_set_d_d(y + j, x[j % nx].r, x[j % nx].i);
			break;
		}
		case STRSXP:
		{
			mpfr_prec_t prec = asPrec(R_NilValue, __func__);
			mpfr_rnd_t rnd = asRnd(R_NilValue, __func__);
			mpfr_t m, r;
			arf_t tmp;
			mpfr_init2(m, prec);
			mpfr_init2(r, MAG_BITS << 1);
			arf_init(tmp);
			const char *s;
			char *t;
			for (j = 0; j < ny; ++j) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (j % nx)));
#define COMMON \
				do { \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != '(') \
					break; \
				mpfr_strtofr(m, s, &t, 0, rnd); \
				if (t <= s) \
					break; \
				s = t; \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != '+' || *(s++) != '/' || *(s++) != '-') \
					break; \
				mpfr_strtofr(r, s, &t, 0, MPFR_RNDA); \
				if (t <= s) \
					break; \
				s = t; \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != ')') \
					break; \
				while (isspace(*s)) \
					s++; \
				} while (0)
				COMMON;
				if (*s == '\0') {
					arf_set_mpfr(arb_midref(acb_realref(y + j)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_realref(y + j)), tmp);
					arb_zero(acb_imagref(y + j));
				} else if (*(s++) == 'i') {
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arb_zero(acb_realref(y + j));
					arf_set_mpfr(arb_midref(acb_imagref(y + j)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_imagref(y + j)), tmp);
				} else {
					s--;
					if (*(s++) != '+')
						break;
					arf_set_mpfr(arb_midref(acb_realref(y + j)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_realref(y + j)), tmp);
					COMMON;
					if (*(s++) != 'i')
						break;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_set_mpfr(arb_midref(acb_imagref(y + j)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_imagref(y + j)), tmp);
				}
#undef COMMON
			}
			mpfr_clear(m);
			mpfr_clear(r);
			arf_clear(tmp);
			if (j < ny)
				Rf_error(_("invalid input in string conversion"));
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					acb_set_ui(y + j, x[j % nx]);
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					acb_set_si(y + j, x[j % nx]);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					acb_set_fmpz(y + j, x + j % nx);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				for (j = 0; j < ny; ++j) {
					arb_fmpz_div_fmpz(acb_realref(y + j), fmpq_numref(x + j % nx), fmpq_denref(x + j % nx), prec);
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set_mag(arb_midref(acb_realref(y + j)), x + j % nx);
					arf_zero(arb_midref(acb_imagref(y + j)));
					mag_zero(arb_radref(acb_realref(y + j)));
					mag_zero(arb_radref(acb_imagref(y + j)));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set(arb_midref(acb_realref(y + j)), x + j % nx);
					arf_zero(arb_midref(acb_imagref(y + j)));
					mag_zero(arb_radref(acb_realref(y + j)));
					mag_zero(arb_radref(acb_imagref(y + j)));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set(arb_midref(acb_realref(y + j)), acf_realref(x + j % nx));
					arf_set(arb_midref(acb_imagref(y + j)), acf_imagref(x + j % nx));
					mag_zero(arb_radref(acb_realref(y + j)));
					mag_zero(arb_radref(acb_imagref(y + j)));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arb_set(acb_realref(y + j), x + j % nx);
					arb_zero(acb_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					acb_set(y + j, x + j % nx);
				break;
			}
			case R_FLINT_CLASS_INVALID:
				Rf_error(_("foreign external pointer"));
				break;
			}
			break;
		}
		if (s_x != R_NilValue && ny > 0 && ny <= R_XLEN_T_MAX) {
		SEXP sx = Rf_getAttrib(s_x, R_NamesSymbol);
		if (sx != R_NilValue && XLENGTH(sx) > 0) {
		PROTECT(sx);
		if (nx == ny)
		R_do_slot_assign(object, R_flint_symbol_names, sx);
		else {
		SEXP sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
		for (j = 0; j < ny; ++j)
			SET_STRING_ELT(sy, (R_xlen_t) j,
			               STRING_ELT(sx, (R_xlen_t) (j % nx)));
		R_do_slot_assign(object, R_flint_symbol_names, sy);
		}
		UNPROTECT(1);
		}
		}
	}
	return object;
}

SEXP R_flint_acb_part(SEXP object, SEXP s_op)
{
	mp_limb_t j, n = R_flint_get_length(object);
	acb_srcptr x = R_flint_get_pointer(object);
	int op = INTEGER_RO(s_op)[0];
	SEXP ans = PROTECT(newObject("arb"));
	arb_ptr y = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_arb_finalize);
	if (op == 0)
	for (j = 0; j < n; ++j)
		arb_set(y + j, acb_realref(x + j));
	else
	for (j = 0; j < n; ++j)
		arb_set(y + j, acb_imagref(x + j));
	SEXP nms = R_do_slot(object, R_flint_symbol_names);
	if (XLENGTH(nms) > 0) {
		PROTECT(nms);
		R_do_slot_assign(ans, R_flint_symbol_names, nms);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_acb_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acb_srcptr x = R_flint_get_pointer(object);
	Rcomplex *y = COMPLEX(ans);
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = arb_midref(acb_realref(x + j));
		if (arf_is_nan(p))
			y[j].r = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j].r = arf_get_d(p, rnd);
		else {
			y[j].r = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		p = arb_midref(acb_imagref(x + j));
		if (arf_is_nan(p))
			y[j].i = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j].i = arf_get_d(p, rnd);
		else {
			y[j].i = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_acb_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	acb_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	mp_limb_t j, n = RECYCLE2(nx, ny);
	slong prec = asPrec(R_NilValue, __func__);
#define COMMON \
	do { \
	SEXP nms; \
	if ((nx == n && XLENGTH(nms = R_do_slot(s_x, R_flint_symbol_names)) > 0) || \
	    (ny == n && XLENGTH(nms = R_do_slot(s_y, R_flint_symbol_names)) > 0)) { \
		PROTECT(nms); \
		R_do_slot_assign(ans, R_flint_symbol_names, nms); \
		UNPROTECT(1); \
	} \
	} while (0)
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case  8: /*  "==" */
	case  9: /*  "!=" */
	case 14: /*   "&" */
	case 15: /*   "|" */
	{
		ERROR_TOO_LONG(n, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) n));
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_acb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	acb_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
#define COMMON \
	do { \
	SEXP nms = R_do_slot(s_x, R_flint_symbol_names); \
	if (XLENGTH(nms) > 0) { \
		PROTECT(nms); \
		R_do_slot_assign(ans, R_flint_symbol_names, nms); \
		UNPROTECT(1); \
	} \
	} while (0)
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
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (n) ? flint_calloc(n, sizeof(acb_t)) : 0;
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
			if (op != 23 || s_dots != R_NilValue) {
			acb_t tmp;
			acb_init(tmp);
			if (op != 23)
				acb_set_ui(tmp, (op == 24) ? 10 : 2);
			else {
				SEXP s_base = VECTOR_ELT(s_dots, 0);
				if (R_flint_get_length(s_base) == 0)
					Rf_error(_("'%s' of length zero in '%s'"),
					         "base", CHAR(STRING_ELT(s_op, 0)));
				acb_srcptr base = R_flint_get_pointer(s_base);
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
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
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
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, rnd); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
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
				arf_div_fmpz(s, xm, p, prec, rnd); \
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
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0],
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
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];
		SEXP ans = PROTECT(newObject("acb"));
		mp_limb_t s = (op == 52) ? 2 : 1;
		acb_ptr z = flint_calloc(s, sizeof(acb_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_acb_finalize);
		switch (op) {
		case 53: /*     "sum" */
			acb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ACB_CONTAINS_NAN(x + j)))
				acb_add(z, z, x + j, prec);
			break;
		case 54: /*    "prod" */
			acb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ACB_CONTAINS_NAN(x + j)))
				acb_mul(z, z, x + j, prec);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = n;
			acb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ACB_CONTAINS_NAN(x + j)))
				acb_add(z, z, x + j, prec);
				else
				--c;
			if (c == 0)
			acb_indeterminate(z);
			else
			acb_div_ui(z, z, c, prec);
			break;
		}
		}
		UNPROTECT(1);
		return ans;
	}
	case 56: /*     "any" */
	case 57: /*     "all" */
	case 58: /*   "anyNA" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0], anyna = 0;
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*     "any" */
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
		case 57: /*     "all" */
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
		case 58: /*   "anyNA" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(acb_realref(x + j))) ||
				    arf_is_nan(arb_midref(acb_imagref(x + j))))
					break;
			z[0] = j < n;
			break;
		}
		UNPROTECT(1);
		return ans;
	}
	case  3: /*       "is.na" */
	case  4: /*      "is.nan" */
	case  5: /* "is.infinite" */
	case  6: /*   "is.finite" */
	case  7: /*           "!" */
	{
		ERROR_TOO_LONG(n, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) n));
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
	{
		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
#undef COMMON
}
