#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/arb.h>
#include "flint.h"

arf_rnd_t remapRnd(mpfr_rnd_t);

#ifndef HAVE_ACF_IS_ZERO
static R_INLINE
int acf_is_zero(const acf_t x)
{
	return
		arf_is_zero(acf_realref(x)) &&
		arf_is_zero(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_NAN
static R_INLINE
int acf_is_nan(const acf_t x)
{
	return
		arf_is_nan(acf_realref(x)) ||
		arf_is_nan(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_INF
static R_INLINE
int acf_is_inf(const acf_t x)
{
	return
		arf_is_inf(acf_realref(x)) ||
		arf_is_inf(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_FINITE
static R_INLINE
int acf_is_finite(const acf_t x)
{
	return
		arf_is_finite(acf_realref(x)) &&
		arf_is_finite(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_ZERO
static R_INLINE
void acf_zero(acf_t res)
{
	arf_zero(acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}
#endif

#ifndef HAVE_ACF_ONE
static R_INLINE
void acf_one(acf_t res)
{
	arf_one (acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}
#endif

#ifndef HAVE_ACF_NAN
static R_INLINE
void acf_nan(acf_t res)
{
	arf_nan(acf_realref(res));
	arf_nan(acf_imagref(res));
	return;
}
#endif

#ifndef HAVE_ACF_CONJ
static R_INLINE
void acf_conj(acf_t res, const acf_t x)
{
	arf_set(acf_realref(res), acf_realref(x));
	arf_neg(acf_imagref(res), acf_imagref(x));
	return;
}
#endif

#ifndef HAVE_ACF_ABS
static
int acf_abs(arf_t res, const acf_t x, slong prec, arf_rnd_t rnd)
{
	int a;
	arf_t u, v, w;
	arf_init(u);
	arf_init(v);
	arf_init(w);
	arf_mul(u, acf_realref(x), acf_realref(x), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(v, acf_imagref(x), acf_imagref(x), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_add(w, u, v, ARF_PREC_EXACT, ARF_RND_DOWN); /* FIXME */
	a = arf_sqrt(res, w, prec, rnd);
	arf_clear(u);
	arf_clear(v);
	arf_clear(w);
	return a;
}
#endif

#ifndef HAVE_ACF_DIV_FMPZ
static R_INLINE
int acf_div_ui(acf_t res, const acf_t x, ulong y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	a = arf_div_ui(acf_realref(res), acf_realref(x), y, prec, rnd);
	b = arf_div_ui(acf_imagref(res), acf_imagref(x), y, prec, rnd);
	return a | (b << 1);
}
#endif

static R_INLINE
int __local_acf_mul(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	arf_t t, u, v, w;
	arf_init(t);
	arf_init(u);
	arf_init(v);
	arf_init(w);
	arf_mul(t, acf_realref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(u, acf_realref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(v, acf_imagref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(w, acf_imagref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	a = arf_sub(acf_realref(res), t, w, prec, rnd);
	b = arf_add(acf_imagref(res), u, v, prec, rnd);
	arf_clear(t);
	arf_clear(u);
	arf_clear(v);
	arf_clear(w);
	return a | (b << 1);
}

#if 0
static R_INLINE
int __local_acf_div(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
{
	/* FIXME: the result is not correctly rounded here ... */
	int a, b;
	arf_t s, t, u, v, w;
	arf_init(s);
	arf_init(t);
	arf_init(u);
	arf_init(v);
	arf_init(w);
	arf_mul(t, acf_realref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(u, acf_realref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(v, acf_imagref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(w, acf_imagref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	a = arf_sub(acf_realref(res), t, w, prec, rnd);
	b = arf_add(acf_imagref(res), u, v, prec, rnd);
	c = arf_sosq(s, acf_realref(y), acf_imagref(y), prec, rnd);
	a |= arf_div(acf_realref(res), acf_realref(res), s, prec, rnd);
	b |= arf_div(acf_imagref(res), acf_imagref(res), s, prec, rnd);
	arf_clear(s);
	arf_clear(t);
	arf_clear(u);
	arf_clear(v);
	arf_clear(w);
	return a | (b << 1);
}
#endif

void R_flint_acf_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	acf_ptr p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		acf_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_acf_initialize(SEXP object, SEXP s_length, SEXP s_x,
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
	acf_ptr y = (ny) ? flint_calloc(ny, sizeof(acf_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_acf_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arf_srcptr xr = R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arf_srcptr xi = R_flint_get_pointer(s_imag);
				for (j = 0; j < ny; ++j) {
					arf_set(acf_realref(y + j), xr + j % nr);
					arf_set(acf_imagref(y + j), xi + j % ni);
				}
			} else {
				for (j = 0; j < ny; ++j) {
					arf_set(acf_realref(y + j), xr + j % nr);
					arf_zero(acf_imagref(y + j));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arf_srcptr xi = R_flint_get_pointer(s_imag);
				for (j = 0; j < ny; ++j) {
					arf_zero(acf_realref(y + j));
					arf_set(acf_imagref(y + j), xi + j % ni);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < ny; ++j) {
				arf_zero(acf_realref(y + j));
				arf_zero(acf_imagref(y + j));
			}
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (j = 0; j < ny; ++j) {
				arf_set_ui(acf_realref(y + j), x[j % nx]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j] == NA_LOGICAL)
				arf_nan(acf_realref(y + j));
				else
				arf_set_si(acf_realref(y + j), x[j % nx]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j] == NA_INTEGER)
				arf_nan(acf_realref(y + j));
				else
				arf_set_si(acf_realref(y + j), x[j % nx]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (j = 0; j < ny; ++j) {
				arf_set_d(acf_realref(y + j), x[j % nx]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (j = 0; j < ny; ++j) {
				arf_set_d(acf_realref(y + j), x[j % nx].r);
				arf_set_d(acf_imagref(y + j), x[j % nx].i);
			}
			break;
		}
		case STRSXP:
		{
			mpfr_prec_t prec = asPrec(R_NilValue, __func__);
			mpfr_rnd_t rnd = asRnd(R_NilValue, __func__);
			mpfr_t m;
			mpfr_init2(m, prec);
			const char *s;
			char *t;
			for (j = 0; j < ny; ++j) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (j % nx)));
#define COMMON \
				do { \
				mpfr_strtofr(m, s, &t, 0, rnd); \
				if (t <= s) \
					break; \
				s = t; \
				while (isspace(*s)) \
					s++; \
				} while (0)
				COMMON;
				if (*s == '\0') {
					arf_set_mpfr(acf_realref(y + j), m);
					arf_zero(acf_imagref(y + j));
				} else if (*(s++) == 'i') {
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_zero(acf_realref(y + j));
					arf_set_mpfr(acf_imagref(y + j), m);
				} else {
					s--;
					arf_set_mpfr(acf_realref(y + j), m);
					COMMON;
					if (*(s++) != 'i')
						break;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_set_mpfr(acf_imagref(y + j), m);
				}
#undef COMMON
			}
			mpfr_clear(m);
			if (j < ny)
				Rf_error(_("invalid input in string conversion"));
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set_ui(acf_realref(y + j), x[j % nx]);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set_si(acf_realref(y + j), x[j % nx]);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set_fmpz(acf_realref(y + j), x + j % nx);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
				for (j = 0; j < ny; ++j) {
					arf_fmpz_div_fmpz(acf_realref(y + j), fmpq_numref(x + j % nx), fmpq_denref(x + j % nx), prec, rnd);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set_mag(acf_realref(y + j), x + j % nx);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					arf_set(acf_realref(y + j), x + j % nx);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					acf_set(y + j, x + j % nx);
				break;
			}
			case R_FLINT_CLASS_ARB:
			case R_FLINT_CLASS_ACB:
				Rf_error(_("coercion from ball to point is not yet supported"));
				break;
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

SEXP R_flint_acf_part(SEXP object, SEXP s_op)
{
	mp_limb_t j, n = R_flint_get_length(object);
	acf_srcptr x = R_flint_get_pointer(object);
	int op = INTEGER_RO(s_op)[0];
	SEXP ans = PROTECT(newObject("arf"));
	arf_ptr y = (n) ? flint_calloc(n, sizeof(arf_t)) : 0;
	R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_arf_finalize);
	if (op == 0)
	for (j = 0; j < n; ++j)
		arf_set(y + j, acf_realref(x + j));
	else
	for (j = 0; j < n; ++j)
		arf_set(y + j, acf_imagref(x + j));
	SEXP nms = R_do_slot(object, R_flint_symbol_names);
	if (XLENGTH(nms) > 0) {
		PROTECT(nms);
		R_do_slot_assign(ans, R_flint_symbol_names, nms);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_acf_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acf_srcptr x = R_flint_get_pointer(object);
	Rcomplex *y = COMPLEX(ans);
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = acf_realref(x + j);
		if (arf_is_nan(p))
			y[j].r = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j].r = arf_get_d(p, rnd);
		else {
			y[j].r = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		p = acf_imagref(x + j);
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

SEXP R_flint_acf_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	acf_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	mp_limb_t j, n = RECYCLE2(nx, ny);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
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
#if 0
	case  6: /*   "/" */
#endif
	{
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (n) ? flint_calloc(n, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				acf_add(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				acf_sub(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				__local_acf_mul(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
#if 0
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				__local_acf_div(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
#endif
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
				(acf_is_nan(x + j % nx) || acf_is_nan(y + j % ny))
				? NA_LOGICAL
				: acf_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(acf_is_nan(x + j % nx) || acf_is_nan(y + j % ny))
				? NA_LOGICAL
				: acf_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] =
				(acf_is_zero(x + j % nx) || acf_is_zero(y + j % ny))
				? 0
				:
				(acf_is_nan (x + j % nx) || acf_is_nan (y + j % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				(!(acf_is_nan(x + j % nx) || acf_is_zero(x + j % nx)) ||
				 !(acf_is_nan(y + j % ny) || acf_is_zero(y + j % ny)))
				? 1
				:
				(acf_is_nan (x + j % nx) || acf_is_nan (y + j % ny))
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
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_acf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	acf_srcptr x = R_flint_get_pointer(s_x);
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
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  8: /*    "Conj" */
#ifdef HAVE_ACF_SGN
	case 14: /*    "sign" */
#endif
#ifdef HAVE_ACF_SQRT
	case 15: /*    "sqrt" */
#endif
	case 21: /*  "cumsum" */
	case 22: /* "cumprod" */
	case 48: /*   "round" */
	case 49: /*  "signif" */
	{
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (n) ? flint_calloc(n, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case  1: /*       "+" */
			for (j = 0; j < n; ++j)
				acf_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				acf_neg(z + j, x + j);
			break;
		case  8: /*    "Conj" */
			for (j = 0; j < n; ++j)
				acf_conj(z + j, x + j);
			break;
#ifdef HAVE_ACF_SGN
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				acf_sgn(z + j, x + j, prec, rnd);
			break;
#endif
#ifdef HAVE_ACF_SQRT
		case 15: /*    "sqrt" */
			for (j = 0; j < n; ++j)
				acf_sqrt(z + j, x + j, prec, rnd);
			break;
#endif
		case 21: /*  "cumsum" */
			if (n) {
			acf_set(z, x);
			for (j = 1; j < n; ++j)
				acf_add(z + j, z + j - 1, x + j, prec, rnd);
			}
			break;
		case 22: /* "cumprod" */
			if (n)
			acf_set(z, x);
			for (j = 1; j < n; ++j)
				__local_acf_mul(z + j, z + j - 1, x + j, prec, rnd);
			break;
		case 48: /*   "round" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
			fmpz_t p, q;
			arf_t s;
			arf_srcptr xm;
			arf_ptr zm;
			fmpz_init(p);
			fmpz_init(q);
			arf_init(s);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* f ~ c/10^+digits   <=>   c ~ f*10^+digits */
			fmpz_pow_ui(p, p, (ulong) digits);
			for (j = 0; j < n; ++j) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + j); \
				zm = acf_partref(z + j); \
				if (!arf_is_finite(xm)) \
				arf_set(zm, xm); \
				else { \
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, rnd); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
				} \
				} while (0)
				TEMPLATE(acf_realref);
				TEMPLATE(acf_imagref);
#undef TEMPLATE
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + j); \
				zm = acf_partref(z + j); \
				if (!arf_is_finite(xm)) \
				arf_set(zm, xm); \
				else { \
				arf_div_fmpz(s, xm, p, prec, rnd); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				} \
				} while (0)
				TEMPLATE(acf_realref);
				TEMPLATE(acf_imagref);
#undef TEMPLATE
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			arf_clear(s);
			break;
		}
		case 49: /*  "signif" */
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
			arf_srcptr xm;
			arf_ptr zm;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + j); \
				zm = acf_partref(z + j); \
				if (!arf_is_finite(xm)) \
				arf_set(zm, xm); \
				else { \
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
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
				} else { \
				fmpz_pow_ui(p, p, (ulong) (clog - digits)); \
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				} \
				} \
				} while (0)
				TEMPLATE(acf_realref);
				TEMPLATE(acf_imagref);
#undef TEMPLATE
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
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
		SEXP ans = PROTECT(newObject("acf"));
		mp_limb_t s = (op == 52) ? 2 : 1;
		acf_ptr z = flint_calloc(s, sizeof(acf_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case 53: /*     "sum" */
			acf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && acf_is_nan(x + j)))
				acf_add(z, z, x + j, prec, rnd);
			break;
		case 54: /*    "prod" */
			acf_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && acf_is_nan(x + j)))
				acf_mul(z, z, x + j, prec, rnd);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = n;
			acf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && acf_is_nan(x + j)))
				acf_add(z, z, x + j, prec, rnd);
				else
				--c;
			if (c == 0)
			acf_nan(z);
			else
			acf_div_ui(z, z, c, prec, rnd);
			break;
		}
		}
		UNPROTECT(1);
		return ans;
	}
	case 56: /*         "any" */
	case 57: /*         "all" */
	case 58: /*       "anyNA" */
	case 59: /* "is.unsorted" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0], anyna = 0;
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*         "any" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (!acf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*         "all" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (acf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*       "anyNA" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
					break;
			z[0] = j < n;
			break;
		case 59: /* "is.unsorted" */
		{
			SEXP s_strict = VECTOR_ELT(s_dots, 1);
			if (XLENGTH(s_strict) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "strictly", CHAR(STRING_ELT(s_op, 0)));
			int strict = LOGICAL_RO(s_strict)[0];
			acf_srcptr last = (void *) 0;
			if (strict)
			for (j = 0; j < n; ++j) {
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (!last)
					last = x + j;
				else if (arf_cmp(acf_realref(last), acf_realref(x + j)) >  0 ||
				         arf_cmp(acf_imagref(last), acf_imagref(x + j)) >= 0)
					break;
			}
			else
			for (j = 0; j < n; ++j) {
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (!last)
					last = x + j;
				else if (arf_cmp(acf_realref(last), acf_realref(x + j)) >  0 ||
				         arf_cmp(acf_imagref(last), acf_imagref(x + j)) >  0)
					break;
			}
			z[0] = (j < n) ? 0 : (!narm && anyna && n > 1) ? NA_LOGICAL : 1;
			break;
		}
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
				z[j] = acf_is_nan(x + j) != 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] = acf_is_inf(x + j) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] = acf_is_finite(x + j) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
				z[j] = NA_LOGICAL;
				else
				z[j] = acf_is_zero(x + j) != 0;
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
#ifdef HAVE_ACF_ARG
	case 12: /*      "Arg" */
#endif
	case 13: /*      "abs" */
	{
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (n) ? flint_calloc(n, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case  9: /*       "Re" */
			for (j = 0; j < n; ++j)
				arf_set(z + j, acf_realref(x + j));
			break;
		case 10: /*       "Im" */
			for (j = 0; j < n; ++j)
				arf_set(z + j, acf_imagref(x + j));
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (j = 0; j < n; ++j)
				acf_abs(z + j, x + j, prec, rnd);
			break;
#ifdef HAVE_ACF_ARG
		case 12: /*      "Arg" */
			for (j = 0; j < n; ++j)
				acf_arg(z + j, x + j, prec, rnd);
			break;
#endif
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
#undef COMMON
}
