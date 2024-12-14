#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/arb.h>
#include "flint.h"
#include "acf.h"

static R_INLINE
void acf_neg(acf_t y, const acf_t x)
{
	arf_neg(acf_realref(y), acf_realref(x));
	arf_neg(acf_imagref(y), acf_imagref(x));
	return;
}

static R_INLINE
void acf_conj(acf_t y, const acf_t x)
{
	arf_set(acf_realref(y), acf_realref(x));
	arf_neg(acf_imagref(y), acf_imagref(x));
	return;
}

static R_INLINE
int acf_add(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	a = arf_add(acf_realref(res), acf_realref(x), acf_realref(y), prec, rnd);
	b = arf_add(acf_imagref(res), acf_imagref(x), acf_imagref(y), prec, rnd);
	return a | (b << 1);
}

static R_INLINE
int acf_sub(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	a = arf_sub(acf_realref(res), acf_realref(x), acf_realref(y), prec, rnd);
	b = arf_sub(acf_imagref(res), acf_imagref(x), acf_imagref(y), prec, rnd);
	return a | (b << 1);
}

static R_INLINE
int acf_mul(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
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
int acf_div(acf_t res, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
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

static R_INLINE
int acf_div_fmpz(acf_t res, const acf_t x, const fmpz_t y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	a = arf_div_fmpz(acf_realref(res), acf_realref(x), y, prec, rnd);
	b = arf_div_fmpz(acf_imagref(res), acf_imagref(x), y, prec, rnd);
	return a | (b << 1);
}

static R_INLINE
int acf_is_zero(const acf_t x)
{
	return
		arf_is_zero(acf_realref(x)) &&
		arf_is_zero(acf_imagref(x));
}

static R_INLINE
int acf_is_nan(const acf_t x)
{
	return
		arf_is_nan(acf_realref(x)) ||
		arf_is_nan(acf_imagref(x));
}

static R_INLINE
int acf_is_inf(const acf_t x)
{
	return
		arf_is_inf(acf_realref(x)) ||
		arf_is_inf(acf_imagref(x));
}

static R_INLINE
int acf_is_finite(const acf_t x)
{
	return
		arf_is_finite(acf_realref(x)) &&
		arf_is_finite(acf_imagref(x));
}

static R_INLINE
void acf_zero(acf_t res)
{
	arf_zero(acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}

static R_INLINE
void acf_one(acf_t res)
{
	arf_one (acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}

static R_INLINE
void acf_nan(acf_t res)
{
	arf_nan(acf_realref(res));
	arf_nan(acf_imagref(res));
	return;
}

void R_flint_acf_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
	acf_ptr p = (acf_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		acf_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_acf_initialize(SEXP object, SEXP s_length, SEXP s_x,
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
	acf_ptr y = (acf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acf_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_acf_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arf_srcptr xr = (arf_ptr) R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arf_srcptr xi = (arf_ptr) R_flint_get_pointer(s_imag);
				for (j = 0; j < n; ++j) {
					arf_set(acf_realref(y + j), xr + j % nr);
					arf_set(acf_imagref(y + j), xi + j % ni);
				}
			} else {
				for (j = 0; j < n; ++j) {
					arf_set(acf_realref(y + j), xr + j);
					arf_zero(acf_imagref(y + j));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arf_srcptr xi = (arf_ptr) R_flint_get_pointer(s_imag);
				for (j = 0; j < n; ++j) {
					arf_zero(acf_realref(y + j));
					arf_set(acf_imagref(y + j), xi + j);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < n; ++j) {
				arf_zero(acf_realref(y + j));
				arf_zero(acf_imagref(y + j));
			}
			break;
		case RAWSXP:
		case LGLSXP:
			s_x = Rf_coerceVector(s_x, INTSXP);
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < n; ++j) {
				if (x[j] == NA_INTEGER)
				arf_nan(acf_realref(y + j));
				else
				arf_set_si(acf_realref(y + j), x[j]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (j = 0; j < n; ++j) {
				arf_set_d(acf_realref(y + j), x[j]);
				arf_zero(acf_imagref(y + j));
			}
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (j = 0; j < n; ++j) {
				arf_set_d(acf_realref(y + j), x[j].r);
				arf_set_d(acf_imagref(y + j), x[j].i);
			}
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = (slong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_si(acf_realref(y + j), x[j]);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = (ulong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_ui(acf_realref(y + j), x[j]);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_fmpz(acf_realref(y + j), x + j);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
				int prec = asPrec(R_NilValue, __func__);
				arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
				for (j = 0; j < n; ++j) {
					arf_fmpz_div_fmpz(acf_realref(y + j), fmpq_numref(x + j), fmpq_denref(x + j), prec, rnd);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set(acf_realref(y + j), x + j);
					arf_zero(acf_imagref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					acf_set(y + j, x + j);
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					arf_set_mag(acf_realref(y + j), x + j);
					arf_zero(acf_imagref(y + j));
				}
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
	}
	return object;
}

SEXP R_flint_acf_vector(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	SEXP to = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acf_srcptr x = (acf_ptr) R_flint_get_pointer(from);
	Rcomplex *y = COMPLEX(to);
	arf_t lb, ub;
	arf_srcptr m;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		m = acf_realref(x + j);
		if (arf_is_nan(m))
			y[j].r = R_NaN;
		else if (arf_cmp(m, lb) >= 0 && arf_cmp(m, ub) <= 0)
			y[j].r = arf_get_d(m, rnd);
		else {
			y[j].r = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		m = acf_imagref(x + j);
		if (arf_is_nan(m))
			y[j].i = R_NaN;
		else if (arf_cmp(m, lb) >= 0 && arf_cmp(m, ub) <= 0)
			y[j].i = arf_get_d(m, rnd);
		else {
			y[j].i = (arf_sgn(m) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_acf_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	acf_srcptr
		x = (acf_ptr) R_flint_get_pointer(s_x),
		y = (acf_ptr) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long long int j, n = RECYCLE2(nx, ny);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
#if 0
	case  6: /*   "/" */
#endif
	{
		SEXP ans = newObject("acf");
		acf_ptr z = (acf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acf_t)) : 0);
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
				acf_mul(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
#if 0
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				acf_div(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
#endif
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
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
}

SEXP R_flint_acf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  8: /*    "Conj" */
#if 0
	case 14: /*    "sign" */
	case 15: /*    "sqrt" */
#endif
	case 21: /*  "cumsum" */
	case 22: /* "cumprod" */
	case 48: /*   "round" */
	case 49: /*  "signif" */
	{
		SEXP ans = newObject("acf");
		acf_ptr z = (acf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(acf_t)) : 0);
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
#if 0
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				acf_sgn(z + j, x + j, prec, rnd);
			break;
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
				acf_mul(z + j, z + j - 1, x + j, prec, rnd);
			break;
		case 48: /*   "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0];
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
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
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
		return ans;
	}
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP ans = newObject("acf");
		size_t s = (op == 52) ? 2 : 1;
		acf_ptr z = (acf_ptr) flint_calloc(s, sizeof(acf_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_acf_finalize);
		int narm = LOGICAL_RO(s_dots)[0];
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
			unsigned long long int c = n;
			acf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && acf_is_nan(x + j)))
				acf_add(z, z, x + j, prec, rnd);
				else
				--c;
			if (c == 0)
			acf_nan(z);
			else {
			fmpz_t p;
			fmpz_init(p);
			unsigned int uu[2];
			ucopy(uu, &c);
			fmpz_set_uiui(p, uu[1], uu[0]);
			acf_div_fmpz(z, z, p, prec, rnd);
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
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (!acf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*     "all" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
					anyna = 1;
				else if (acf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*   "anyNA" */
			for (j = 0; j < n; ++j)
				if (acf_is_nan(x + j))
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
		return ans;
	}
	case  9: /*       "Re" */
	case 10: /*       "Im" */
#if 0
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
#endif
	{
		SEXP ans = newObject("arf");
		arf_ptr z = (arf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arf_t)) : 0);
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
#if 0
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (j = 0; j < n; ++j)
				acf_abs(z + j, x + j, prec, rnd);
			break;
		case 12: /*      "Arg" */
			for (j = 0; j < n; ++j)
				acf_arg(z + j, x + j, prec, rnd);
			break;
#endif
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
}
