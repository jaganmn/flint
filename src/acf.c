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
	acf_ptr p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		acf_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_acf_initialize(SEXP object, SEXP s_x, SEXP s_length,
                            SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                            SEXP s_real, SEXP s_imag)
{
	mp_limb_t jy, nx = 0, ny = 0, nr = 1, ni = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	PROTECT(s_dim = validDim(s_dim));
	PROTECT(s_dimnames = validDimNames(s_dimnames, s_dim));
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "real", "imag");
		if (s_real != R_NilValue)
			nr = R_flint_get_length(s_real);
		if (s_imag != R_NilValue)
			ni = R_flint_get_length(s_imag);
		ny = validLength(s_length, s_dim, RECYCLE2(nr, ni));
		if (ny > 0 && (nr == 0 || ni == 0))
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         (nr == 0) ? "real" : "imag");
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
		ny = validLength(s_length, s_dim, nx);
		if (ny > 0 && nx == 0)
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         "x");
	}
	else
		ny = validLength(s_length, s_dim, nx);
	PROTECT(s_names = validNames(s_names, ny));
	acf_ptr y = (ny) ? flint_calloc(ny, sizeof(acf_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_acf_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arf_srcptr xr = R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arf_srcptr xi = R_flint_get_pointer(s_imag);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(acf_realref(y + jy), xr + jy % nr);
					arf_set(acf_imagref(y + jy), xi + jy % ni);
				}
			} else {
				for (jy = 0; jy < ny; ++jy) {
					arf_set(acf_realref(y + jy), xr + jy % nr);
					arf_zero(acf_imagref(y + jy));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arf_srcptr xi = R_flint_get_pointer(s_imag);
				for (jy = 0; jy < ny; ++jy) {
					arf_zero(acf_realref(y + jy));
					arf_set(acf_imagref(y + jy), xi + jy % ni);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (jy = 0; jy < ny; ++jy) {
				arf_zero(acf_realref(y + jy));
				arf_zero(acf_imagref(y + jy));
			}
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				arf_set_ui(acf_realref(y + jy), x[jy % nx]);
				arf_zero(acf_imagref(y + jy));
			}
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (x[jy] == NA_LOGICAL)
				arf_nan(acf_realref(y + jy));
				else
				arf_set_si(acf_realref(y + jy), x[jy % nx]);
				arf_zero(acf_imagref(y + jy));
			}
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (x[jy] == NA_INTEGER)
				arf_nan(acf_realref(y + jy));
				else
				arf_set_si(acf_realref(y + jy), x[jy % nx]);
				arf_zero(acf_imagref(y + jy));
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				arf_set_d(acf_realref(y + jy), x[jy % nx]);
				arf_zero(acf_imagref(y + jy));
			}
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				arf_set_d(acf_realref(y + jy), x[jy % nx].r);
				arf_set_d(acf_imagref(y + jy), x[jy % nx].i);
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
			for (jy = 0; jy < ny; ++jy) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (jy % nx)));
#define COMMON \
				do { \
				mpfr_strtofr(m, s, &t, 0, rnd); \
				if (t <= s) \
					break; \
				s = t; \
				} while (0)
				COMMON;
				if (*s != 'i')
					while (isspace(*s))
						s++;
				if (*s == '\0') {
					arf_set_mpfr(acf_realref(y + jy), m);
					arf_zero(acf_imagref(y + jy));
				} else if (*s == 'i') {
					s++;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_zero(acf_realref(y + jy));
					arf_set_mpfr(acf_imagref(y + jy), m);
				} else if (*s == '+' || *s == '-') {
					arf_set_mpfr(acf_realref(y + jy), m);
					COMMON;
					if (*s != 'i')
						break;
					s++;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_set_mpfr(acf_imagref(y + jy), m);
				} else
					break;
#undef COMMON
			}
			mpfr_clear(m);
			if (jy < ny)
				Rf_error(_("invalid input in string conversion"));
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_ui(acf_realref(y + jy), x[jy % nx]);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_si(acf_realref(y + jy), x[jy % nx]);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_fmpz(acf_realref(y + jy), x + jy % nx);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
				for (jy = 0; jy < ny; ++jy) {
					arf_fmpz_div_fmpz(acf_realref(y + jy), fmpq_numref(x + jy % nx), fmpq_denref(x + jy % nx), prec, rnd);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_mag(acf_realref(y + jy), x + jy % nx);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(acf_realref(y + jy), x + jy % nx);
					arf_zero(acf_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					acf_set(y + jy, x + jy % nx);
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
	setDDNN(object, s_dim, s_dimnames, s_names);
	UNPROTECT(3);
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
	setDDNN1(ans, object);
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

SEXP R_flint_acf_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	acf_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	int dz[3];
	int mop = checkConformable(s_x, s_y, nx, ny, matrixop(op), dz);
	if (mop >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
#if 0
	case  6: /*   "/" */
#endif
	{
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (jz = 0; jz < nz; ++jz)
				acf_add(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
		case 2: /*   "-" */
			for (jz = 0; jz < nz; ++jz)
				acf_sub(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
		case 3: /*   "*" */
			for (jz = 0; jz < nz; ++jz)
				__local_acf_mul(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
#if 0
		case 6: /*   "/" */
			for (jz = 0; jz < nz; ++jz)
				__local_acf_div(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
#endif
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case  8: /*  "==" */
	case  9: /*  "!=" */
	case 14: /*   "&" */
	case 15: /*   "|" */
	{
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(acf_is_nan(x + jz % nx) || acf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: acf_equal(x + jz % nx, y + jz % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(acf_is_nan(x + jz % nx) || acf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: acf_equal(x + jz % nx, y + jz % ny) == 0;
			break;
		case 14: /*   "&" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(acf_is_zero(x + jz % nx) || acf_is_zero(y + jz % ny))
				? 0
				:
				(acf_is_nan (x + jz % nx) || acf_is_nan (y + jz % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(!(acf_is_nan(x + jz % nx) || acf_is_zero(x + jz % nx)) ||
				 !(acf_is_nan(y + jz % ny) || acf_is_zero(y + jz % ny)))
				? 1
				:
				(acf_is_nan (x + jz % nx) || acf_is_nan (y + jz % ny))
				? NA_LOGICAL
				: 0;
			break;
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case 16: /*        "%*%" */
	case 17: /*  "crossprod" */
	case 18: /* "tcrossprod" */
	{
		/* C = A B                            */
		/*                                    */
		/*        %*%: C = Z', A = Y', B = X' */
		/*  crossprod: C = Z', A = Y', B = X  */
		/* tcrossprod: C = Z', A = Y , B = X' */

		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
		mp_limb_t jx, jy, ja, jb;
		acb_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (ny) ? flint_calloc(ny, sizeof(acb_t)) : 0;
		mb->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(y + jy));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(y + jy));
				}
		}
		else
			for (ja = 0; ja < ny; ++ja) {
				arf_set(arb_midref(acb_realref(ma->entries + ja)),
				        acf_realref(y + ja));
				arf_set(arb_midref(acb_imagref(ma->entries + ja)),
				        acf_imagref(y + ja));
			}
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r) {
					arf_set(arb_midref(acb_realref(mb->entries + jb)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(mb->entries + jb)),
					        acf_imagref(x + jx));
				}
		}
		else
			for (jb = 0; jb < nx; ++jb) {
				arf_set(arb_midref(acb_realref(mb->entries + jb)),
				        acf_realref(x + jb));
				arf_set(arb_midref(acb_imagref(mb->entries + jb)),
				        acf_imagref(x + jb));
			}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
		if (mb->r) {
			mb->rows[0] = mb->entries;
			for (i = 1; i < mb->r; ++i)
				mb->rows[i] = mb->rows[i - 1] + mb->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = ma->c;
#endif
		acb_mat_approx_mul(mc, ma, mb, prec);
		for (jz = 0; jz < nz; ++jz) {
			arf_set(acf_realref(z + jz),
			        arb_midref(acf_realref(mc->entries + jz)));
			arf_set(acf_imagref(z + jz),
			        arb_midref(acf_imagref(mc->entries + jz)));
			acb_clear(mc->entries + jz);
		}
		for (ja = 0; ja < ny; ++ja)
			acb_clear(ma->entries + ja);
		for (jb = 0; jb < nx; ++jb)
			acb_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case 19: /*      "solve" */
	case 20: /*  "backsolve" */
	case 21: /* "tbacksolve" */
	{
		/* A C = B                          */
		/*                                  */
		/*      solve: C = Z, A = X , B = Y */
		/*  backsolve: C = Z, A = X , B = Y */
		/* tbacksolve: C = Z, A = X', B = Y */
		int uplo = 'N';
		if (op == 20 || op == 21) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		acb_mat_t mc, ma, mb;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(acb_t)) : 0;
		if (op == 21)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja) {
				arf_set(arb_midref(acb_realref(ma->entries + ja)),
				        acf_realref(x + ja));
				arf_set(arb_midref(acb_imagref(ma->entries + ja)),
				        acf_imagref(x + ja));
			}
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += ma->r - (++i))
				for (j = 0; j <= i; ++j, ++ja) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + ja));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + ja));
				}
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (++i))
				for (j = i; j < ma->c; ++j, ++ja) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + ja));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + ja));
				}
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += ma->c - (++i), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += mb->r) {
				arf_set(arb_midref(acb_realref(mb->entries + jb)),
				        acf_realref(y + jy));
				arf_set(arb_midref(acb_imagref(mb->entries + jb)),
				        acf_imagref(y + jy));
			}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
		if (mb->r) {
			mb->rows[0] = mb->entries;
			for (i = 1; i < mb->r; ++i)
				mb->rows[i] = mb->rows[i - 1] + mb->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = ma->c;
#endif
		if (uplo == 'N')
			singular = !acb_mat_approx_solve(mc, ma, mb, prec);
		else if ((uplo == 'U') == (op != 21)) {
			acb_mat_approx_solve_triu(mc, ma, mb, 0, prec);
			singular = 0;
		}
		else {
			acb_mat_approx_solve_tril(mc, ma, mb, 0, prec);
			singular = 0;
		}
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += mc->c) {
				arf_set(acf_realref(z + jz),
				        arb_midref(acb_realref(mc->entries + jc)));
				arf_set(acf_imagref(z + jz),
				        arb_midref(acb_imagref(mc->entries + jc)));
				acb_clear(mc->entries + jc);
			}
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		for (jb = 0; jb < ny; ++jb)
			acb_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular or precision is insufficient"));
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
}

SEXP R_flint_acf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	acf_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
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
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case  1: /*       "+" */
			for (jz = 0; jz < nz; ++jz)
				acf_set(z + jz, x + jz);
			break;
		case  2: /*       "-" */
			for (jz = 0; jz < nz; ++jz)
				acf_neg(z + jz, x + jz);
			break;
		case  8: /*    "Conj" */
			for (jz = 0; jz < nz; ++jz)
				acf_conj(z + jz, x + jz);
			break;
#ifdef HAVE_ACF_SGN
		case 14: /*    "sign" */
			for (jz = 0; jz < nz; ++jz)
				acf_sgn(z + jz, x + jz, prec, rnd);
			break;
#endif
#ifdef HAVE_ACF_SQRT
		case 15: /*    "sqrt" */
			for (jz = 0; jz < nz; ++jz)
				acf_sqrt(z + jz, x + jz, prec, rnd);
			break;
#endif
		case 21: /*  "cumsum" */
			if (nz) {
			acf_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				acf_add(z + jz, z + jz - 1, x + jz, prec, rnd);
			}
			break;
		case 22: /* "cumprod" */
			if (nz)
			acf_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				__local_acf_mul(z + jz, z + jz - 1, x + jz, prec, rnd);
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
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + jz); \
				zm = acf_partref(z + jz); \
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
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + jz); \
				zm = acf_partref(z + jz); \
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
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acf_partref) \
				do { \
				xm = acf_partref(x + jz); \
				zm = acf_partref(z + jz); \
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
		setDDNN1(ans, s_x);
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
		nz = (op == 52) ? 2 : 1;
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = flint_calloc(nz, sizeof(acf_t));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		switch (op) {
		case 53: /*     "sum" */
			acf_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && acf_is_nan(x + jx)))
				acf_add(z, z, x + jx, prec, rnd);
			break;
		case 54: /*    "prod" */
			acf_one(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && acf_is_nan(x + jx)))
				acf_mul(z, z, x + jx, prec, rnd);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = nx;
			acf_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && acf_is_nan(x + jx)))
				acf_add(z, z, x + jx, prec, rnd);
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
			for (jx = 0; jx < nx; ++jx)
				if (acf_is_nan(x + jx))
					anyna = 1;
				else if (!acf_is_zero(x + jx))
					break;
			z[0] = (jx < nx) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*         "all" */
			for (jx = 0; jx < nx; ++jx)
				if (acf_is_nan(x + jx))
					anyna = 1;
				else if (acf_is_zero(x + jx))
					break;
			z[0] = (jx < nx) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*       "anyNA" */
			for (jx = 0; jx < nx; ++jx)
				if (acf_is_nan(x + jx))
					break;
			z[0] = jx < nx;
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
			for (jx = 0; jx < nx; ++jx) {
				if (acf_is_nan(x + jx))
					anyna = 1;
				else if (!last)
					last = x + jx;
				else if (arf_cmp(acf_realref(last), acf_realref(x + jx)) >  0 ||
				         arf_cmp(acf_imagref(last), acf_imagref(x + jx)) >= 0)
					break;
			}
			else
			for (jx = 0; jx < nx; ++jx) {
				if (acf_is_nan(x + jx))
					anyna = 1;
				else if (!last)
					last = x + jx;
				else if (arf_cmp(acf_realref(last), acf_realref(x + jx)) >  0 ||
				         arf_cmp(acf_imagref(last), acf_imagref(x + jx)) >  0)
					break;
			}
			z[0] = (jx < nx) ? 0 : (!narm && anyna && nx > 1) ? NA_LOGICAL : 1;
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
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case  3: /*       "is.na" */
		case  4: /*      "is.nan" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = acf_is_nan(x + jz) != 0;
			break;
		case  5: /* "is.infinite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = acf_is_inf(x + jz) != 0;
			break;
		case  6: /*   "is.finite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = acf_is_finite(x + jz) != 0;
			break;
		case  7: /*           "!" */
			for (jz = 0; jz < nz; ++jz)
				if (acf_is_nan(x + jz))
				z[jz] = NA_LOGICAL;
				else
				z[jz] = acf_is_zero(x + jz) != 0;
			break;
		}
		setDDNN1(ans, s_x);
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
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case  9: /*       "Re" */
			for (jz = 0; jz < nz; ++jz)
				arf_set(z + jz, acf_realref(x + jz));
			break;
		case 10: /*       "Im" */
			for (jz = 0; jz < nz; ++jz)
				arf_set(z + jz, acf_imagref(x + jz));
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (jz = 0; jz < nz; ++jz)
				acf_abs(z + jz, x + jz, prec, rnd);
			break;
#ifdef HAVE_ACF_ARG
		case 12: /*      "Arg" */
			for (jz = 0; jz < nz; ++jz)
				acf_arg(z + jz, x + jz, prec, rnd);
			break;
#endif
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case 60: /*     "colSums" */
	case 61: /*     "rowSums" */
	case 62: /*    "colMeans" */
	case 63: /*    "rowMeans" */
	{
		int byrow = op == 61 || op == 63, domean = op == 62 || op == 63;

		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		if (dimx == R_NilValue || XLENGTH(dimx) < 2)
			Rf_error(_("'%s' is not a matrix or a higher dimensional array"),
			         "x");
		const int *dx = INTEGER_RO(dimx);
		int ndx = LENGTH(dimx);

		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];

		SEXP s_off = VECTOR_ELT(s_dots, 1);
		if (XLENGTH(s_off) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "dims", CHAR(STRING_ELT(s_op, 0)));
		int off = INTEGER_RO(s_off)[0];
		if (off < 1 || off >= ndx)
			Rf_error(_("'%s' is not in 1:(length(dim(%s))-1)"),
			         "dims", "x");

		SEXP dimz = PROTECT(Rf_allocVector(INTSXP, (byrow) ? off : ndx - off));
		int *dz = INTEGER(dimz), ndz = LENGTH(dimz), k;

		nz = 1;
		for (k = 0; k < ndz; ++k)
			nz *= (mp_limb_t) (dz[k] = dx[(byrow) ? k : off + k]);
		mp_limb_t jt, nt = (nz == 0) ? 0 : nx/nz;

		SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
			dimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue) {
			PROTECT(dimnamesx);
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, ndz));
			for (k = 0; k < ndz; ++k)
				SET_VECTOR_ELT(dimnamesz, k, VECTOR_ELT(dimnamesx, (byrow) ? k : off + k));
			SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol),
				namesdimnamesz = R_NilValue;
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesx);
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, ndz));
				for (k = 0; k < ndz; ++k)
					SET_STRING_ELT(namesdimnamesz, k, STRING_ELT(namesdimnamesx, (byrow) ? k : off + k));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(2);
			}
			UNPROTECT(2);
		}
		PROTECT(dimnamesz);

		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		jx = 0;
		if (byrow) {
			mp_limb_t *c = 0;
			if (domean && nz) {
				c = (void *) R_alloc(nz, sizeof(mp_limb_t));
				memset(c, 0, nz * sizeof(mp_limb_t));
			}
			for (jz = 0; jz < nz; ++jz)
				acf_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					if (!(narm && acf_is_nan(x + jx)))
						acf_add(z + jz, z + jz, x + jx, prec, rnd);
					else if (domean)
						++c[jz];
			if (domean) {
			for (jz = 0; jz < nz; ++jz)
				if (c[jz] == nt)
					acf_nan(z + jz);
				else
					acf_div_ui(z + jz, z + jz, nt - c[jz], prec, rnd);
			}
		} else {
			mp_limb_t c = 0;
			for (jz = 0; jz < nz; ++jz) {
				acf_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					if (!(narm && acf_is_nan(x + jx)))
						acf_add(z + jz, z + jz, x + jx, prec, rnd);
					else if (domean)
						++c;
				if (domean) {
					if (c == nt)
						acf_nan(z + jz);
					else
						acf_div_ui(z + jz, z + jz, nt - c, prec, rnd);
					c = 0;
				}
			}
		}
		if (ndz > 1)
			setDDNN(ans, dimz, dimnamesz, R_NilValue);
		else if (dimnamesz != R_NilValue)
			setDDNN(ans, R_NilValue, R_NilValue, VECTOR_ELT(dimnamesz, 0));
		UNPROTECT(4);
		return ans;
	}
	case 64: /*      "solve" */
	case 65: /*  "backsolve" */
	case 66: /* "tbacksolve" */
	{
		/* A C = I                    */
		/*                            */
		/*      solve: C = Z', A = X' */
		/*  backsolve: C = Z', A = X' */
		/* tbacksolve: C = Z', A = X  */
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("first argument is not a square matrix"));
		int uplo = 'N';
		if (op == 65 || op == 66) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		int i, j, singular;
		mp_limb_t jc, ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		if (op == 64 || op == 65)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja) {
				arf_set(arb_midref(acb_realref(ma->entries + ja)),
				        acf_realref(x + ja));
				arf_set(arb_midref(acb_imagref(ma->entries + ja)),
				        acf_imagref(x + ja));
			}
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j <= i; ++j, ++ja) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + ja));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + ja));
				}
				for (; j < ma->c; ++j, ++ja) {
					arf_zero(arb_midref(acb_realref(ma->entries + ja)));
					arf_zero(arb_midref(acb_imagref(ma->entries + ja)));
				}
			}
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja) {
					arf_zero(arb_midref(acb_realref(ma->entries + ja)));
					arf_zero(arb_midref(acb_imagref(ma->entries + ja)));
				}
				for (; j < ma->c; ++j, ++ja) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + ja));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + ja));
				}
			}
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja) {
					arf_zero(arb_midref(acb_realref(ma->entries + ja)));
					arf_zero(arb_midref(acb_imagref(ma->entries + ja)));
				}
				jx = ja;
				for (; j < ma->c; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
			}
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				jx = ja;
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r) {
					arf_set(arb_midref(acb_realref(ma->entries + ja)),
					        acf_realref(x + jx));
					arf_set(arb_midref(acb_imagref(ma->entries + ja)),
					        acf_imagref(x + jx));
				}
				for (; j < ma->c; ++j, ++ja) {
					arf_zero(arb_midref(acb_realref(ma->entries + ja)));
					arf_zero(arb_midref(acb_imagref(ma->entries + ja)));
				}
			}
			break;
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
#endif
		singular = !acb_mat_approx_inv(mc, ma, prec);
		for (jc = 0; jc < nz; ++jc) {
			arf_set(acf_realref(z + jc),
			        arb_midref(acb_realref(mc->entries + jc)));
			arf_set(acf_imagref(z + jc),
			        arb_midref(acb_imagref(mc->entries + jc)));
			acb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(mc->entries);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular or precision is insufficient"));
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
			dimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue) {
			PROTECT(dimnamesx);
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 0, VECTOR_ELT(dimnamesx, 1));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 0));
			SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol),
				namesdimnamesz = R_NilValue;
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesx);
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 0, STRING_ELT(namesdimnamesx, 1));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 0));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(2);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(2);
		}
		UNPROTECT(2);
		return ans;
	}
#ifdef HAVE_ACB_MAT_INV_CHO_PRECOMP
	case 67: /*   "chol2inv" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		int i;
		mp_limb_t jc, ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		for (ja = 0; ja < nx; ++ja) {
			arf_set(arb_midref(acb_realref(ma->entries + ja)),
			        acf_realref(x + ja));
			arf_neg(arb_midref(acb_imagref(ma->entries + ja)),
			        acf_imagref(x + ja));
			mag_zero(arb_radref(acb_realref(ma->entries + ja)));
			mag_zero(arb_radref(acb_imagref(ma->entries + ja)));
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
#endif
		acb_mat_inv_cho_precomp(mc, ma, prec);
		for (jc = 0; jc < nx; ++jc) {
			arf_set(acf_realref(z + jc),
			        arb_midref(acb_realref(mc->entries + jc)));
			arf_neg(acf_imagref(z + jc),
			        arb_midref(acb_imagref(mc->entries + jc)));
			acb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(mc->entries);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = PROTECT(R_do_slot(s_x, R_flint_symbol_dimnames)),
			dimnamesz = R_NilValue;
		SEXP namesdimnamesx = PROTECT(Rf_getAttrib(dimnamesx, R_NamesSymbol)),
			namesdimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue &&
		    (VECTOR_ELT(dimnamesx, 1) != R_NilValue || namesdimnamesx != R_NilValue)) {
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 0, VECTOR_ELT(dimnamesx, 1));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 0, STRING_ELT(namesdimnamesx, 1));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 1));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(1);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(1);
		}
		UNPROTECT(4);
		return ans;
	}
#endif
#ifdef HAVE_ACB_MAT_CHO
	case 68: /*       "chol" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("acf"));
		acf_ptr z = (nz) ? flint_calloc(nz, sizeof(acf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acf_finalize);
		int i, posdef;
		mp_limb_t jc, ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		for (ja = 0; ja < nx; ++ja) {
			arf_set(arb_midref(acb_realref(ma->entries + ja)),
			        acf_realref(x + ja));
			arf_neg(arb_midref(acb_imagref(ma->entries + ja)),
			        acf_imagref(x + ja));
			mag_zero(arb_radref(acb_realref(ma->entries + ja)));
			mag_zero(arb_radref(acb_imagref(ma->entries + ja)));
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
#endif
		posdef = acb_mat_cho(mc, ma, prec);
		for (jc = 0; jc < nx; ++jc) {
			arf_set(acf_realref(z + jc),
			        arb_midref(acb_realref(mc->entries + jc)));
			arf_neg(acf_imagref(z + jc),
			        arb_midref(acb_imagref(mc->entries + jc)));
			acb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(mc->entries);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		if (!posdef)
			Rf_error(_("matrix is not positive definite or precision is insufficient"));
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = PROTECT(R_do_slot(s_x, R_flint_symbol_dimnames)),
			dimnamesz = R_NilValue;
		SEXP namesdimnamesx = PROTECT(Rf_getAttrib(dimnamesx, R_NamesSymbol)),
			namesdimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue &&
		    (VECTOR_ELT(dimnamesx, 1) != R_NilValue || namesdimnamesx != R_NilValue)) {
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 1));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(1);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(1);
		}
		UNPROTECT(4);
		return ans;
	}
#endif
	case 69: /*        "det" */
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("arf"));
		acf_ptr z = flint_calloc(1, sizeof(acf_t));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_acf_finalize);
		acb_mat_t mx;
		acb_t det;
		mx->r = mx->c = dx[0];
		mx->entries = (nx) ? flint_calloc((size_t) mx->r, sizeof(acb_t)) : 0;
		for (jx = 0; jx < nx; ++jx) {
			arf_set(arb_midref(acb_realref(mx->entries + jx)),
			        acf_realref(x + jx));
			arf_set(arb_midref(acb_imagref(mx->entries + jx)),
			        acf_imagref(x + jx));
			mag_zero(arb_radref(acf_realref(mx->entries + jx)));
			mag_zero(arb_radref(acf_imagref(mx->entries + jx)));
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(acb_ptr)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		acb_init(det);
		acb_mat_det(det, mx, prec);
		arf_set(acf_realref(z), arb_midref(acb_realref(det)));
		arf_set(acf_imagref(z), arb_midref(acb_imagref(det)));
		flint_free(mx->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mx->rows);
#endif
		acb_clear(det);
		UNPROTECT(2);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "acf");
		return R_NilValue;
	}
}
