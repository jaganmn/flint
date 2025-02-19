#include <gmp.h>
#include "noreturn.h"
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/longlong.h>
#include <flint/ulong_extras.h>
#if !defined(__GNUC__)
#include <flint/long_extras.h> /* z_mul_checked */
#endif
#include "flint.h"

#ifdef R_FLINT_ABI_LL
# define __local_mpz_fits_slong_p(op) \
	(((op)->_mp_size ==  0) || \
	 ((op)->_mp_size ==  1 && (op)->_mp_d[0] <= WORD_MAX) || \
	 ((op)->_mp_size == -1 && (op)->_mp_d[0] <= (mp_limb_t) -1 - (mp_limb_t) WORD_MIN + 1))
# define __local_mpz_get_si(op) \
	(((op)->_mp_size == 0) ? 0 : \
	 ((op)->_mp_size >  0) ? (op)->_mp_d[0] & WORD_MAX : \
	 -1 - (mp_limb_signed_t) (((op)->_mp_d[0] - 1) & WORD_MAX))
# define __local_mpz_set_si(rop, op) \
	do { \
		if ((op) == 0) \
			(rop)->_mp_size =  0; \
		else if ((op) > 0) { \
			(rop)->_mp_size =  1; \
			(rop)->_mp_d[0] = (mp_limb_t) (op); \
		} \
		else { \
			(rop)->_mp_size = -1; \
			(rop)->_mp_d[0] = (mp_limb_t) -1 - (mp_limb_t) (op) + 1; \
		} \
	} while (0)
#else
# define __local_mpz_fits_slong_p(op) \
	mpz_fits_slong_p(op)
# define __local_mpz_get_si(op) \
	mpz_get_si(op)
# define __local_mpz_set_si(rop, op) \
	mpz_set_si(rop, op)
#endif

void R_flint_slong_finalize(SEXP x)
{
	slong *p = R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_slong_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	mp_limb_t j, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_x != R_NilValue) {
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
	slong *y = (ny) ? flint_calloc(ny, sizeof(slong)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < ny; ++j)
			y[j] = (slong) x[j % nx];
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_LOGICAL)
			Rf_error(_("NaN is not representable by '%s'"), "slong");
			else
			y[j] = x[j % nx];
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_INTEGER)
			Rf_error(_("NaN is not representable by '%s'"), "slong");
			else
			y[j] = x[j % nx];
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (ISNAN(x[j % nx]))
			Rf_error(_("NaN is not representable by '%s'"), "slong");
#ifdef R_FLINT_ABI_64
			else if (x[j % nx] <  -0x1.0p+63       ||
			         x[j % nx] >=  0x1.0p+63)
#else
			else if (x[j % nx] <= -0x1.0p+31 - 1.0 ||
			         x[j % nx] >=  0x1.0p+31)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "slong");
			else
			y[j] = (slong) x[j % nx];
		}
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (ISNAN(x[j % nx].r))
			Rf_error(_("NaN is not representable by '%s'"), "slong");
#ifdef R_FLINT_ABI_64
			else if (x[j % nx].r <  -0x1.0p+63       ||
			         x[j % nx].r >=  0x1.0p+63)
#else
			else if (x[j % nx].r <= -0x1.0p+31 - 1.0 ||
			         x[j % nx].r >=  0x1.0p+31)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "slong");
			else
			y[j] = (slong) x[j % nx].r;
		}
		break;
	}
	case STRSXP:
	{
		mpz_t r;
		mpz_init(r);
		const char *s;
		for (j = 0; j < ny; ++j) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) (j % nx)));
			if (mpz_set_str(r, s, 0) != 0) {
				mpz_clear(r);
				Rf_error(_("invalid input in string conversion"));
			}
			if (!__local_mpz_fits_slong_p(r)) {
				mpz_clear(r);
				Rf_error(_("converted string not in range of '%s'"), "slong");
			}
			y[j] = __local_mpz_get_si(r);
		}
		mpz_clear(r);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j % nx] > WORD_MAX)
				Rf_error(_("integer not in range of '%s'"), "slong");
				else
				y[j] = (slong) x[j % nx];
			}
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				y[j] = x[j % nx];
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (!fmpz_fits_si(x + j % nx))
				Rf_error(_("integer not in range of '%s'"), "slong");
				else
				y[j] = fmpz_get_si(x + j % nx);
			}
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				fmpz_tdiv_q(q, fmpq_numref(x + j % nx), fmpq_denref(x + j % nx));
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("rational not in range of '%s'"), "slong");
				}
				else
				y[j] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				mag_get_fmpz_lower(q, x + j % nx);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "slong");
				}
				else
				y[j] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				arf_get_fmpz(q, x + j % nx, ARF_RND_DOWN);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "slong");
				}
				else
				y[j] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				arf_get_fmpz(q, acf_realref(x + j % nx), ARF_RND_DOWN);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "slong");
				}
				else
				y[j] = fmpz_get_si(q);
			}
			fmpz_clear(q);
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
	return object;
}

SEXP R_flint_slong_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const slong *x = R_flint_get_pointer(object);
	double *y = REAL(ans);
#ifdef R_FLINT_ABI_64
	fmpz_t tmp;
	fmpz_init(tmp);
	for (j = 0; j < n; ++j) {
		fmpz_set_si(tmp, x[j]);
		y[j] = fmpz_get_d(tmp);
	}
	fmpz_clear(tmp);
#else
	for (j = 0; j < n; ++j)
		y[j] = (double) x[j];
#endif
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_slong_format(SEXP object, SEXP s_base)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP ans = Rf_allocVector(STRSXP, (R_xlen_t) n);
	if (n) {
	PROTECT(ans);
	const slong *x = R_flint_get_pointer(object);
	slong xmin = 0, xmax = 0;
	for (j = 0; j < n; ++j) {
		if (x[j] > xmax)
			xmax = x[j];
		else if (x[j] < xmin)
			xmin = x[j];
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init2(z, 64);
	__local_mpz_set_si(z, (xmin < -xmax) ? xmin : xmax);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	__local_mpz_set_si(z, (xmin < -xmax) ? xmax : xmin);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		__local_mpz_set_si(z, x[j]);
		nc = mpz_sizeinbase(z, abase) + (mpz_sgn(z) < 0);
		if (nc > ncmax)
			nc = ncmax;
		ns = ncmax - nc;
		if (ns > 0 && buffer[ns - 1] != ' ')
			memset(buffer, ' ', ns);
		mpz_get_str(buffer + ns, base, z);
		if (buffer[ncmax - 1] == '\0') {
			memmove(buffer + ns + 1, buffer + ns, nc);
			buffer[ns] = ' ';
		}
		SET_STRING_ELT(ans, (R_xlen_t) j, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	SEXP nms = R_do_slot(object, R_flint_symbol_names);
	if (XLENGTH(nms) > 0) {
		PROTECT(nms);
		Rf_setAttrib(ans, R_NamesSymbol, nms);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	}
	return ans;
}

SEXP R_flint_slong_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const slong
		*x = R_flint_get_pointer(s_x),
		*y = R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	mp_limb_t j, n = RECYCLE2(nx, ny);
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
	case  4: /*  "%%" */
	case  5: /* "%/%" */
	{
		slong *z = (n) ? flint_calloc(n, sizeof(slong)) : 0;
		slong a, b;
		int over = 0;
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j) {
#if !defined(__GNUC__)
				a = x[j % nx];
				b = y[j % ny];
				if ((a >= 0) ? b > WORD_MAX - a : b < WORD_MIN - a)
					break;
				z[j] = a + b;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_saddll_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#else
				if (__builtin_saddl_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_si(Z + j, x[j % nx]);
				fmpz_add_si(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j) {
#if !defined(__GNUC__)
				a = x[j % nx];
				b = y[j % ny];
				if ((a >= 0) ? b < a - WORD_MAX : b > a - WORD_MIN)
					break;
				z[j] = b - a;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_ssubll_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#else
				if (__builtin_ssubl_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_si(Z + j, x[j % nx]);
				fmpz_sub_si(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j) {
#if !defined(__GNUC__)
				if (z_mul_checked(&z[j], x[j % nx], y[j % ny]))
					break;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_smulll_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#else
				if (__builtin_smull_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_si(Z + j, x[j % nx]);
				fmpz_mul_si(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 4: /*  "%%" */
		{
			slong t;
			for (j = 0; j < n; ++j) {
				a = x[j % nx];
				b = y[j % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					z[j] = 0;
				else {
					t = a % b;
					z[j] = (t && (a >= 0) != (b >= 0)) ? t + b : t;
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			break;
		}
		case 5: /* "%/%" */
		{
			slong t;
			for (j = 0; j < n; ++j) {
				a = x[j % nx];
				b = y[j % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					break;
				else {
					t = a / b;
					z[j] = (a % b && (a >= 0) != (b >= 0)) ? t - 1 : t;
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j) {
				b = y[j % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					fmpz_set_ui(Z + j, (ulong) -1 - (ulong) WORD_MIN + 1);
				else {
					t = a / b;
					fmpz_set_si(Z + j, (a % b && (a >= 0) != (b >= 0)) ? t - 1 : t);
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			}
			break;
		}
		}
		SEXP ans = PROTECT(newObject((over) ? "fmpz" : "slong"));
		R_flint_set(ans, z, n, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_slong_finalize));
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (n) ? flint_calloc(n, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				if (y[j % ny]) {
				fmpz_set_si(fmpq_numref(z + j), x[j % nx]);
				fmpz_set_si(fmpq_denref(z + j), y[j % ny]);
				fmpq_canonicalise(z + j);
				}
				else
				Rf_error(_("quotient with 0 is undefined"));
			break;
		case 7: /*   "^" */
		{
			slong b, e;
			fmpz_t t;
			fmpz_init(t);
			for (j = 0; j < n; ++j) {
				b = x[j % nx];
				e = y[j % ny];
				if (b == 0 && e < 0) {
				fmpz_clear(t);
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "slong", "^", "slong", "fmpq");
				}
				fmpz_set_si(t, b);
				if (e >= 0) {
				fmpz_pow_ui(fmpq_numref(z + j), t, (ulong) e);
				fmpz_one(fmpq_denref(z + j));
				} else {
				fmpz_one(fmpq_numref(z + j));
				fmpz_pow_ui(fmpq_denref(z + j), t, (ulong) -1 - (ulong) e + 1);
				fmpq_canonicalise(z + j);
				}
			}
			fmpz_clear(t);
			break;
		}
		}
		COMMON;
		UNPROTECT(1);
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
		ERROR_TOO_LONG(n, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) n));
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] == y[j % ny];
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] != y[j % ny];
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] < y[j % ny];
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] > y[j % ny];
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] <= y[j % ny];
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] >= y[j % ny];
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] && y[j % ny];
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] = x[j % nx] || y[j % ny];
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "slong");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_slong_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	const slong *x = R_flint_get_pointer(s_x);
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
	case  9: /*      "Re" */
	case 10: /*      "Im" */
	case 11: /*     "Mod" */
	case 13: /*     "abs" */
	case 14: /*    "sign" */
	case 15: /*    "sqrt" */
	case 16: /*   "floor" */
	case 17: /* "ceiling" */
	case 18: /*   "trunc" */
	case 19: /*  "cummin" */
	case 20: /*  "cummax" */
	case 21: /*  "cumsum" */
	case 22: /* "cumprod" */
	case 48: /*   "round" */
	case 49: /*  "signif" */
	{
		slong *z = (n) ? flint_calloc(n, sizeof(slong)) : 0;
		int over = 0;
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
		case 16: /*   "floor" */
		case 17: /* "ceiling" */
		case 18: /*   "trunc" */
			for (j = 0; j < n; ++j)
				z[j] = x[j];
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j) {
				if (x[j] == WORD_MIN)
					break;
				z[j] = -x[j];
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j)
				if (x[j] == WORD_MIN)
				fmpz_set_ui(Z + j, (ulong) -1 - (ulong) WORD_MIN + 1);
				else
				fmpz_set_si(Z + j, -x[j]);
			}
			break;
		case 10: /*      "Im" */
			for (j = 0; j < n; ++j)
				z[j] = 0;
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (j = 0; j < n; ++j) {
				if (x[j] == WORD_MIN)
					break;
				z[j] = (x[j] < 0) ? -x[j] : x[j];
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			for (j = 0; j < n; ++j)
				fmpz_set_ui(Z + j, (x[j] < 0) ? (ulong) -1 - (ulong) WORD_MIN + 1 : (ulong) x[j]);
			}
			break;
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				z[j] = (x[j] < 0) ? -1 : (x[j] > 0);
			break;
		case 15: /*    "sqrt" */
		{
			ulong r;
			for (j = 0; j < n; ++j) {
				if (x[j] >= 0)
				z[j] = (slong) n_sqrtrem(&r, (ulong) x[j]);
				else
				r = 1;
				if (r) {
				flint_free(z);
				Rf_error(_("%s(<%s>): value is not in the range of '%s'"),
				         "sqrt", "slong", "slong");
				}
			}
			break;
		}
		case 19: /*  "cummin" */
			if (n) {
			z[0] = x[0];
			for (j = 1; j < n; ++j)
				z[j] = (z[j - 1] <= x[j]) ? z[j - 1] : x[j];
			}
			break;
		case 20: /*  "cummax" */
			if (n) {
			z[0] = x[0];
			for (j = 1; j < n; ++j)
				z[j] = (z[j - 1] >= x[j]) ? z[j - 1] : x[j];
			}
			break;
		case 21: /*  "cumsum" */
			if (n) {
			z[0] = x[0];
			for (j = 1; j < n; ++j) {
#if !defined(__GNUC__)
				if ((z[j - 1] >= 0) ? x[j] > WORD_MAX - z[j - 1] : x[j] < WORD_MIN - z[j - 1])
					break;
				z[j] = z[j - 1] + x[j];
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_saddll_overflow(z[j - 1], x[j], z + j))
					break;
#else
				if (__builtin_saddl_overflow(z[j - 1], x[j], z + j))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_set_si(Z, x[0]);
			for (j = 1; j < n; ++j)
				fmpz_add_si(Z + j, Z + j - 1, x[j]);
			}
			}
			break;
		case 22: /* "cumprod" */
			if (n && x[0]) {
			z[0] = x[0];
			for (j = 1; j < n; ++j) {
				if (x[j]) {
#if !defined(__GNUC__)
				if (z_mul_checked(z + j, z[j - 1], x[j]))
					break;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_smulll_overflow(z[j - 1], x[j], z + j))
					break;
#else
				if (__builtin_smull_overflow(z[j - 1], x[j], z + j))
					break;
#endif
				} else {
					while (j < n)
						z[j++] = 0;
					break;
				}
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_set_si(Z, x[0]);
			for (j = 1; j < n; ++j)
				if (x[j])
				fmpz_mul_si(Z + j, Z + j - 1, x[j]);
				else
				break;
			}
			}
			break;
		case 48: /*   "round" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
			if (digits >= 0)
			for (j = 0; j < n; ++j)
				z[j] = x[j];
#ifdef R_FLINT_ABI_64
			else if (digits <= -20)
#else
			else if (digits <= -10)
#endif
			for (j = 0; j < n; ++j)
				z[j] = 0;
			else {
#ifdef R_FLINT_ABI_64
			if (digits == -19) {
			for (j = 0; j < n; ++j) {
				if (x[j] <= -5000000000000000000L ||
				    x[j] >=  5000000000000000000L)
					break;
				z[j] = 0;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			fmpz_set_ui(t, 10000000000000000000UL);
			for (j = 0; j < n; ++j)
				if (x[j] <= -5000000000000000000L)
				fmpz_neg(Z + j, t);
				else if (x[j] >= 5000000000000000000L)
				fmpz_set(Z + j, t);
			fmpz_clear(t);
			}
			} else {
#endif
			slong i, h, p, q, r, qmin, qmax;
			for (i = digits, p = 1; i < 0; ++i)
				p *= 10;
			h = p / 2; qmin = WORD_MIN / p; qmax = WORD_MAX / p;
			for (j = 0; j < n; ++j) {
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
					if (q > qmax)
						break;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
					if (q < qmin)
						break;
				}
				z[j] = q * p;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			fmpz_set_si(t, p);
			for (j = 0; j < n; ++j) {
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
				}
				fmpz_mul_si(Z + j, t, q);
			}
			fmpz_clear(t);
			}
#ifdef R_FLINT_ABI_64
			}
#endif
			}
			break;
		}
		case 49: /*  "signif" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0],
				clog, i, h, p, q, r;
			if (digits <= 0)
				digits = 1;
			for (j = 0; j < n; ++j) {
				if (x[j]) {
				clog = (slong) n_clog((x[j] >= 0) ? (ulong) x[j] : (ulong) -1 - (ulong) x[j] + 1, 10);
				if (clog <= digits)
				z[j] = x[j];
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
					if (q > WORD_MAX / p)
						break;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
					if (q < WORD_MIN / p)
						break;
				}
				z[j] = q * p;
				}
				}
				else
				z[j] = 0;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			for (j = 0; j < n; ++j) {
				if (x[j]) {
				clog = (slong) n_clog((x[j] >= 0) ? (ulong) x[j] : (ulong) -1 - (ulong) x[j] + 1, 10);
				if (clog <= digits)
				fmpz_set_si(Z + j, x[j]);
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
				}
				fmpz_set_si(t, p);
				fmpz_mul_si(Z + j, t, q);
				}
				}
			}
			}
			break;
		}
		}
		SEXP ans = PROTECT(newObject((over) ? "fmpz" : "slong"));
		R_flint_set(ans, z, n, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_slong_finalize));
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
		if (n == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		mp_limb_t s = (op == 52) ? 2 : 1;
		slong *z = flint_calloc(s, sizeof(slong));
		int over = 0;
		switch (op) {
		case 50: /*     "min" */
			z[0] = x[0];
			for (j = 1; j < n; ++j)
				if (z[0] > x[j])
					z[0] = x[j];
			break;
		case 51: /*     "max" */
			z[0] = x[0];
			for (j = 1; j < n; ++j)
				if (z[0] < x[j])
					z[0] = x[j];
			break;
		case 52: /*   "range" */
			z[0] = z[1] = x[0];
			for (j = 1; j < n; ++j)
				if (z[0] > x[j])
					z[0] = x[j];
				else if (z[1] < x[j])
					z[1] = x[j];
			break;
		case 53: /*     "sum" */
		{
			fmpz_t t;
			fmpz_init(t);
			fmpz_zero(t);
			for (j = 0; j < n; ++j)
				fmpz_add_si(t, t, x[j]);
			if (fmpz_fits_si(t))
				z[0] = fmpz_get_si(t);
			else {
				z[0] = 0;
				fmpz_set((void *) z, t);
				over = 1;
			}
			fmpz_clear(t);
			break;
		}
		case 54: /*    "prod" */
		{
			fmpz_t t;
			fmpz_init(t);
			fmpz_one(t);
			for (j = 0; j < n; ++j) {
				if (x[j])
				fmpz_mul_si(t, t, x[j]);
				else {
				fmpz_zero(t);
				break;
				}
			}
			if (fmpz_fits_si(t))
				z[0] = fmpz_get_si(t);
			else {
				z[0] = 0;
				fmpz_set((void *) z, t);
				over = 1;
			}
			fmpz_clear(t);
			break;
		}
		}
		SEXP ans = PROTECT(newObject((over) ? "fmpz" : "slong"));
		R_flint_set(ans, z, s, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_slong_finalize));
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case 55: /*    "mean" */
	{
		if (n == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = flint_calloc(1, sizeof(fmpq));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 55: /*    "mean" */
		{
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpz_add_si(fmpq_numref(z), fmpq_numref(z), x[j]);
			fmpz_set_ui(fmpq_denref(z), n);
			fmpq_canonicalise(z);
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
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*         "any" */
			for (j = 0; j < n && x[j] == 0; ++j) ;
			z[0] = j <  n;
			break;
		case 57: /*         "all" */
			for (j = 0; j < n && x[j] != 0; ++j) ;
			z[0] = j >= n;
			break;
		case 58: /*       "anyNA" */
			z[0] = 0;
			break;
		case 59: /* "is.unsorted" */
		{
			SEXP s_strict = VECTOR_ELT(s_dots, 1);
			if (XLENGTH(s_strict) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "strictly", CHAR(STRING_ELT(s_op, 0)));
			int strict = LOGICAL_RO(s_strict)[0];
			if (strict)
			for (j = 1; j < n && x[0] <  x[1]; ++j, ++x) ;
			else
			for (j = 1; j < n && x[0] <= x[1]; ++j, ++x) ;
			z[0] = j <  n;
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
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] = 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] = 1;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				z[j] = !x[j];
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "slong");
		return R_NilValue;
	}
#undef COMMON
}
