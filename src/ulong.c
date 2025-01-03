#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/longlong.h>
#include <flint/ulong_extras.h>
#include "flint.h"

void R_flint_ulong_finalize(SEXP x)
{
	ulong *p = (ulong *) R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_ulong_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long int j, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
			nx = (unsigned long int) XLENGTH(s_x);
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
	ulong *y = (ulong *) ((ny) ? flint_malloc(ny * sizeof(ulong)) : 0);
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_ulong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < ny; ++j)
			y[j] = x[j % nx];
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_LOGICAL)
			Rf_error(_("NaN is not representable by '%s'"), "ulong");
			else
			y[j] = (ulong) x[j % nx];
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_INTEGER)
			Rf_error(_("NaN is not representable by '%s'"), "ulong");
			else if (x[j % nx] < 0)
			Rf_error(_("integer not in range of '%s'"), "ulong");
			else
			y[j] = (ulong) x[j % nx];
		}
		break;
	}
	case CPLXSXP:
		s_x = Rf_coerceVector(s_x, REALSXP);
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (ISNAN(x[j % nx]))
			Rf_error(_("NaN is not representable by '%s'"), "ulong");
#ifdef R_FLINT_ABI_64
			else if (x[j % nx] <= -1.0 || x[j % nx] >= 0x1.0p+64)
#else
			else if (x[j % nx] <= -1.0 || x[j % nx] >= 0x1.0p+32)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "ulong");
			else
			y[j] = (ulong) x[j % nx];
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
			if (!mpz_fits_ulong_p(r)) {
				mpz_clear(r);
				Rf_error(_("converted string not in range of '%s'"), "ulong");
			}
			y[j] = mpz_get_ui(r);
		}
		mpz_clear(r);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = (slong *) R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j % nx] < 0)
				Rf_error(_("integer not in range of '%s'"), "ulong");
				else
				y[j] = (ulong) x[j % nx];
			}
			break;
		}
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = (ulong *) R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				y[j] = x[j % nx];
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (fmpz_sgn(x + j % nx) < 0 || !fmpz_abs_fits_ui(x + j % nx))
				Rf_error(_("integer not in range of '%s'"), "ulong");
				else
				y[j] = fmpz_get_ui(x + j % nx);
			}
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				fmpz_tdiv_q(q, fmpq_numref(x + j % nx), fmpq_denref(x + j % nx));
				if (fmpz_sgn(q) < 0 || !fmpz_abs_fits_ui(q)) {
				fmpz_clear(q);
				Rf_error(_("rational not in range of '%s'"), "ulong");
				}
				else
				y[j] = fmpz_get_ui(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				mag_get_fmpz_lower(q, x + j % nx);
				if (!fmpz_abs_fits_ui(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "ulong");
				}
				else
				y[j] = fmpz_get_ui(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				arf_get_fmpz(q, x + j % nx, ARF_RND_DOWN);
				if (fmpz_sgn(q) < 0 || !fmpz_abs_fits_ui(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "ulong");
				}
				else
				y[j] = fmpz_get_ui(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < ny; ++j) {
				arf_get_fmpz(q, acf_realref(x + j % nx), ARF_RND_DOWN);
				if (fmpz_sgn(q) < 0 || !fmpz_abs_fits_ui(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of '%s'"), "ulong");
				}
				else
				y[j] = fmpz_get_ui(q);
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
	if (nx == ny)
	R_do_slot_assign(object, R_flint_symbol_names, sx);
	else {
	SEXP sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	for (j = 0; j < ny; ++j)
		SET_STRING_ELT(sy, (R_xlen_t) j,
		               STRING_ELT(sx, (R_xlen_t) (j % nx)));
	R_do_slot_assign(object, R_flint_symbol_names, sy);
	}
	}
	}
	return object;
}

SEXP R_flint_ulong_vector(SEXP object)
{
	unsigned long int j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const ulong *x = (ulong *) R_flint_get_pointer(object);
	double *y = REAL(ans);
#if R_FLINT_ABI_64
	fmpz_t tmp;
	fmpz_init(tmp);
	for (j = 0; j < n; ++j) {
		fmpz_set_ui(tmp, x[j]);
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

SEXP R_flint_ulong_format(SEXP object, SEXP s_base)
{
	unsigned long int j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	const ulong *x = (ulong *) R_flint_get_pointer(object);
	ulong xmax = 0;
	for (j = 0; j < n; ++j)
		if (x[j] > xmax)
			xmax = x[j];
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init(z);
	mpz_set_ui(z, xmax);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		mpz_set_ui(z, x[j]);
		nc = mpz_sizeinbase(z, abase);
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
	return ans;
}

SEXP R_flint_ulong_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const ulong
		*x = (ulong *) R_flint_get_pointer(s_x),
		*y = (ulong *) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long int j, n = RECYCLE2(nx, ny);
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
	case  7: /*   "^" */
	{
		SEXP ans;
		ulong *z = (ulong *) ((n) ? flint_malloc(n * sizeof(ulong)) : 0);
#ifndef __GNUC__
		ulong a;
#endif
		ulong b;
		int over = 0;
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j) {
#ifndef __GNUC__
				a = x[j % nx];
				b = y[j % ny];
				if (b > ULONG_MAX - a)
					break;
				z[j] = a + b;
#else
				if (__builtin_uaddl_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_ui(Z + j, x[j % nx]);
				fmpz_add_ui(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j) {
#ifndef __GNUC__
				a = x[j % nx];
				b = y[j % ny];
				if (b > a)
					break;
				z[j] = b - a;
#else
				if (__builtin_usubl_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_ui(Z + j, x[j % nx]);
				fmpz_sub_ui(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j) {
#ifndef __GNUC__
				a = x[j % nx];
				b = y[j % ny];
				if (a > 0 && b > ULONG_MAX / a)
					break;
				z[j] = a * b;
#else
				if (__builtin_umull_overflow(x[j % nx], y[j % ny], &z[j]))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_ui(Z + j, x[j % nx]);
				fmpz_mul_ui(Z + j, Z + j, y[j % ny]);
			}
			}
			break;
		case 4: /*  "%%" */
			for (j = 0; j < n; ++j) {
				b = y[j % ny];
				if (b)
				z[j] = x[j % nx] % b;
				else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			break;
		case 5: /* "%/%" */
			for (j = 0; j < n; ++j) {
				b = y[j % ny];
				if (b)
				z[j] = x[j % nx] / b;
				else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			break;
		case 7: /*   "^" */
		{
			ulong b, e;
			for (j = 0; j < n; ++j) {
				b = x[j % nx];
				e = y[j % ny];
				if (b <= 1)
					z[j] = b;
				else if (e <= 1)   
					z[j] = (e == 0) ? 1 : b;
#ifdef R_FLINT_ABI_64
				else if (e <= 64 && (int) e * (64 - flint_clz(b)) <= 64)
#else
				else if (e <= 32 && (int) e * (32 - flint_clz(b)) <= 32)
#endif
					z[j] = n_pow(b, e);
				else
					break;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			for (j = 0; j < n; ++j)
				fmpz_ui_pow_ui(Z + j, x[j % nx], y[j % ny]);
			}
			break;
		}
		}
		ans = newObject((over) ? "fmpz" : "ulong");
		R_flint_set(ans, z, n, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_ulong_finalize));
		COMMON;
		return ans;
	}
	case  6: /*   "/" */
	{
		SEXP ans = newObject("fmpq");
		fmpq *z = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				if (y[j % ny]) {
				fmpz_set_ui(fmpq_numref(z + j), x[j % nx]);
				fmpz_set_ui(fmpq_denref(z + j), y[j % ny]);
				fmpq_canonicalise(z + j);
				}
				else
				Rf_error(_("quotient with 0 is undefined"));
			break;
		}
		COMMON;
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
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "ulong");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_ulong_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long int j, n = R_flint_get_length(s_x);
	const ulong *x = (ulong *) R_flint_get_pointer(s_x);
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
		SEXP ans;
		ulong *z = (ulong *) ((n) ? flint_malloc(n * sizeof(ulong)) : 0);
		int over = 0;
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
		case 16: /*   "floor" */
		case 17: /* "ceiling" */
		case 18: /*   "trunc" */
			for (j = 0; j < n; ++j)
				z[j] = x[j];
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j) {
				if (x[j])
					break;
				z[j] = 0;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			for (j = 0; j < n; ++j) {
				fmpz_set_ui(Z + j, x[j]);
				fmpz_neg(Z + j, Z + j);
			}
			}
			break;
		case 10: /*      "Im" */
			for (j = 0; j < n; ++j)
				z[j] = 0;
			break;
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				z[j] = x[j] > 0;
			break;
		case 15: /*    "sqrt" */
		{
			ulong r;
			for (j = 0; j < n; ++j) {
				z[j] = n_sqrtrem(&r, (ulong) x[j]);
				if (r) {
				flint_free(z);
				Rf_error(_("%s(<%s>): value is not in the range of '%s'"),
				         "sqrt", "ulong", "ulong");
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
#ifndef __GNUC__
				if (x[j] > ULONG_MAX - z[j - 1])
					break;
				z[j] = z[j - 1] + x[j];
#else
				if (__builtin_uaddl_overflow(z[j - 1], x[j], z + j))
					break;
#endif
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			fmpz_set_ui(Z, x[0]);
			for (j = 1; j < n; ++j)
				fmpz_add_ui(Z + j, Z + j - 1, x[j]);
			}
			}
			break;
		case 22: /* "cumprod" */
			if (n && x[0]) {
			z[0] = x[0];
			for (j = 1; j < n; ++j) {
				if (x[j]) {
#ifndef __GNUC__
				if (x[j] > ULONG_MAX / z[j - 1])
					break;
#else
				if (__builtin_umull_overflow(z[j - 1], x[j], z + j))
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
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			fmpz_set_ui(Z, x[0]);
			for (j = 1; j < n; ++j)
				if (x[j])
				fmpz_mul_ui(Z + j, Z + j - 1, x[j]);
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
			slong i;
			ulong h, p, q, r, qmax;
			for (i = digits, p = 1; i < 0; ++i)
				p *= 10;
			h = p / 2; qmax = ULONG_MAX / p;
			for (j = 0; j < n; ++j) {
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r > h || q % 2)
						q += 1;
					if (q > qmax)
						break;
				}
				z[j] = q * p;
			}
			over = j < n;
			if (over) {
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			fmpz_t t;
			fmpz_init(t);
			fmpz_set_ui(t, p);
			for (j = 0; j < n; ++j) {
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r > h || q % 2)
						q += 1;
				}
				fmpz_mul_ui(Z + j, t, q);
			}
			fmpz_clear(t);
			}
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
				clog, i;
			ulong h, p, q, r;
			if (digits <= 0)
				digits = 1;
			for (j = 0; j < n; ++j) {
				if (x[j]) {
				clog = (slong) n_clog(x[j], 10);
				if (clog <= digits)
				z[j] = x[j];
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r > h || q % 2)
						q += 1;
					if (q > ULONG_MAX / p)
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
			memset(z, 0, n * sizeof(ulong));
			fmpz *Z = (fmpz *) z;
			fmpz_t t;
			fmpz_init(t);
			for (j = 0; j < n; ++j) {
				if (x[j]) {
				clog = (slong) n_clog(x[j], 10);
				if (clog <= digits)
				fmpz_set_ui(Z + j, x[j]);
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[j] / p;
				r = x[j] % p;
				if (r >= h) {
					if (r > h || q % 2)
						q += 1;
				}
				fmpz_set_ui(t, p);
				fmpz_mul_ui(Z + j, t, q);
				}
				}
			}
			}
			break;
		}
		}
		ans = newObject((over) ? "fmpz" : "ulong");
		R_flint_set(ans, z, n, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_ulong_finalize));
		COMMON;
		return ans;
	}
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
		if (n == 0)
			Rf_error(_("argument of length zero in '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		SEXP ans;
		unsigned long int s = (op == 52) ? 2 : 1;
		ulong *z = (ulong *) flint_malloc(s * sizeof(ulong));
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
				fmpz_add_ui(t, t, x[j]);
			if (fmpz_abs_fits_ui(t))
				z[0] = fmpz_get_ui(t);
			else {
				z[0] = 0;
				fmpz_set((fmpz *) z, t);
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
				fmpz_mul_ui(t, t, x[j]);
				else {
				fmpz_zero(t);
				break;
				}
			}
			if (fmpz_abs_fits_ui(t))
				z[0] = fmpz_get_ui(t);
			else {
				z[0] = 0;
				fmpz_set((fmpz *) z, t);
				over = 1;
			}
			fmpz_clear(t);
			break;
		}
		}
		ans = newObject((over) ? "fmpz" : "ulong");
		R_flint_set(ans, z, s, (R_CFinalizer_t) ((over) ? &R_flint_fmpz_finalize : &R_flint_ulong_finalize));
		COMMON;
		return ans;
	}
	case 55: /*    "mean" */
	{
		if (n == 0)
			Rf_error(_("argument of length zero in '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
		SEXP ans = newObject("fmpq");
		fmpq *z = (fmpq *) flint_calloc(1, sizeof(fmpq));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 55: /*    "mean" */
		{
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpz_add_ui(fmpq_numref(z), fmpq_numref(z), x[j]);
			fmpz_set_ui(fmpq_denref(z), n);
			fmpq_canonicalise(z);
			break;
		}
		}
		return ans;
	}
	case 56: /*         "any" */
	case 57: /*         "all" */
	case 58: /*       "anyNA" */
	case 59: /* "is.unsorted" */
	{
		SEXP ans = Rf_allocVector(LGLSXP, 1);
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
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "ulong");
		return R_NilValue;
	}
#undef COMMON
}
