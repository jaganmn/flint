#include <gmp.h>
#include "noreturn.h"
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include "flint.h"

void R_flint_fmpz_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	fmpz *p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		fmpz_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP s_length, SEXP s_x)
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
	fmpz *y = (ny) ? flint_calloc(ny, sizeof(fmpz)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < ny; ++j)
			fmpz_set_ui(y + j, x[j % nx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_LOGICAL)
			Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
			else
			fmpz_set_si(y + j, x[j % nx]);
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_INTEGER)
			Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
			else
			fmpz_set_si(y + j, x[j % nx]);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (!R_FINITE(x[j % nx]))
			Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
			else
			fmpz_set_d(y + j, (fabs(x[j % nx]) < DBL_MIN) ? 0.0 : x[j % nx]);
		}
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (!R_FINITE(x[j % nx].r))
			Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
			else
			fmpz_set_d(y + j, (fabs(x[j % nx].r) < DBL_MIN) ? 0.0 : x[j % nx].r);
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
			fmpz_set_mpz(y + j, r);
		}
		mpz_clear(r);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				fmpz_set_ui(y + j, x[j % nx]);
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				fmpz_set_si(y + j, x[j % nx]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				fmpz_set(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				fmpz_tdiv_q(y + j, fmpq_numref(x + j % nx), fmpq_denref(x + j % nx));
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (mag_is_inf(x + j % nx))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
				else
				mag_get_fmpz_lower(y + j, x + j % nx);
			}
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (!arf_is_finite(x + j % nx))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
				else
				arf_get_fmpz(y + j, x + j % nx, ARF_RND_DOWN);
			}
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j) {
				if (!arf_is_finite(acf_realref(x + j % nx)))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
				else
				arf_get_fmpz(y + j, acf_realref(x + j % nx), ARF_RND_DOWN);
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

SEXP R_flint_fmpz_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const fmpz *x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	int w = 1;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, lb) >= 0 && fmpz_cmp(x + j, ub) <= 0)
			y[j] = fmpz_get_d(x + j);
		else {
			y[j] = (fmpz_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_fmpz_format(SEXP object, SEXP s_base)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP ans = Rf_allocVector(STRSXP, (R_xlen_t) n);
	if (n) {
	PROTECT(ans);
	const fmpz *x = R_flint_get_pointer(object);
	mp_limb_t jmax = 0, jmin = 0;
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, x + jmax) > 0)
			jmax = j;
		else if (fmpz_cmp(x + j, x + jmin) < 0)
			jmin = j;
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init(z);
	fmpz_get_mpz(z, x + ((fmpz_cmpabs(x + jmin, x + jmax) >= 0) ? jmin : jmax));
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	fmpz_get_mpz(z, x + ((fmpz_cmpabs(x + jmin, x + jmax) <= 0) ? jmin : jmax));
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(z, x + j);
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

SEXP R_flint_fmpz_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const fmpz
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
		SEXP ans = PROTECT(newObject("fmpz"));
		fmpz *z = (n) ? flint_calloc(n, sizeof(fmpz)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				fmpz_add(z + j, x + j % nx, y + j % ny);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				fmpz_sub(z + j, x + j % nx, y + j % ny);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				fmpz_mul(z + j, x + j % nx, y + j % ny);
			break;
		case 4: /*  "%%" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_r(z + j, x + j % nx, y + j % ny);
			break;
		case 5: /* "%/%" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_q(z + j, x + j % nx, y + j % ny);
			break;
		}
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
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpz_set(fmpq_numref(z + j), x + j % nx);
				fmpz_set(fmpq_denref(z + j), y + j % ny);
				fmpq_canonicalise(z + j);
				}
			break;
		case 7: /*   "^" */
		{
			const fmpz *b, *e;
			ulong u;
			fmpz_t a;
			fmpz_init(a);
			for (j = 0; j < n; ++j) {
				b = x + j % nx;
				e = y + j % ny;
				if (fmpz_is_zero(b) && fmpz_sgn(e) < 0) {
				fmpz_clear(a);
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "fmpz", "^", "fmpz", "fmpq");
				}
				if (!fmpz_abs_fits_ui(e)) {
				fmpz_clear(a);
				Rf_error(_("<%s> %s <%s>: exponent exceeds maximum %llu in absolute value"),
				         "fmpz", "^", "fmpz", (unsigned long long int) UWORD_MAX);
				}
				if (fmpz_sgn(e) >= 0) {
				u = fmpz_get_ui(e);
				fmpz_pow_ui(fmpq_numref(z + j), b, u);
				fmpz_one(fmpq_denref(z + j));
				} else {
				fmpz_neg(a, e);
				u = fmpz_get_ui(a);
				fmpz_one(fmpq_numref(z + j));
				fmpz_pow_ui(fmpq_denref(z + j), b, u);
				fmpq_canonicalise(z + j);
				}
			}
			fmpz_clear(a);
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
				z[j] = fmpz_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) < 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpz_is_zero(x + j % nx) && !fmpz_is_zero(y + j % ny);
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpz_is_zero(x + j % nx) || !fmpz_is_zero(y + j % ny);
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_fmpz_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	const fmpz *x = R_flint_get_pointer(s_x);
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
		SEXP ans = PROTECT(newObject("fmpz"));
		fmpz *z = (n) ? flint_calloc(n, sizeof(fmpz)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
		case 16: /*   "floor" */
		case 17: /* "ceiling" */
		case 18: /*   "trunc" */
			for (j = 0; j < n; ++j)
				fmpz_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				fmpz_neg(z + j, x + j);
			break;
		case 10: /*      "Im" */
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (j = 0; j < n; ++j)
				fmpz_abs(z + j, x + j);
			break;
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				fmpz_set_si(z + j, fmpz_sgn(x + j));
			break;
		case 15: /*    "sqrt" */
		{
			fmpz_t r;
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpz_sgn(x + j) >= 0)
				fmpz_sqrtrem(z + j, r, x + j);
				if (!(fmpz_sgn(x + j) >= 0 && fmpz_is_zero(r))) {
				fmpz_clear(r);
				Rf_error(_("%s(<%s>): value is not in the range of '%s'"),
				         "sqrt", "fmpz", "fmpz");
				}
			}
			fmpz_clear(r);
			break;
		}
		case 19: /*  "cummin" */
			if (n) {
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_set(z + j, (fmpz_cmp(z + j - 1, x + j) <= 0) ? z + j - 1 : x + j);
			}
			break;
		case 20: /*  "cummax" */
			if (n) {
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_set(z + j, (fmpz_cmp(z + j - 1, x + j) >= 0) ? z + j - 1 : x + j);
			}
			break;
		case 21: /*  "cumsum" */
			if (n) {
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_add(z + j, z + j - 1, x + j);
			}
			break;
		case 22: /* "cumprod" */
			if (n) {
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_mul(z + j, z + j - 1, x + j);
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
				fmpz_set(z + j, x + j);
			else {
			fmpz_t p, q, r;
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			fmpz_set_si(p, 10);
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
				fmpz_ndiv_qr(q, r, x + j, p);
				if (fmpz_cmp2abs(p, r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + j, q, p);
			}
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
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
				clog;
			if (digits <= 0)
				digits = 1;
			fmpz_t a, p, q, r;
			fmpz_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpz_is_zero(x + j))
				fmpz_zero(z + j);
				else {
				fmpz_abs(a, x + j);
				clog = fmpz_clog_ui(a, 10);
				if (clog <= digits)
				fmpz_set(z + j, x + j);
				else {
				fmpz_set_si(p, 10);
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_ndiv_qr(q, r, x + j, p);
				if (fmpz_cmp2abs(p, r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + j, q, p);
				}
				}
			}
			fmpz_clear(a);
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
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
		if (n == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		SEXP ans = PROTECT(newObject("fmpz"));
		mp_limb_t s = (op == 52) ? 2 : 1;
		fmpz *z = flint_calloc(s, sizeof(fmpz));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		switch (op) {
		case 50: /*     "min" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z, x + j) > 0)
					fmpz_set(z, x + j);
			break;
		case 51: /*     "max" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z, x + j) < 0)
					fmpz_set(z, x + j);
			break;
		case 52: /*   "range" */
			fmpz_set(z, x);
			fmpz_set(z + 1, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z, x + j) > 0)
					fmpz_set(z, x + j);
				else if (fmpz_cmp(z + 1, x + j) < 0)
					fmpz_set(z + 1, x + j);
			break;
		case 53: /*     "sum" */
			fmpz_zero(z);
			for (j = 0; j < n; ++j)
				fmpz_add(z, z, x + j);
			break;
		case 54: /*    "prod" */
			fmpz_one(z);
			for (j = 0; j < n; ++j)
				fmpz_mul(z, z, x + j);
			break;
		}
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
				fmpz_add(fmpq_numref(z), fmpq_numref(z), x + j);
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
			for (j = 0; j < n &&  fmpz_is_zero(x + j); ++j) ;
			z[0] = j <  n;
			break;
		case 57: /*         "all" */
			for (j = 0; j < n && !fmpz_is_zero(x + j); ++j) ;
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
			for (j = 1; j < n && fmpz_cmp(x, x + 1) <  0; ++j, ++x) ;
			else
			for (j = 1; j < n && fmpz_cmp(x, x + 1) <= 0; ++j, ++x) ;
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
				z[j] = fmpz_is_zero(x + j) != 0;
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
#undef COMMON
}
