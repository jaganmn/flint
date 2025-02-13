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

arf_rnd_t remapRnd(mpfr_rnd_t rnd)
{
	switch (rnd) {
	case MPFR_RNDN:
		return ARF_RND_NEAR;
	case MPFR_RNDZ:
		return ARF_RND_DOWN;
	case MPFR_RNDU:
		return ARF_RND_CEIL;
	case MPFR_RNDD:
		return ARF_RND_FLOOR;
	case MPFR_RNDA:
		return ARF_RND_UP;
	default:
		Rf_error(_("should never happen ..."));
		return -1;
	}
}

void R_flint_arf_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	arf_ptr p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		arf_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP s_length, SEXP s_x)
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
	arf_ptr y = (ny) ? flint_calloc(ny, sizeof(arf_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_arf_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (j = 0; j < ny; ++j)
			arf_zero(y + j);
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < ny; ++j)
			arf_set_ui(y + j, x[j % nx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_LOGICAL)
			arf_nan(y + j);
			else
			arf_set_si(y + j, x[j % nx]);
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < ny; ++j) {
			if (x[j % nx] == NA_INTEGER)
			arf_nan(y + j);
			else
			arf_set_si(y + j, x[j % nx]);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < ny; ++j)
			arf_set_d(y + j, x[j % nx]);
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		for (j = 0; j < ny; ++j)
			arf_set_d(y + j, x[j % nx].r);
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
			mpfr_strtofr(m, s, &t, 0, rnd);
			if (t <= s)
				break;
			s = t;
			while (isspace(*s))
				s++;
			if (*s != '\0')
				break;
			arf_set_mpfr(y + j, m);
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
			for (j = 0; j < ny; ++j)
				arf_set_ui(y + j, x[j % nx]);
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				arf_set_si(y + j, x[j % nx]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				arf_set_fmpz(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			slong prec = asPrec(R_NilValue, __func__);
			arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
			for (j = 0; j < ny; ++j)
				arf_fmpz_div_fmpz(y + j, fmpq_numref(x + j % nx), fmpq_denref(x + j % nx), prec, rnd);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				arf_set_mag(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				arf_set(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			for (j = 0; j < ny; ++j)
				arf_set(y + j, acf_realref(x + j % nx));
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

SEXP R_flint_arf_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arf_srcptr x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (arf_is_nan(x + j))
			y[j] = R_NaN;
		else if (arf_cmp(x + j, lb) >= 0 && arf_cmp(x + j, ub) <= 0)
			y[j] = arf_get_d(x + j, rnd);
		else {
			y[j] = (arf_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arf_format(SEXP object, SEXP s_base,
                        SEXP s_digits, SEXP s_sep, SEXP s_rnd)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	mpfr_rnd_t rnd = asRnd(s_rnd, __func__);
	SEXP ans = Rf_allocVector(STRSXP, (R_xlen_t) n);
	if (n) {
	PROTECT(ans);
	arf_srcptr x = R_flint_get_pointer(object);
	mpfr_exp_t e__;
	slong p__;
	mpfr_uexp_t e, emax = 0;
	mpfr_prec_t p, pmax = 0;
	char work[4];
	unsigned int flags = 0;
	mpz_t z;
	mpfr_t f;
	MPFR_ERANGE_SET;
	mpz_init(z);
	mpfr_init2(f, FLINT_BITS);
	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		if (mpfr_regular_p(f)) {
			flags |= 1;
			if (mpfr_sgn(f) < 0)
				flags |= 2;
			mpfr_get_str(work, &e__, base, 2, f, rnd);
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			if (e > emax)
				emax = e;
			p__ = arf_bits(x + j);
			p = (p__ <= MPFR_PREC_MAX) ? (mpfr_prec_t) p__ : MPFR_PREC_MAX;
			if (p > pmax)
				pmax = p;
		}
		else if (mpfr_zero_p(f))
			flags |= 1;
		else if (mpfr_inf_p(f) && mpfr_sgn(f) < 0)
			flags |= 4;
	}

	if (flags & 1) {

	mpz_set_ui(z, emax);
	mpfr_set_prec(f, (pmax == 0) ? 1 : pmax);
	if (digits == 0)
		digits = (pmax == 0) ? 1 : mpfr_get_str_ndigits(abase, pmax);

	size_t ns, nc,
		ncsgn = (flags & 2) ? 1 : 0,
		ncrad = (digits > 1) ? 1 : 0,
		ncman = digits,
		ncsep = strlen(sep),
		ncexp = mpz_sizeinbase(z, abase),
		ncmax = ncsgn + ncrad + ncman + ncsep + 1;
	char *buffer = R_alloc(ncmax + ncexp + 1, 1);
	mpz_get_str(buffer, base, z);
	ncexp = strlen(buffer);
	ncmax += ncexp;
	char
		*bufman = buffer + (ncsgn + ncrad),
		*bufsep = buffer + (ncsgn + ncrad + ncman),
		*bufexp = buffer + (ncsgn + ncrad + ncman + ncsep + 1),
		*bufnan = buffer + (ncmax - 4);
	buffer[ncmax] = '\0';

	memset(buffer, ' ', ncmax - 4);
	memcpy(bufnan, "-Inf", 4);
	SEXP s_neg_inf = PROTECT(Rf_mkChar(buffer));
	memcpy(bufnan, " Inf", 4);
	SEXP s_pos_inf = PROTECT(Rf_mkChar(buffer));
	memcpy(bufnan, " NaN", 4);
	SEXP s_nan     = PROTECT(Rf_mkChar(buffer));
	memset(buffer, '0', ncmax);
	if (ncsgn)
		buffer[0] = ' ';
	if (ncrad)
		bufman[0] = '.';
	memcpy(bufsep, sep, ncsep);
	bufexp[-1] = '+';
	SEXP s_zero    = PROTECT(Rf_mkChar(buffer));

	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		if (!mpfr_regular_p(f))
			SET_STRING_ELT(ans, (R_xlen_t) j,
			               (mpfr_zero_p(f)) ? s_zero :
			               (mpfr_nan_p(f)) ? s_nan :
			               (mpfr_sgn(f) < 0) ? s_neg_inf : s_pos_inf);
		else {
			/* Sign */
			if (ncsgn)
				buffer[0] = (mpfr_sgn(f) < 0) ? '-' : ' ';
			/* Mantissa */
			mpfr_abs(f, f, rnd);
			mpfr_get_str(bufman, &e__, base, ncman, f, rnd);
			/* Radix point */
			if (ncrad) {
				bufman[-1] = bufman[0];
				bufman[0] = '.';
			}
			/* Separator */
			bufsep[0] = sep[0];
			/* Exponent sign */
			bufexp[-1] = (e__ <= 0) ? '-' : '+';
			/* Exponent absolute value */
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			mpz_set_ui(z, e);
			nc = mpz_sizeinbase(z, abase);
			if (nc > ncexp)
				nc = ncexp;
			ns = ncexp - nc;
			if (ns > 0)
				memset(bufexp, '0', ns);
			mpz_get_str(bufexp + ns, base, z);
			if (bufexp[ncexp - 1] == '\0') {
				memmove(bufexp + ns + 1, bufexp + ns, nc);
				bufexp[ns] = '0';
			}
			SET_STRING_ELT(ans, (R_xlen_t) j, Rf_mkChar(buffer));
		}
	}

	UNPROTECT(4);

	} else {

	SEXP
		s_neg_inf = PROTECT(Rf_mkChar(              "-Inf"        )),
		s_pos_inf = PROTECT(Rf_mkChar((flags & 4) ? " Inf" : "Inf")),
		s_nan     = PROTECT(Rf_mkChar((flags & 4) ? " NaN" : "NaN"));

	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		SET_STRING_ELT(ans, (R_xlen_t) j,
		               (mpfr_nan_p(f)) ? s_nan :
		               (mpfr_sgn(f) < 0) ? s_neg_inf : s_pos_inf);
	}

	UNPROTECT(3);

	}

	mpz_clear(z);
	mpfr_clear(f);
	MPFR_ERANGE_RESET;
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

SEXP R_flint_arf_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	arf_srcptr
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
	case  6: /*   "/" */
	{
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (n) ? flint_calloc(n, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				arf_add(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				arf_sub(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				arf_mul(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				arf_div(z + j, x + j % nx, y + j % ny, prec, rnd);
			break;
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
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_cmp(x + j % nx, y + j % ny) < 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_cmp(x + j % nx, y + j % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_cmp(x + j % nx, y + j % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: arf_cmp(x + j % nx, y + j % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] =
				(arf_is_zero(x + j % nx) || arf_is_zero(y + j % ny))
				? 0
				:
				(arf_is_nan (x + j % nx) || arf_is_nan (y + j % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				(!(arf_is_nan(x + j % nx) || arf_is_zero(x + j % nx)) ||
				 !(arf_is_nan(y + j % ny) || arf_is_zero(y + j % ny)))
				? 1
				:
				(arf_is_nan (x + j % nx) || arf_is_nan (y + j % ny))
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
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_arf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	arf_srcptr x = R_flint_get_pointer(s_x);
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
	case  9: /*      "Re" */
	case 10: /*      "Im" */
	case 11: /*     "Mod" */
	case 12: /*     "Arg" */
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
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (n) ? flint_calloc(n, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
			for (j = 0; j < n; ++j)
				arf_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				arf_neg(z + j, x + j);
			break;
		case 10: /*      "Im" */
			for (j = 0; j < n; ++j)
				arf_zero(z + j);
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (j = 0; j < n; ++j)
				arf_abs(z + j, x + j);
			break;
		case 12: /*     "Arg" */
		{
			arb_t pi;
			arb_init(pi);
			arb_const_pi(pi, prec);
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j) || arf_is_zero(x + j))
					arf_set(z + j, x + j);
				else if (arf_sgn(x + j) > 0)
					arf_set(z + j, arb_midref(pi));
				else
					arf_neg(z + j, arb_midref(pi));
			arb_clear(pi);
			break;
		}
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
				arf_nan(z + j);
				else
				arf_set_si(z + j, arf_sgn(x + j));
			break;
		case 15: /*    "sqrt" */
			for (j = 0; j < n; ++j)
				arf_sqrt(z + j, x + j, prec, rnd);
			break;
		case 16: /*   "floor" */
			for (j = 0; j < n; ++j)
				arf_floor(z + j, x + j);
			break;
		case 17: /* "ceiling" */
			for (j = 0; j < n; ++j)
				arf_ceil(z + j, x + j);
			break;
		case 18: /*   "trunc" */
			for (j = 0; j < n; ++j)
				if (arf_sgn(x + j) >= 0)
				arf_floor(z + j, x + j);
				else
				arf_ceil(z + j, x + j);
			break;
		case 19: /*  "cummin" */
			if (n) {
			arf_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(x + j); ++j)
				arf_min(z + j, last, x + j);
			for (; j < n; ++j)
				arf_nan(z + j);
			}
			break;
		case 20: /*  "cummax" */
			if (n) {
			arf_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(x + j); ++j)
				arf_max(z + j, last, x + j);
			for (; j < n; ++j)
				arf_nan(z + j);
			}
			break;
		case 21: /*  "cumsum" */
			if (n) {
			arf_set(z, x);
			for (j = 1; j < n; ++j)
				arf_add(z + j, z + j - 1, x + j, prec, rnd);
			}
			break;
		case 22: /* "cumprod" */
			if (n)
			arf_set(z, x);
			for (j = 1; j < n; ++j)
				arf_mul(z + j, z + j - 1, x + j, prec, rnd);
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
			fmpz_init(p);
			fmpz_init(q);
			arf_init(s);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* f ~ c/10^+digits   <=>   c ~ f*10^+digits */
			fmpz_pow_ui(p, p, (ulong) digits);
			for (j = 0; j < n; ++j) {
				if (!arf_is_finite(x + j))
				arf_set(z + j, x + j);
				else {
				arf_mul_fmpz(s, x + j, p, ARF_PREC_EXACT, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(z + j, q, p, prec, rnd);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
				if (!arf_is_finite(x + j))
				arf_set(z + j, x + j);
				else {
				arf_div_fmpz(s, x + j, p, prec, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				arf_set_fmpz(z + j, q);
				}
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
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (!arf_is_finite(x + j))
				arf_set(z + j, x + j);
				else {
				arf_get_fmpq(a, x + j);
				fmpq_abs(a, a);
				clog = fmpq_clog_ui(a, 10);
				if (arf_sgn(x + j) < 0)
					fmpq_neg(a, a);
				fmpz_set_si(p, 10);
				if (clog <= digits) {
				if (clog >= 0)
				fmpz_pow_ui(p, p, (ulong) (digits - clog));
				else
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1));
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				arf_fmpz_div_fmpz(z + j, q, p, prec, rnd);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				arf_set_fmpz(z + j, q);
				}
				}
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
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];
		SEXP ans = PROTECT(newObject("arf"));
		mp_limb_t s = (op == 52) ? 2 : 1;
		arf_ptr z = flint_calloc(s, sizeof(arf_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case 50: /*     "min" */
			arf_pos_inf(z);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(x + j)) {
					if (arf_cmp(z, x + j) > 0)
						arf_set(z, x + j);
				}
				else if (!narm) {
					arf_nan(z);
					break;
				}
			break;
		case 51: /*     "max" */
			arf_neg_inf(z);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(x + j)) {
					if (arf_cmp(z, x + j) < 0)
						arf_set(z, x + j);
				}
				else if (!narm) {
					arf_nan(z);
					break;
				}
			break;
		case 52: /*   "range" */
			arf_pos_inf(z);
			arf_neg_inf(z + 1);
			for (j = 0; j < n; ++j)
				if (!arf_is_nan(x + j)) {
					if (arf_cmp(z, x + j) > 0)
						arf_set(z, x + j);
					if (arf_cmp(z + 1, x + j) < 0)
						arf_set(z + 1, x + j);
				}
				else if (!narm) {
					arf_nan(z);
					arf_nan(z + 1);
					break;
				}
			break;
		case 53: /*     "sum" */
			arf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(x + j)))
				arf_add(z, z, x + j, prec, rnd);
			break;
		case 54: /*    "prod" */
			arf_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(x + j)))
				arf_mul(z, z, x + j, prec, rnd);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = n;
			arf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(x + j)))
				arf_add(z, z, x + j, prec, rnd);
				else
				--c;
			if (c == 0)
			arf_nan(z);
			else
			arf_div_ui(z, z, c, prec, rnd);
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
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (!arf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*         "all" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (arf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*       "anyNA" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
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
			arf_srcptr last = (void *) 0;
			if (strict)
			for (j = 0; j < n; ++j) {
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (!last)
					last = x + j;
				else if (arf_cmp(last, x + j) >= 0)
					break;
			}
			else
			for (j = 0; j < n; ++j) {
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (!last)
					last = x + j;
				else if (arf_cmp(last, x + j) >  0)
					break;
			}
			z[0] = (j < n) ? 1 : (!narm && anyna && n > 1) ? NA_LOGICAL : 0;
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
				z[j] = arf_is_nan(x + j) != 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] = arf_is_inf(x + j) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] = arf_is_finite(x + j) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
				z[j] = NA_LOGICAL;
				else
				z[j] = arf_is_zero(x + j) != 0;
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
#undef COMMON
}
