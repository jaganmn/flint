#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/arb.h>
#include "flint.h"

int asRnd(SEXP rnd, int gnu, const char *where)
{
	if (rnd == R_NilValue) {
		static SEXP tag = NULL;
		if (!tag)
			tag = Rf_install("flint.rnd");
		rnd = Rf_GetOption1(tag);
		if (rnd == R_NilValue)
			return ARF_RND_NEAR;
	}
	if (TYPEOF(rnd) == STRSXP && XLENGTH(rnd) > 0 &&
	    (rnd = STRING_ELT(rnd, 0)) != NA_STRING) {
		switch (CHAR(rnd)[0]) {
		case 'N': case 'n':
			return (gnu) ? MPFR_RNDN : ARF_RND_NEAR;
		case 'Z': case 'z':
			return (gnu) ? MPFR_RNDZ : ARF_RND_DOWN;
		case 'U': case 'u':
			return (gnu) ? MPFR_RNDU : ARF_RND_CEIL;
		case 'D': case 'd':
			return (gnu) ? MPFR_RNDD : ARF_RND_FLOOR;
		case 'A': case 'a':
			return (gnu) ? MPFR_RNDA : ARF_RND_UP;
		}
	}
	Rf_error(_("invalid '%s' in '%s'"), "rnd", where);
	return 0;
}

void R_flint_arf_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
	arf_ptr p = (arf_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		arf_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int j, n;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
		n = (unsigned long long int) XLENGTH(s_x);
		else if ((class = R_flint_get_class(s_x)) != R_FLINT_CLASS_INVALID)
		n = R_flint_get_length(s_x);
		else
		n = 0;
	} else
		n = asLength(s_length, __func__);
	arf_ptr y = (arf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arf_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_arf_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (j = 0; j < n; ++j)
			arf_zero(y + j);
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (x[j] == NA_INTEGER)
			arf_nan(y + j);
			else
			arf_set_si(y + j, x[j]);
		}
		break;
	}
	case CPLXSXP:
		s_x = Rf_coerceVector(s_x, REALSXP);
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < n; ++j)
			arf_set_d(y + j, x[j]);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = (slong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_set_si(y + j, x[j]);
			break;
		}
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = (ulong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_set_ui(y + j, x[j]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_set_fmpz(y + j, x + j);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
			int prec = asPrec(R_NilValue, __func__);
			for (j = 0; j < n; ++j)
				arf_fmpz_div_fmpz(y + j, fmpq_numref(x + j), fmpq_denref(x + j), prec, ARF_RND_NEAR);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_set(y + j, x + j);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_set_mag(y + j, x + j);
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
	return object;
}

SEXP R_flint_arf_vector(SEXP from, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(s_rnd, 0, __func__);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arf_srcptr x = (arf_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (arf_is_nan(x + j))
			y[j] = R_NaN;
		else if (arf_cmp(x + j, lb) > 0 && arf_cmp(x + j, ub) < 0)
			y[j] = arf_get_d(x + j, rnd);
		else {
			y[j] = (arf_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_arf_format(SEXP from, SEXP s_base,
                        SEXP s_digits, SEXP s_sep, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	mpfr_rnd_t rnd = (mpfr_rnd_t) asRnd(s_rnd, 1, __func__);
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	arf_srcptr x = (arf_ptr) R_flint_get_pointer(from);
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
	SEXP s_neg_inf = Rf_mkChar(buffer);
	memcpy(bufnan, " Inf", 4);
	SEXP s_pos_inf = Rf_mkChar(buffer);
	memcpy(bufnan, " NaN", 4);
	SEXP s_nan     = Rf_mkChar(buffer);
	memset(buffer, '0', ncmax);
	if (ncsgn)
		buffer[0] = ' ';
	if (ncrad)
		bufman[0] = '.';
	memcpy(bufsep, sep, ncsep);
	bufexp[-1] = '+';
	SEXP s_zero    = Rf_mkChar(buffer);

	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		if (!mpfr_regular_p(f))
			SET_STRING_ELT(to, (R_xlen_t) j,
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
			SET_STRING_ELT(to, (R_xlen_t) j, Rf_mkChar(buffer));
		}
	}

	} else {

	SEXP
		s_neg_inf = Rf_mkChar(              "-Inf"        ),
		s_pos_inf = Rf_mkChar((flags & 4) ? " Inf" : "Inf"),
		s_nan     = Rf_mkChar((flags & 4) ? " NaN" : "NaN");
	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		SET_STRING_ELT(to, (R_xlen_t) j,
		               (mpfr_nan_p(f)) ? s_nan :
		               (mpfr_sgn(f) < 0) ? s_neg_inf : s_pos_inf);
	}

	}

	mpz_clear(z);
	mpfr_clear(f);
	MPFR_ERANGE_RESET;
	UNPROTECT(1);
	return to;
}

SEXP R_flint_arf_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	arf_srcptr
		x = (arf_ptr) R_flint_get_pointer(s_x),
		y = (arf_ptr) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long long int j, n = RECYCLE2(nx, ny);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	{
		SEXP ans = newObject("arf");
		arf_ptr z = (arf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arf_t)) : 0);
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
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] =
				((!arf_is_nan(x + j % nx) && !arf_is_zero(x + j % nx)) ||
				 (!arf_is_nan(y + j % ny) && !arf_is_zero(y + j % ny)))
				? 1
				:
				(arf_is_nan(x + j % nx) || arf_is_nan(y + j % ny))
				? NA_LOGICAL
				: 0;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
}

SEXP R_flint_arf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
	slong prec = (slong) asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = (arf_rnd_t) asRnd(R_NilValue, 0, __func__);
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  3: /*     "abs" */
	case  4: /*    "sign" */
	case  5: /*    "sqrt" */
	case  6: /*   "floor" */
	case  7: /* "ceiling" */
	case  8: /*   "trunc" */
	case  9: /*  "cummin" */
	case 10: /*  "cummax" */
	case 11: /*  "cumsum" */
	case 12: /* "cumprod" */
	case 38: /*   "round" */
	case 39: /*  "signif" */
	case 47: /*    "Conj" */
	case 48: /*      "Re" */
	case 49: /*      "Im" */
	case 50: /*     "Mod" */
	case 51: /*     "Arg" */
	{
		SEXP ans = newObject("arf");
		arf_ptr z = (arf_ptr) ((n) ? flint_calloc((size_t) n, sizeof(arf_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case  1: /*       "+" */
		case 47: /*    "Conj" */
		case 48: /*      "Re" */
			for (j = 0; j < n; ++j)
				arf_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				arf_neg(z + j, x + j);
			break;
		case  3: /*     "abs" */
		case 50: /*     "Mod" */
			for (j = 0; j < n; ++j)
				arf_abs(z + j, x + j);
			break;
		case  4: /*    "sign" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
				arf_nan(z + j);
				else
				arf_set_si(z + j, arf_sgn(x + j));
			break;
		case  5: /*    "sqrt" */
			for (j = 0; j < n; ++j)
				arf_sqrt(z + j, x + j, prec, rnd);
			break;
		case  6: /*   "floor" */
			for (j = 0; j < n; ++j)
				arf_floor(z + j, x + j);
			break;
		case  7: /* "ceiling" */
			for (j = 0; j < n; ++j)
				arf_ceil(z + j, x + j);
			break;
		case  8: /*   "trunc" */
			for (j = 0; j < n; ++j)
				if (arf_sgn(x + j) >= 0)
				arf_floor(z + j, x + j);
				else
				arf_ceil(z + j, x + j);
			break;
		case  9: /*  "cummin" */
			if (n) {
			arf_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(x + j); ++j)
				arf_min(z + j, last, x + j);
			for (; j < n; ++j)
				arf_nan(z + j);
			}
			break;
		case 10: /*  "cummax" */
			if (n) {
			arf_srcptr last = x;
			for (j = 0; j < n && !arf_is_nan(x + j); ++j)
				arf_max(z + j, last, x + j);
			for (; j < n; ++j)
				arf_nan(z + j);
			}
			break;
		case 11: /*  "cumsum" */
			if (n) {
			arf_set(z, x);
			for (j = 1; j < n; ++j)
				arf_add(z + j, z + j - 1, x + j, prec, rnd);
			}
			break;
		case 12: /* "cumprod" */
			if (n)
			arf_set(z, x);
			for (j = 1; j < n; ++j)
				arf_mul(z + j, z + j - 1, x + j, prec, rnd);
			break;
		case 38: /*   "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				prec = asPrec(R_NilValue, __func__);
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
				arf_mul_fmpz(s, x + j, p, ARF_PREC_EXACT, ARF_RND_NEAR);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(z + j, q, p, prec, ARF_RND_NEAR);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
				if (!arf_is_finite(x + j))
				arf_set(z + j, x + j);
				else {
				arf_div_fmpz(s, x + j, p, prec, ARF_RND_NEAR);
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
		case 39: /*  "signif" */
		{
			slong fmpq_clog_ui(const fmpq_t, ulong);
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				prec = asPrec(R_NilValue, __func__),
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
				arf_fmpz_div_fmpz(z + j, q, p, prec, ARF_RND_NEAR);
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
		case 49: /*      "Im" */
			for (j = 0; j < n; ++j)
				arf_zero(z + j);
			break;
		case 51: /*     "Arg" */
		{
			arb_t pi;
			arb_init(pi);
			arb_const_pi(pi, asPrec(R_NilValue, __func__));
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
		}
		return ans;
	}
	case 40: /*     "min" */
	case 41: /*     "max" */
	case 42: /*   "range" */
	case 43: /*     "sum" */
	case 44: /*    "prod" */
	{
		SEXP ans = newObject("arf");
		size_t s = (op == 42) ? 2 : 1;
		arf_ptr z = (arf_ptr) flint_calloc(s, sizeof(arf_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_arf_finalize);
		int narm = LOGICAL_RO(s_dots)[0];
		switch (op) {
		case 40: /*     "min" */
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
		case 41: /*     "max" */
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
		case 42: /*   "range" */
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
		case 43: /*     "sum" */
			arf_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(x + j)))
				arf_add(z, z, x + j, prec, rnd);
			break;
		case 44: /*    "prod" */
			arf_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && arf_is_nan(x + j)))
				arf_mul(z, z, x + j, prec, rnd);
			break;
		}
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
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (!arf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 46: /*     "all" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(x + j))
					anyna = 1;
				else if (arf_is_zero(x + j))
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
}
