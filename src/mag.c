#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include "flint.h"

void R_flint_mag_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
	mag_ptr p = (mag_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		mag_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int j, n, nx = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
			nx = (unsigned long long int) XLENGTH(s_x);
		else {
			class = R_flint_get_class(s_x);
			if (class == R_FLINT_CLASS_INVALID)
				Rf_error(_("foreign external pointer"));
			nx = R_flint_get_length(s_x);
		}
		if (s_length == R_NilValue)
			n = nx;
		else {
			n = asLength(s_length, __func__);
			if (n > 0 && nx == 0)
				Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
				         "x");
		}
	}
	else if (s_length != R_NilValue)
		n = asLength(s_length, __func__);
	else
		n = 0;
	mag_ptr y = (mag_ptr) ((n) ? flint_calloc((size_t) n, sizeof(mag_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_mag_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (j = 0; j < n; ++j)
			mag_zero(y + j);
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < n; ++j)
			mag_set_ui(y + j, x[j % nx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (x[j % nx] == NA_LOGICAL)
			Rf_error(_("NaN is not representable by '%s'"), "mag");
			else
			mag_set_ui(y + j, (ulong) x[j % nx]);
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (x[j % nx] == NA_INTEGER)
			Rf_error(_("NaN is not representable by '%s'"), "mag");
			else if (x[j % nx] >= 0)
			mag_set_ui(y + j, (ulong) x[j % nx]);
			else
			mag_set_ui(y + j, (ulong) -1 - (ulong) x[j % nx] + 1);
		}
		break;
	}
	case CPLXSXP:
		s_x = Rf_coerceVector(s_x, REALSXP);
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (ISNAN(x[j % nx]))
			Rf_error(_("NaN is not representable by '%s'"), "mag");
			else
			mag_set_d(y + j, x[j % nx]);
		}
		break;
	}
	case STRSXP:
	{
		mpfr_t r;
		arf_t tmp;
		mpfr_init2(r, MAG_BITS << 1);
		arf_init(tmp);
		const char *s;
		char *t;
		for (j = 0; j < n; ++j) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) (j % nx)));
			mpfr_strtofr(r, s, &t, 0, MPFR_RNDA);
			if (t <= s)
				break;
			s = t;
			while (isspace(*s))
				s++;
			if (*s != '\0')
				break;
			arf_set_mpfr(tmp, r);
			arf_get_mag(y + j, tmp);
		}
		mpfr_clear(r);
		arf_clear(tmp);
		if (j < n)
			Rf_error(_("invalid input in string conversion"));
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = (slong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j) {
				if (x[j % nx] >= 0)
				mag_set_ui(y + j, (ulong) x[j % nx]);
				else
				mag_set_ui(y + j, (ulong) -1 - (ulong) x[j % nx] + 1);
			}
			break;
		}
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = (ulong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				mag_set_ui(y + j, x[j % nx]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				mag_set_fmpz(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
			arf_t q;
			arf_init(q);
			for (j = 0; j < n; ++j) {
				arf_fmpz_div_fmpz(q, fmpq_numref(x + j % nx), fmpq_denref(x + j % nx), MAG_BITS << 1, ARF_RND_UP);
				arf_get_mag(y + j, q);
			}
			arf_clear(q);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				mag_set(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_get_mag(y + j, x + j % nx);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				arf_get_mag(y + j, acf_realref(x + j % nx));
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

SEXP R_flint_mag_vector(SEXP object)
{
	unsigned long long int j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	mag_srcptr x = (mag_ptr) R_flint_get_pointer(object);
	double *y = REAL(ans);
	mag_t ub;
	mag_init(ub);
	mag_set_ui_2exp_si(ub, 1U, DBL_MAX_EXP);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (mag_cmp(x + j, ub) < 0)
			y[j] = mag_get_d(x + j);
		else {
			y[j] = R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	mag_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_mag_format(SEXP object, SEXP s_base,
                        SEXP s_digits, SEXP s_sep)
{
	unsigned long long int j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	mpfr_rnd_t rnd = (mpfr_rnd_t) MPFR_RNDA;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	mag_srcptr x = (mag_ptr) R_flint_get_pointer(object);
	mpfr_exp_t e__;
	slong p__;
	mpfr_uexp_t e, emax = 0;
	mpfr_prec_t p, pmax = 0;
	char work[4];
	unsigned int flags = 0;
	mpz_t z;
	mpfr_t f;
	arf_t tmp;
	MPFR_ERANGE_SET;
	mpz_init(z);
	mpfr_init2(f, FLINT_BITS);
	arf_init(tmp);
	for (j = 0; j < n; ++j) {
		arf_set_mag(tmp, x + j);
		arf_get_mpfr(f, tmp, rnd);
		if (mpfr_regular_p(f)) {
			flags |= 1;
			mpfr_get_str(work, &e__, base, 2, f, rnd);
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			if (e > emax)
				emax = e;
			p__ = arf_bits(tmp);
			p = (p__ <= MPFR_PREC_MAX) ? (mpfr_prec_t) p__ : MPFR_PREC_MAX;
			if (p > pmax)
				pmax = p;
		}
		else if (mpfr_zero_p(f))
			flags |= 1;
	}

	if (flags & 1) {

	mpz_set_ui(z, emax);
	mpfr_set_prec(f, (pmax == 0) ? 1 : pmax);
	if (digits == 0)
		digits = (pmax == 0) ? 1 : mpfr_get_str_ndigits(abase, pmax);

	size_t ns, nc,
		ncrad = (digits > 1) ? 1 : 0,
		ncman = digits,
		ncsep = strlen(sep),
		ncexp = mpz_sizeinbase(z, abase),
		ncmax = ncrad + ncman + ncsep + 1;
	char *buffer = R_alloc(ncmax + ncexp + 1, 1);
	mpz_get_str(buffer, base, z);
	ncexp = strlen(buffer);
	ncmax += ncexp;
	char
		*bufman = buffer + (ncrad),
		*bufsep = buffer + (ncrad + ncman),
		*bufexp = buffer + (ncrad + ncman + ncsep + 1),
		*bufnan = buffer + (ncmax - 3);
	buffer[ncmax] = '\0';

	memset(buffer, ' ', ncmax - 3);
	memcpy(bufnan, "Inf", 3);
	SEXP s_pos_inf = Rf_mkChar(buffer);
	memset(buffer, '0', ncmax);
	if (ncrad)
		bufman[0] = '.';
	memcpy(bufsep, sep, ncsep);
	bufexp[-1] = '+';
	SEXP s_zero    = Rf_mkChar(buffer);

	for (j = 0; j < n; ++j) {
		arf_set_mag(tmp, x + j);
		arf_get_mpfr(f, tmp, rnd);
		if (!mpfr_regular_p(f))
			SET_STRING_ELT(ans, (R_xlen_t) j,
			               (mpfr_zero_p(f)) ? s_zero : s_pos_inf);
		else {
			/* Mantissa */
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

	} else {

	SEXP s_pos_inf = Rf_mkChar("Inf");
	for (j = 0; j < n; ++j)
		SET_STRING_ELT(ans, (R_xlen_t) j, s_pos_inf);

	}

	mpz_clear(z);
	mpfr_clear(f);
	arf_clear(tmp);
	MPFR_ERANGE_RESET;
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_mag_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	mag_srcptr
		x = (mag_ptr) R_flint_get_pointer(s_x),
		y = (mag_ptr) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long long int j, n = RECYCLE2(nx, ny);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = newObject("mag");
		mag_ptr z = (mag_ptr) ((n) ? flint_calloc((size_t) n, sizeof(mag_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_mag_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				mag_add(z + j, x + j % nx, y + j % ny);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				if (mag_cmp(x + j % nx, y + j % ny) >= 0)
				mag_sub(z + j, x + j % nx, y + j % ny);
				else
				mag_sub(z + j, y + j % ny, x + j % nx);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				mag_mul(z + j, x + j % nx, y + j % ny);
			break;
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				mag_div(z + j, x + j % nx, y + j % ny);
			break;
		case 7: /*   "^" */
		{
			mag_srcptr b, e;
			mag_t a;
			mag_init(a);
			for (j = 0; j < n; ++j) {
				b = x + j % nx;
				e = y + j % ny;
				if (mag_is_zero(e) || mag_cmp_2exp_si(b, 0) == 0)
					/* b^0, 1^e = 1 */
					mag_one(z + j);
				else if (mag_cmp_2exp_si(e, 0) == 0)
					/* b^1 = b */
					mag_set(z + j, b);
				else if (mag_is_inf(e) || mag_is_special(b)) {
					/* b^Inf, 0^e, Inf^e = 0|Inf */
					if (mag_cmp_2exp_si(b, 0) < 0)
						mag_zero(z + j);
					else
						mag_inf(z + j);
				}
				else if (mag_cmp_2exp_si(b, 0) > 0) {
					/* b^e = exp(e * log(b)) */
					mag_log(a, b);
					mag_mul(a, e, a);
					mag_exp(z + j, a);
				}
				else {
					/* b^e = exp(-(e * -log(b))) */
					mag_neg_log_lower(a, b);
					mag_mul_lower(a, e, a);
					mag_expinv(z + j, a);
				}
			}
			mag_clear(a);
			break;
		}
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
				z[j] = mag_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] = mag_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] = mag_cmp(x + j % nx, y + j % ny) < 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] = mag_cmp(x + j % nx, y + j % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] = mag_cmp(x + j % nx, y + j % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] = mag_cmp(x + j % nx, y + j % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] = !mag_is_zero(x + j % nx) && !mag_is_zero(y + j % ny);
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] = !mag_is_zero(x + j % nx) || !mag_is_zero(y + j % ny);
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "mag");
		return R_NilValue;
	}
}

SEXP R_flint_mag_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
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
	case 23: /*     "log" */
	case 24: /*   "log10" */
	case 25: /*    "log2" */
	case 26: /*   "log1p" */
	case 27: /*     "exp" */
	case 28: /*   "expm1" */
	case 48: /*   "round" */
	case 49: /*  "signif" */
	{
		SEXP ans = newObject("mag");
		mag_ptr z = (mag_ptr) ((n) ? flint_calloc((size_t) n, sizeof(mag_t)) : 0);
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_mag_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  2: /*       "-" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (j = 0; j < n; ++j)
				mag_set(z + j, x + j);
			break;
		case 10: /*      "Im" */
			for (j = 0; j < n; ++j)
				mag_zero(z + j);
			break;
		case 12: /*     "Arg" */
			for (j = 0; j < n; ++j)
				if (mag_is_zero(x + j))
					mag_zero(z + j);
				else
					mag_const_pi(z + j);
			break;
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j)
				if (mag_is_zero(x + j))
					mag_zero(z + j);
				else
					mag_one(z + j);
			break;
		case 15: /*    "sqrt" */
			for (j = 0; j < n; ++j)
				mag_sqrt(z + j, x + j);
			break;
		case 16: /*   "floor" */
		case 18: /*   "trunc" */
		{
			fmpz_t r;
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (mag_is_inf(x + j))
					mag_inf(z + j);
				if (mag_is_zero(x + j))
					mag_zero(z + j);
				else if (fmpz_cmp_si(MAG_EXPREF(x + j),        0) <= 0)
					mag_zero(z + j);
				else if (fmpz_cmp_si(MAG_EXPREF(x + j), MAG_BITS) >= 0)
					mag_set(z + j, x + j);
				else {
					mag_get_fmpz_lower(r, x + j);
					mag_set_fmpz(z + j, r);
				}
			}
			fmpz_clear(r);
			break;
		}
		case 17: /* "ceiling" */
		{
			fmpz_t r;
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (mag_is_inf(x + j))
					mag_inf(z + j);
				if (mag_is_zero(x + j))
					mag_zero(z + j);
				else if (fmpz_cmp_si(MAG_EXPREF(x + j),        0) <= 0)
					mag_one(z + j);
				else if (fmpz_cmp_si(MAG_EXPREF(x + j), MAG_BITS) >= 0)
					mag_set(z + j, x + j);
				else {
					mag_get_fmpz(r, x + j);
					mag_set_fmpz(z + j, r);
				}
			}
			fmpz_clear(r);
			break;
		}
		case 19: /*  "cummin" */
			if (n) {
			mag_set(z, x);
			for (j = 1; j < n; ++j)
				mag_min(z + j, z + j - 1, x + j);
			}
			break;
		case 20: /*  "cummax" */
			if (n) {
			mag_set(z, x);
			for (j = 1; j < n; ++j)
				mag_max(z + j, z + j - 1, x + j);
			}
			break;
		case 21: /*  "cumsum" */
			if (n) {
			mag_set(z, x);
			for (j = 1; j < n; ++j)
				mag_add(z + j, z + j - 1, x + j);
			}
			break;
		case 22: /* "cumprod" */
			if (n) {
			mag_set(z, x);
			for (j = 1; j < n; ++j)
				mag_mul(z + j, z + j - 1, x + j);
			}
			break;
		case 23: /*     "log" */
		case 24: /*   "log10" */
		case 25: /*    "log2" */
			for (j = 0; j < n; ++j)
				if (mag_cmp_2exp_si(x + j, 0) >= 0)
					mag_log(z + j, x + j);
				else
					mag_neg_log(z + j, x + j);
			if (op != 13 || s_dots != R_NilValue) {
			mag_t tmp;
			mag_init(tmp);
			if (op != 13)
				mag_set_ui(tmp, (op == 14) ? 10 : 2);
			else {
				arf_srcptr base = (arf_ptr) R_flint_get_pointer(s_dots);
				if (arf_is_nan(base) || arf_sgn(base) < 0) {
					mag_clear(tmp);
					Rf_error(_("NaN not representable by '%s'"), "mag");
				}
				arf_get_mag_lower(tmp, base);
			}
			if (mag_cmp_2exp_si(tmp, 0) >= 0)
				mag_log_lower(tmp, tmp);
			else
				mag_neg_log_lower(tmp, tmp);
			if (mag_is_special(tmp)) {
				for (j = 0; j < n; ++j)
					if (mag_is_inf(z + j)) {
					mag_clear(tmp);
					Rf_error(_("NaN not representable by '%s'"), "mag");
					}
					else
					mag_zero(z + j);
			}
			else if (mag_cmp_2exp_si(tmp, 0) == 0) {
				for (j = 0; j < n; ++j)
					if (mag_is_zero(z + j)) {
					mag_clear(tmp);
					Rf_error(_("NaN not representable by '%s'"), "mag");
					}
					else
					mag_inf(z + j);
			}
			else {
				for (j = 0; j < n; ++j)
					mag_div(z + j, z + j, tmp);
			}
			mag_clear(tmp);
			}
			break;
		case 26: /*   "log1p" */
			for (j = 0; j < n; ++j)
				mag_log1p(z + j, x + j);
			break;
		case 27: /*     "exp" */
			for (j = 0; j < n; ++j)
				mag_exp(z + j, x + j);
			break;
		case 28: /*   "expm1" */
			for (j = 0; j < n; ++j)
				mag_expm1(z + j, x + j);
			break;
		case 48: /*   "round" */
		{
			if (R_flint_get_length(s_dots) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0];
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
				if (mag_is_special(x + j))
				mag_set(z + j, x + j);
				else {
				arf_set_mag(s, x + j);
				arf_mul_fmpz(s, s, p, ARF_PREC_EXACT, ARF_RND_UP);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(s, q, p, MAG_BITS << 1, ARF_RND_UP);
				arf_get_mag(z + j, s);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
				if (mag_is_special(x + j))
				mag_set(z + j, x + j);
				else {
				arf_set_mag(s, x + j);
				arf_div_fmpz(s, s, p, MAG_BITS << 1, ARF_RND_UP);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				mag_set_fmpz(z + j, q);
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
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				clog;
			if (digits <= 0)
				digits = 1;
			fmpq_t a;
			fmpz_t p, q, r;
			arf_t s;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			arf_init(s);
			for (j = 0; j < n; ++j) {
				if (mag_is_special(x + j))
				mag_set(z + j, x + j);
				else {
				mag_get_fmpq(a, x + j);
				clog = fmpq_clog_ui(a, 10);
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
				arf_fmpz_div_fmpz(s, q, p, MAG_BITS << 1, ARF_RND_UP);
				arf_get_mag(z + j, s);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				mag_set_fmpz(z + j, q);
				}
				}
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			arf_clear(s);
			break;
		}
		}
		return ans;
	}
	case 55: /*    "mean" */
		if (n == 0)
			Rf_error(_("argument of length zero in '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		SEXP ans = newObject("mag");
		size_t s = (op == 52) ? 2 : 1;
		mag_ptr z = (mag_ptr) flint_calloc(s, sizeof(mag_t));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_mag_finalize);
		switch (op) {
		case 50: /*     "min" */
			mag_inf(z);
			for (j = 0; j < n; ++j)
				if (mag_cmp(z, x + j) > 0)
					mag_set(z, x + j);
			break;
		case 51: /*     "max" */
			mag_zero(z);
			for (j = 0; j < n; ++j)
				if (mag_cmp(z, x + j) < 0)
					mag_set(z, x + j);
			break;
		case 52: /*   "range" */
			mag_inf(z);
			mag_zero(z + 1);
			for (j = 0; j < n; ++j)
				if (mag_cmp(z, x + j) > 0)
					mag_set(z, x + j);
				else if (mag_cmp(z + 1, x + j) < 0)
					mag_set(z + 1, x + j);
			break;
		case 53: /*     "sum" */
			mag_zero(z);
			for (j = 0; j < n; ++j)
				mag_add(z, z, x + j);
			break;
		case 54: /*    "prod" */
			mag_one(z);
			for (j = 0; j < n; ++j)
				mag_mul(z, z, x + j);
			break;
		case 55: /*    "mean" */
		{
			mag_zero(z);
			for (j = 0; j < n; ++j)
				mag_add(z, z, x + j);
			fmpz_t p;
			fmpz_init(p);
			unsigned int uu[2];
			ucopy(uu, &n);
			fmpz_set_uiui(p, uu[1], uu[0]);
			mag_div_fmpz(z, z, p);
			fmpz_clear(p);
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
		switch (op) {
		case 56: /*     "any" */
			for (j = 0; j < n &&  mag_is_zero(x + j); ++j) ;
			z[0] = j <  n;
			break;
		case 57: /*     "all" */
			for (j = 0; j < n && !mag_is_zero(x + j); ++j) ;
			z[0] = j >= n;
			break;
		case 58: /*   "anyNA" */
			z[0] = 0;
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
				z[j] = 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] = mag_is_inf(x + j) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] = mag_is_finite(x + j) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				z[j] = mag_is_zero(x + j) != 0;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "mag");
		return R_NilValue;
	}
}
