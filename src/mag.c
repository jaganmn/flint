#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <flint/arf.h>
#include <flint/mag.h>
#include "flint.h"

void R_flint_mag_finalize(SEXP x)
{
	unsigned long long int j, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	mag_ptr p = (mag_ptr) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		mag_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int j, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	mag_ptr y = (mag_ptr) ((n) ? flint_calloc(n, sizeof(mag_t)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (j = 0; j < n; ++j)
			mag_zero(y + j);
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		int *x = INTEGER(s_x), tmp;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			if (tmp == NA_INTEGER)
			Rf_error(_("NaN not representable by '%s'"), "mag");
			else
			mag_set_ui(y + j, (ulong) ((tmp < 0) ? -tmp : tmp));
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			if (ISNAN(tmp))
			Rf_error(_("NaN not representable by '%s'"), "mag");
			else
			mag_set_d(y + j, tmp);
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_mag_nmag(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "mag", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nmag", REALSXP, (R_xlen_t) n));
	mag_ptr x = (mag_ptr) R_flint_get_pointer(from);
	double *y = REAL(to);
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
	return to;
}

SEXP R_flint_mag_vector(SEXP from)
{
	SEXP to = R_flint_mag_nmag(from);
	CLEAR_ATTRIB(to);
	return to;
}

SEXP R_flint_mag_format(SEXP from, SEXP s_base,
                        SEXP s_digits, SEXP s_sep, SEXP s_rnd)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "mag", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	mpfr_rnd_t rnd = (mpfr_rnd_t) asRnd(s_rnd, 1, __func__);
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	mag_ptr x = (mag_ptr) R_flint_get_pointer(from);
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
			SET_STRING_ELT(to, (R_xlen_t) j,
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
			SET_STRING_ELT(to, (R_xlen_t) j, Rf_mkChar(buffer));
		}
	}

	} else {

	SEXP s_pos_inf = Rf_mkChar("Inf");
	for (j = 0; j < n; ++j)
		SET_STRING_ELT(to, (R_xlen_t) j, s_pos_inf);

	}

	mpz_clear(z);
	mpfr_clear(f);
	arf_clear(tmp);
	MPFR_ERANGE_RESET;
	UNPROTECT(1);
	return to;
}
