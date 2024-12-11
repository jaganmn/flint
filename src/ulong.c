#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/mag.h>
#include "flint.h"

void R_flint_ulong_finalize(SEXP x)
{
	ulong *p = (ulong *) R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_ulong_initialize(SEXP object, SEXP s_length, SEXP s_x)
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
	ulong *y = (ulong *) ((n) ? flint_calloc((size_t) n, sizeof(ulong)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_ulong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (x[j] == NA_INTEGER)
			Rf_error(_("NaN is not representable by '%s'"), "ulong");
			else if (x[j] < 0)
			Rf_error(_("integer not in range of '%s'"), "ulong");
			else
			y[j] = (ulong) x[j];
		}
		break;
	}
	case CPLXSXP:
		s_x = Rf_coerceVector(s_x, REALSXP);
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (j = 0; j < n; ++j) {
			if (ISNAN(x[j]))
			Rf_error(_("NaN is not representable by '%s'"), "ulong");
#if FLINT64
			else if (x[j] <= -1.0 || x[j] >= 0x1.0p+64)
#else
			else if (x[j] <= -1.0 || x[j] >= 0x1.0p+32)
#endif
			Rf_error(_("floating-point number not in range of '%s'"), "ulong");
			else
			y[j] = (ulong) x[j];
		}
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = (slong *) R_flint_get_pointer(s_x);
			slong tmp;
			for (j = 0; j < n; ++j) {
				if (x[j] < 0)
				Rf_error(_("integer not in range of '%s'"), "ulong");
				else
				y[j] = (ulong) x[j];
			}
			break;
		}
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = (ulong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				y[j] = x[j];
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j) {
				if (fmpz_sgn(x + j) < 0 || !fmpz_abs_fits_ui(x + j))
				Rf_error(_("integer not in range of '%s'"), "ulong");
				else
				y[j] = fmpz_get_ui(x + j);
			}
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < n; ++j) {
				fmpz_tdiv_q(q, fmpq_numref(x + j), fmpq_denref(x + j));
				if (fmpz_sgn(q) < 0 || !fmpz_abs_fits_ui(q))
				Rf_error(_("rational not in range of '%s'"), "ulong");
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
			for (j = 0; j < n; ++j) {
				arf_get_fmpz(q, x + j, ARF_RND_DOWN);
				if (fmpz_sgn(q) < 0 || !fmpz_abs_fits_ui(q))
				Rf_error(_("floating point number not in range of '%s'"), "ulong");
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
			for (j = 0; j < n; ++j) {
				mag_get_fmpz_lower(q, x + j);
				if (!fmpz_abs_fits_ui(q))
				Rf_error(_("floating point number not in range of '%s'"), "ulong");
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
	return object;
}

SEXP R_flint_ulong_nulong(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	SEXP to = PROTECT(newBasic("nulong", INTSXP, (R_xlen_t) n));
	const ulong *x = (ulong *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (x[j] <= INT_MAX)
			y[j] = (int) x[j];
		else {
			y[j] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	UNPROTECT(1);
	return to;
}

SEXP R_flint_ulong_vector(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const ulong *x = (ulong *) R_flint_get_pointer(from);
	double *y = REAL(to);
#if FLINT64
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
	return to;
}

SEXP R_flint_ulong_format(SEXP from, SEXP s_base)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	const ulong *x = (ulong *) R_flint_get_pointer(from);
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
		SET_STRING_ELT(to, (R_xlen_t) j, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	UNPROTECT(1);
	return to;
}
