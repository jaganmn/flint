#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include "flint.h"
#include "acf.h"

void R_flint_slong_finalize(SEXP x)
{
	slong *p = (slong *) R_ExternalPtrAddr(x);
	flint_free(p);
	return;
}

SEXP R_flint_slong_initialize(SEXP object, SEXP s_length, SEXP s_x)
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
	slong *y = (slong *) ((n) ? flint_malloc(n * sizeof(slong)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (j = 0; j < n; ++j)
			y[j] = (slong) x[j % nx];
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (j = 0; j < n; ++j) {
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
		for (j = 0; j < n; ++j) {
			if (x[j % nx] == NA_INTEGER)
			Rf_error(_("NaN is not representable by '%s'"), "slong");
			else
			y[j] = x[j % nx];
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
			Rf_error(_("NaN is not representable by '%s'"), "slong");
#if FLINT64
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
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = (slong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j)
				y[j] = x[j % nx];
			break;
		}
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = (ulong *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j) {
				if (x[j % nx] > ((ulong) -1) >> 1)
				Rf_error(_("integer not in range of '%s'"), "slong");
				else
				y[j] = (slong) x[j % nx];
			}
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
			for (j = 0; j < n; ++j) {
				if (!fmpz_fits_si(x + j % nx))
				Rf_error(_("integer not in range of '%s'"), "slong");
				else
				y[j] = fmpz_get_si(x + j % nx);
			}
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < n; ++j) {
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
			mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < n; ++j) {
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
			arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < n; ++j) {
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
			acf_srcptr x = (acf_ptr) R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (j = 0; j < n; ++j) {
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
	return object;
}

SEXP R_flint_slong_vector(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const slong *x = (slong *) R_flint_get_pointer(from);
	double *y = REAL(to);
#if FLINT64
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
	return to;
}

SEXP R_flint_slong_format(SEXP from, SEXP s_base)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	const slong *x = (slong *) R_flint_get_pointer(from);
	slong xmin = 0, xmax = 0;
	for (j = 0; j < n; ++j) {
		if (x[j] > xmax)
			xmax = x[j];
		else if (x[j] < xmin)
			xmin = x[j];
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init(z);
	mpz_set_si(z, (xmin < -xmax) ? xmin : xmax);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	mpz_set_si(z, (xmin < -xmax) ? xmax : xmin);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		mpz_set_si(z, x[j]);
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
		SET_STRING_ELT(to, (R_xlen_t) j, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	UNPROTECT(1);
	return to;
}
