#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include "flint.h"

void R_flint_fmpz_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
	fmpz *p = (fmpz *) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		fmpz_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int j, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	fmpz *y = (fmpz *) ((n) ? flint_calloc((size_t) n, sizeof(fmpz)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		/* nothing to do */
		break;
	case RAWSXP:
	case LGLSXP:
		s_x = Rf_coerceVector(s_x, INTSXP);
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		int tmp;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			if (tmp == NA_INTEGER)
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_si(y + j, tmp);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		double tmp;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_d(y + j, (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_fmpz_nfmpz(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	const fmpz *x = (fmpz *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, lb) > 0 && fmpz_cmp(x + j, ub) < 0)
			y[j] = (int) fmpz_get_si(x + j);
		else {
			y[j] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_fmpz_vector(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const fmpz *x = (fmpz *) R_flint_get_pointer(from);
	double *y = REAL(to);
	int w = 1;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, lb) > 0 && fmpz_cmp(x + j, ub) < 0)
			y[j] = fmpz_get_d(x + j);
		else {
			y[j] = (fmpz_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}

static R_INLINE mpz_ptr as_mpz_ptr(fmpz x, mpz_ptr work)
{
	if (COEFF_IS_MPZ(x))
		return COEFF_TO_PTR(x);
	else {
		mpz_set_si(work, x);
		return work;
	}
}

#define AMIN2(a, b) ((fmpz_cmpabs(a, b) <= 0) ? a : b)
#define AMAX2(a, b) ((fmpz_cmpabs(a, b) >= 0) ? a : b)

SEXP R_flint_fmpz_format(SEXP from, SEXP s_base)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	const fmpz *x = (fmpz *) R_flint_get_pointer(from);
	fmpz xmin = 0, xmax = 0;
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, &xmax) > 0)
			xmax = x[j];
		else if (fmpz_cmp(x + j, &xmin) < 0)
			xmin = x[j];
	}
	size_t ns, nc, ncmax;
	mpz_ptr z;
	mpz_t work;
	mpz_init(work);
	z = as_mpz_ptr(AMAX2(&xmin, &xmax)[0], work);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	z = as_mpz_ptr(AMIN2(&xmin, &xmax)[0], work);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		z = as_mpz_ptr(x[j], work);
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
	mpz_clear(work);
	UNPROTECT(1);
	return to;
}
