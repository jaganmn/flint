#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include "flint.h"

void R_flint_fmpz_finalize(SEXP x)
{
	unsigned long long int i, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	fmpz *p = (fmpz *) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		fmpz_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP s_length, SEXP s_x)
{
	unsigned long long int i, n;
	if (s_x == R_NilValue)
		n = asLength(s_length, __func__);
	else {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
	}
	fmpz *y = (fmpz *) ((n) ? flint_calloc(n, sizeof(fmpz)) : 0);
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
		int *x = INTEGER(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (tmp == NA_INTEGER)
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_si(y + i, tmp);
		}
		break;
	}
	case REALSXP:
	{
		double *x = REAL(s_x), tmp;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
			else
			fmpz_set_d(y + i, (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
		}
		break;
	}
	}
	return object;
}

SEXP R_flint_fmpz_nfmpz(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) R_flint_get_pointer(from);
	int *y = INTEGER(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = (int) fmpz_get_si(x + i);
		else {
			y[i] = NA_INTEGER;
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
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) R_flint_get_pointer(from);
	double *y = REAL(to);
	int w = 1;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, lb) > 0 && fmpz_cmp(x + i, ub) < 0)
			y[i] = fmpz_get_d(x + i);
		else {
			y[i] = (fmpz_sgn(x + i) < 0) ? R_NegInf : R_PosInf;
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

/* NB: mpz_sizeinbase() can be 1 too big for bases not equal to power of 2 */
SEXP R_flint_fmpz_format(SEXP from, SEXP s_base)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpz", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	fmpz *x = (fmpz *) R_flint_get_pointer(from), xmin = 0, xmax = 0;
	for (i = 0; i < n; ++i) {
		if (fmpz_cmp(x + i, &xmax) > 0)
			xmax = x[i];
		else if (fmpz_cmp(x + i, &xmin) < 0)
			xmin = x[i];
	}
	size_t ns, nc, ncmax;
	mpz_ptr z;
	mpz_t work;
	mpz_init(work);
	z = as_mpz_ptr(AMAX2(&xmin, &xmax)[0], work);
	char *buffer = R_alloc(mpz_sizeinbase(z, abase) + 2, 1);
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	z = as_mpz_ptr(AMIN2(&xmin, &xmax)[0], work);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (i = 0; i < n; ++i) {
		z = as_mpz_ptr(x[i], work);
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
		SET_STRING_ELT(to, (R_xlen_t) i, Rf_mkChar(buffer));
	}
	mpz_clear(work);
	UNPROTECT(1);
	return to;
}
