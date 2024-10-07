#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpq.h>
#include "flint.h"

void R_flint_fmpq_finalize(SEXP x)
{
	unsigned long long int i, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	fmpq *p = (fmpq *) R_ExternalPtrAddr(x);
	for (i = 0; i < n; ++i)
		fmpq_clear(p + i);
	flint_free(p);
	return;
}

SEXP R_flint_fmpq_initialize(SEXP object, SEXP s_length, SEXP s_x,
                             SEXP s_num, SEXP s_den)
{
	unsigned long long int i, n, np = 1, nq = 1;
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_num != R_NilValue) {
			checkType(s_num, R_flint_sexptypes + 1, __func__);
			np = (unsigned long long int) XLENGTH(s_num);
		}
		if (s_den != R_NilValue) {
			checkType(s_den, R_flint_sexptypes + 1, __func__);
			nq = (unsigned long long int) XLENGTH(s_den);
		}
		n = RECYCLE2(np, nq);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes + 1, __func__);
		n = (unsigned long long int) XLENGTH(s_x);
		if (TYPEOF(s_x) != REALSXP) {
			s_num = s_x;
			np = n;
		}
	} else
		n = asLength(s_length, __func__);
	fmpq *y = (fmpq *) ((n) ? flint_calloc(n, sizeof(fmpq)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_slong_finalize);
	if (s_num != R_NilValue || s_den != R_NilValue) {
		switch (TYPEOF(s_num)) {
		case NILSXP:
			break;
		case RAWSXP:
		case LGLSXP:
			s_num = Rf_coerceVector(s_num, INTSXP);
		case INTSXP:
		{
			int *xp = INTEGER(s_num), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xp[i % np];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_si(fmpq_numref(y + i), tmp);
			}
			break;
		}
		case REALSXP:
		{
			double *xp = REAL(s_num), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xp[i % np];
				if (!R_FINITE(tmp))
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_d(fmpq_numref(y + i), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_den)) {
		case NILSXP:
			for (i = 0; i < n; ++i)
				fmpz_one(fmpq_denref(y + i));
			break;
		case RAWSXP:
		case LGLSXP:
			s_den = Rf_coerceVector(s_den, INTSXP);
		case INTSXP:
		{
			int *xq = INTEGER(s_den), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xq[i % nq];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_si(fmpq_denref(y + i), tmp);
			}
			break;
		}
		case REALSXP:
		{
			double *xq = REAL(s_den), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xq[i % nq];
				if (!R_FINITE(tmp))
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_d(fmpq_denref(y + i), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
			}
			break;
		}
		}
	} else if (s_x != R_NilValue) {
		double *x = REAL(s_x), tmp;
		int e;
		for (i = 0; i < n; ++i) {
			tmp = x[i];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpq");
			else {
			fmpz_set_d(fmpq_numref(y + i), ldexp(frexp(tmp, &e), DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
			if (e < 0)
				fmpz_one_2exp(fmpq_denref(y + i),
				              (ulong) -e);
			else {
				fmpz_mul_2exp(fmpq_numref(y + i), fmpq_numref(y + i),
				              (ulong)  e);
				fmpz_one(fmpq_denref(y + i));
			}
			}
		}
	} else
		for (i = 0; i < n; ++i)
			fmpq_zero(y + i);
	for (i = 0; i < n; ++i) {
		if (fmpz_is_zero(fmpq_denref(y + i)))
		Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
		else
		fmpq_canonicalise(y + i);
	}
	return object;
}

SEXP R_flint_fmpq_nfmpq(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newObject("nfmpq")),
		num = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n)),
		den = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	R_do_slot_assign(to, R_flint_symbol_num, num);
	R_do_slot_assign(to, R_flint_symbol_den, den);
	fmpq *x = (fmpq *) R_flint_get_pointer(from);
	int *yp = INTEGER(num), *yq = INTEGER(den);
	fmpz_t lb, ub;
	fmpz *p, *q;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		p = fmpq_numref(x + i);
		if (fmpz_cmp(p, lb) > 0 && fmpz_cmp(p, ub) < 0)
			yp[i] = (int) fmpz_get_si(p);
		else {
			yp[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
		q = fmpq_denref(x + i);
		if (fmpz_cmp(q, lb) > 0 && fmpz_cmp(q, ub) < 0)
			yq[i] = (int) fmpz_get_si(q);
		else {
			yq[i] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(3);
	return to;
}

SEXP R_flint_fmpq_vector(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	fmpq *x = (fmpq *) R_flint_get_pointer(from);
	double *y = REAL(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	int w = 1;
	for (i = 0; i < n; ++i) {
		if (fmpq_cmp_fmpz(x + i, lb) > 0 && fmpq_cmp_fmpz(x + i, ub) < 0)
			y[i] = fmpq_get_d(x + i);
		else {
			y[i] = (fmpq_sgn(x + i) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_fmpq_num(SEXP from)
{
	THISPART(num, fmpq, fmpz, fmpq *, fmpz *, fmpz);
}

SEXP R_flint_fmpq_den(SEXP from)
{
	THISPART(den, fmpq, fmpz, fmpq *, fmpz *, fmpz);
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

SEXP R_flint_fmpq_format(SEXP from, SEXP s_base)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpq", (long long int) R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base, k;
	SEXP to = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	fmpq *x = (fmpq *) R_flint_get_pointer(from);
	fmpz xmin[2] = {0, 0}, xmax[2] = {0, 0}, *tmp[2];
	for (i = 0; i < n; ++i) {
		tmp[0] = fmpq_numref(x + i);
		tmp[1] = fmpq_denref(x + i);
		for (k = 0; k < 2; ++k) {
			if (fmpz_cmp(tmp[k], &xmax[k]) > 0)
				xmax[k] = tmp[k][0];
			else if (fmpz_cmp(tmp[k], &xmin[k]) < 0)
				xmin[k] = tmp[k][0];
		}
	}
	size_t ns, nc, ncmax[2];
	mpz_ptr z;
	mpz_t work;
	mpz_init(work);
	for (k = 0; k < 2; ++k) {
		z = as_mpz_ptr(AMAX2(&xmin[k], &xmax[k])[0], work);
		ncmax[k] = mpz_sizeinbase(z, abase);
	}
	char *buffer = R_alloc(ncmax[0] + ncmax[1] + 6, 1);
	for (k = 0; k < 2; ++k) {
		z = as_mpz_ptr(AMAX2(&xmin[k], &xmax[k])[0], work);
		mpz_get_str(buffer, base, z);
		ncmax[k] = strlen(buffer);
		z = as_mpz_ptr(AMIN2(&xmin[k], &xmax[k])[0], work);
		mpz_get_str(buffer, base, z);
		if (buffer[ncmax[k]] != '\0')
			ncmax[k] = strlen(buffer);
	}
	buffer[ncmax[0] + ncmax[1] + 3] = '\0';
	buffer[0] = '(';
	char *bufoff[2];
	bufoff[0] = buffer + (1);
	bufoff[1] = buffer + (1 + ncmax[0] + 1);
	char delim[2];
	delim[0] = '/';
	delim[1] = ')';
	for (i = 0; i < n; ++i) {
		tmp[0] = fmpq_numref(x + i);
		tmp[1] = fmpq_denref(x + i);
		for (k = 0; k < 2; ++k) {
			z = as_mpz_ptr(tmp[k][0], work);
			nc = mpz_sizeinbase(z, abase) + (mpz_sgn(z) < 0);
			if (nc > ncmax[k])
				nc = ncmax[k];
			ns = ncmax[k] - nc;
			if (ns > 0 && bufoff[k][ns - 1] != ' ')
				memset(bufoff[k], ' ', ns);
			mpz_get_str(bufoff[k] + ns, base, z);
			if (bufoff[k][ncmax[k] - 1] == '\0') {
				memmove(bufoff[k] + ns + 1, bufoff[k] + ns, nc);
				bufoff[k][ns] = ' ';
			}
			bufoff[k][ncmax[k]] = delim[k];
		}
		SET_STRING_ELT(to, (R_xlen_t) i, Rf_mkChar(buffer));
	}
	mpz_clear(work);
	UNPROTECT(1);
	return to;
}
