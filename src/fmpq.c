#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpq.h>
#include "flint.h"

void R_flint_fmpq_finalize(SEXP x)
{
	unsigned long long int j, n;
	ucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)), 1);
	fmpq *p = (fmpq *) R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		fmpq_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_fmpq_initialize(SEXP object, SEXP s_length, SEXP s_x,
                             SEXP s_num, SEXP s_den)
{
	unsigned long long int j, n, np = 1, nq = 1;
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
	fmpq *y = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
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
			const int *xp = INTEGER_RO(s_num);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xp[j % np];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_si(fmpq_numref(y + j), tmp);
			}
			break;
		}
		case REALSXP:
		{
			const double *xp = REAL_RO(s_num);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xp[j % np];
				if (!R_FINITE(tmp))
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_d(fmpq_numref(y + j), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
			}
			break;
		}
		}
		switch (TYPEOF(s_den)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				fmpz_one(fmpq_denref(y + j));
			break;
		case RAWSXP:
		case LGLSXP:
			s_den = Rf_coerceVector(s_den, INTSXP);
		case INTSXP:
		{
			const int *xq = INTEGER_RO(s_den);
			int tmp;
			for (j = 0; j < n; ++j) {
				tmp = xq[j % nq];
				if (tmp == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_si(fmpq_denref(y + j), tmp);
			}
			break;
		}
		case REALSXP:
		{
			const double *xq = REAL_RO(s_den);
			double tmp;
			for (j = 0; j < n; ++j) {
				tmp = xq[j % nq];
				if (!R_FINITE(tmp))
				Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpz");
				else
				fmpz_set_d(fmpq_denref(y + j), (fabs(tmp) < DBL_MIN) ? 0.0 : tmp);
			}
			break;
		}
		}
	} else if (s_x != R_NilValue) {
		const double *x = REAL_RO(s_x);
		double tmp;
		int e;
		for (j = 0; j < n; ++j) {
			tmp = x[j];
			if (!R_FINITE(tmp))
			Rf_error(_("NaN, -Inf, Inf not representable by '%s'"), "fmpq");
			else {
			fmpz_set_d(fmpq_numref(y + j), ldexp(frexp(tmp, &e), DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
			if (e < 0)
				fmpz_one_2exp(fmpq_denref(y + j),
				              (ulong) -e);
			else {
				fmpz_mul_2exp(fmpq_numref(y + j), fmpq_numref(y + j),
				              (ulong)  e);
				fmpz_one(fmpq_denref(y + j));
			}
			}
		}
	} else
		for (j = 0; j < n; ++j)
			fmpq_zero(y + j);
	for (j = 0; j < n; ++j) {
		if (fmpz_is_zero(fmpq_denref(y + j)))
		Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
		else
		fmpq_canonicalise(y + j);
	}
	return object;
}

SEXP R_flint_fmpq_nfmpq(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newObject("nfmpq")),
		num = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n)),
		den = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	R_do_slot_assign(to, R_flint_symbol_num, num);
	R_do_slot_assign(to, R_flint_symbol_den, den);
	const fmpq *x = (fmpq *) R_flint_get_pointer(from);
	int *yp = INTEGER(num), *yq = INTEGER(den);
	fmpz_t lb, ub;
	const fmpz *p, *q;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_ui(ub, (unsigned int) INT_MAX + 1U);
	fmpz_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = fmpq_numref(x + j);
		if (fmpz_cmp(p, lb) > 0 && fmpz_cmp(p, ub) < 0)
			yp[j] = (int) fmpz_get_si(p);
		else {
			yp[j] = NA_INTEGER;
			WARNING_OOB_INTEGER(w);
		}
		q = fmpq_denref(x + j);
		if (fmpz_cmp(q, lb) > 0 && fmpz_cmp(q, ub) < 0)
			yq[j] = (int) fmpz_get_si(q);
		else {
			yq[j] = NA_INTEGER;
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
	unsigned long long int j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("'%s' length exceeds R maximum (%lld)"),
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const fmpq *x = (fmpq *) R_flint_get_pointer(from);
	double *y = REAL(to);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (fmpq_cmp_fmpz(x + j, lb) > 0 && fmpq_cmp_fmpz(x + j, ub) < 0)
			y[j] = fmpq_get_d(x + j);
		else {
			y[j] = (fmpq_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return to;
}
