#include <flint/fmpq.h>
#include "R_flint.h"

void R_flint_fmpq_finalize(SEXP object)
{
	unsigned long long int i, n = R_flint_get_length(object);
	fmpq *x = (fmpq *) R_flint_get_x(object);
	for (i = 0; i < n; ++i)
		fmpq_clear(x + i);
	flint_free(x);
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
		if (TYPEOF(s_x) == INTSXP) {
			s_num = s_x;
			np = n;
		}
	} else
		n = asLength(s_length, __func__);
	R_flint_set_length(object, n);
	fmpq *y = (fmpq *) flint_calloc(n, sizeof(fmpq));
	R_flint_set_x(object, y, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (s_num != R_NilValue || s_den != R_NilValue) {
		switch (TYPEOF(s_num)) {
		case NILSXP:
			/* numerators are already initialized to zero */
			break;
		case INTSXP:
		{
			int *xp = INTEGER(s_num), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xp[i % np];
				if (tmp == NA_INTEGER)
				Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
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
				Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
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
		case INTSXP:
		{
			int *xq = INTEGER(s_den), tmp;
			for (i = 0; i < n; ++i) {
				tmp = xq[i % nq];
				if (tmp == NA_INTEGER)
				Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
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
				Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpz");
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
			Rf_error("NaN, -Inf, Inf not representable by '%s'", "fmpq");
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
		Rf_error("zero denominator not valid in canonical '%s'", "fmpq");
		else
		fmpq_canonicalise(y + i);
	}
	return object;
}

SEXP R_flint_fmpq_nflint(SEXP from)
{
	unsigned long long int i, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(newObject("nfmpq")),
		num = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n)),
		den = PROTECT(newBasic("nfmpz", INTSXP, (R_xlen_t) n));
	R_do_slot_assign(to, R_flint_symbol_num, num);
	R_do_slot_assign(to, R_flint_symbol_den, den);
	fmpq *x = (fmpq *) R_flint_get_x(from);
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
		Rf_error("'%s' length exceeds R maximum (%lld)",
		         "fmpq", (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	fmpq *x = (fmpq *) R_flint_get_x(from);
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
