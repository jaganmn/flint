#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpq.h>
#include "flint.h"

void R_flint_fmpq_finalize(SEXP x)
{
	unsigned long long int j, n;
	uucopy(&n, (unsigned int *) INTEGER(R_ExternalPtrProtected(x)));
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

SEXP R_flint_fmpq_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const fmpq
		*x = (fmpq *) R_flint_get_pointer(s_x),
		*y = (fmpq *) R_flint_get_pointer(s_y);
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
		SEXP ans = newObject("fmpq");
		fmpq *z = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				fmpq_add(z + j, x + j % nx, y + j % ny);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				fmpq_sub(z + j, x + j % nx, y + j % ny);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				fmpq_mul(z + j, x + j % nx, y + j % ny);
			break;
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(fmpq_numref(y + j % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpq_div(z + j, x + j % nx, y + j % ny);
			break;
		case 7: /*   "^" */
		{
			fmpz_t exp;
			fmpz_init(exp);
			for (j = 0; j < n; ++j) {
				if (!fmpz_is_one(fmpq_denref(y + j % ny)))
				Rf_error(_("exponent is not an integer"));
				else {
				fmpz_set(exp, fmpq_numref(y + j % ny));
				if (!fmpz_abs_fits_ui(exp))
				Rf_error(_("exponent exceeds maximum %llu in absolute value"),
				         (unsigned long long int) (ulong) -1);
				else if (fmpz_sgn(exp) >= 0) {
				fmpz_pow_ui(fmpq_numref(z + j), fmpq_numref(x + j % nx), fmpz_get_ui(exp));
				fmpz_pow_ui(fmpq_denref(z + j), fmpq_denref(x + j % nx), fmpz_get_ui(exp));
				}
				else {
				fmpz_neg(exp, exp);
				fmpz_pow_ui(fmpq_numref(z + j), fmpq_denref(x + j % nx), fmpz_get_ui(exp));
				fmpz_pow_ui(fmpq_denref(z + j), fmpq_numref(x + j % nx), fmpz_get_ui(exp));
				fmpq_canonicalise(z + j);
				}
				}
			}
			fmpz_clear(exp);
			break;
		}
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
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
		if (n > R_XLEN_T_MAX)
			Rf_error(_("value length would exceed maximum %lld"),
			         (long long int) R_XLEN_T_MAX);
		SEXP ans = Rf_allocVector(LGLSXP, (R_xlen_t) n);
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_cmp(x + j % nx, y + j % ny) < 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_cmp(x + j % nx, y + j % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_cmp(x + j % nx, y + j % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_cmp(x + j % nx, y + j % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpq_is_zero(x + j % nx) && !fmpq_is_zero(y + j % ny);
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpq_is_zero(x + j % nx) || !fmpq_is_zero(y + j % ny);
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
}

SEXP R_flint_fmpq_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  3: /*     "abs" */
	case  9: /*  "cummax" */
	case 10: /*  "cummin" */
	case 11: /* "cumprod" */
	case 12: /*  "cumsum" */
	case 38: /*   "round" */
	case 39: /*  "signif" */
	{
		SEXP ans = newObject("fmpq");
		fmpq *z = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
		switch (op) {
		case  1: /*       "+" */
			for (j = 0; j < n; ++j)
				fmpq_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				fmpq_neg(z + j, x + j);
			break;
		case  3: /*     "abs" */
			for (j = 0; j < n; ++j)
				fmpq_abs(z + j, x + j);
			break;
		case  9: /*  "cummax" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) < 0) ? x + j : z + j - 1);
			break;
		case 10: /*  "cummin" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) < 0) ? z + j - 1 : x + j);
			break;
		case 11: /* "cumprod" */
			if (n == 0)
				fmpq_one(z);
			else
				fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_mul(z + j, z + j - 1, x + j);
			break;
		case 12: /*  "cumsum" */
			if (n == 0)
				fmpq_zero(z);
			else
				fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_add(z + j, z + j - 1, x + j);
			break;
		case 38: /*   "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0];
			fmpz_t p, q, r;
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* a/b ~ c/10^+digits   <=>   c ~ (a * 10^+digits)/b */
			fmpz_pow_ui(p, p, (ulong) digits);
			for (j = 0; j < n; ++j) {
				fmpz_mul(fmpq_numref(z + j), fmpq_numref(x + j), p);
				fmpz_set(fmpq_denref(z + j), fmpq_denref(x + j));
				fmpz_ndiv_qr(q, r, fmpq_numref(z + j), fmpq_denref(z + j));
				if (fmpz_cmp2abs(fmpq_denref(z + j), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_set(fmpq_numref(z + j), q);
				fmpz_set(fmpq_denref(z + j), p);
				fmpq_canonicalise(z + j);
			}
			} else {
			/* a/b ~ c*10^-digits   <=>   c ~ a/(10^-digits * b) */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (j = 0; j < n; ++j) {
				fmpz_set(fmpq_numref(z + j), fmpq_numref(x + j));
				fmpz_mul(fmpq_denref(z + j), fmpq_denref(x + j), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(z + j), fmpq_denref(z + j));
				if (fmpz_cmp2abs(fmpq_denref(z + j), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(fmpq_numref(z + j), q, p);
				fmpz_one(fmpq_denref(z + j));
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			break;
		}
		case 39: /*  "signif" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				clog;
			if (digits <= 0)
				digits = 1;
			fmpz_t p, q, r;
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpq_is_zero(x + j))
				fmpq_zero(z + j);
				else {
				fmpq_abs(z + j, x + j);
				clog = fmpz_clog_ui(fmpq_numref(z + j), 10) -
					fmpz_flog_ui(fmpq_denref(z + j), 10);
				if (clog <= digits) {
				fmpz_set_si(p, 10);
				if (clog >= 0)
				fmpz_pow_ui(p, p, (ulong) (digits - clog));
				else
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1));
				fmpz_mul(fmpq_numref(z + j), fmpq_numref(x + j), p);
				fmpz_set(fmpq_denref(z + j), fmpq_denref(x + j));
				fmpz_ndiv_qr(q, r, fmpq_numref(z + j), fmpq_denref(z + j));
				if (fmpz_cmp2abs(fmpq_denref(z + j), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_set(fmpq_numref(z + j), q);
				fmpz_set(fmpq_denref(z + j), p);
				fmpq_canonicalise(z + j);
				} else {
				fmpz_set_si(p, 10);
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_set(fmpq_numref(z + j), fmpq_numref(x + j));
				fmpz_mul(fmpq_denref(z + j), fmpq_denref(x + j), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(z + j), fmpq_denref(z + j));
				if (fmpz_cmp2abs(fmpq_denref(z + j), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(fmpq_numref(z + j), q, p);
				fmpz_one(fmpq_denref(z + j));
				}
				}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			break;
		}
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		return ans;
	}
	case  4: /*    "sign" */
	case  6: /* "ceiling" */
	case  7: /*   "floor" */
	case  8: /*   "trunc" */
	{
		SEXP ans = newObject("fmpz");
		fmpz *z = (fmpz *) ((n) ? flint_calloc((size_t) n, sizeof(fmpz)) : 0);
		switch (op) {
		case  4: /*    "sign" */
			for (j = 0; j < n; ++j)
				fmpz_set_si(z + j, fmpq_sgn(x + j));
			break;
		case  6: /* "ceiling" */
			for (j = 0; j < n; ++j)
				fmpz_cdiv_q(z + j, fmpq_numref(x + j), fmpq_denref(x + j));
			break;
		case  7: /*   "floor" */
			for (j = 0; j < n; ++j)
				fmpz_fdiv_q(z + j, fmpq_numref(x + j), fmpq_denref(x + j));
			break;
		case  8: /*   "trunc" */
			for (j = 0; j < n; ++j)
				fmpz_tdiv_q(z + j, fmpq_numref(x + j), fmpq_denref(x + j));
			break;
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		return ans;
	}
	case 40: /*     "max" */
	case 41: /*     "min" */
	case 42: /*   "range" */
		if (n == 0)
			Rf_error(_("argument of length zero in '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
	case 43: /*    "prod" */
	case 44: /*     "sum" */
	{
		SEXP ans = newObject("fmpq");
		size_t s = (op == 40) ? 2 : 1;
		fmpq *z = (fmpq *) flint_calloc(s, sizeof(fmpq));
		switch (op) {
		case 40: /*     "max" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) < 0)
					fmpq_set(z, x + j);
			break;
		case 41: /*     "min" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
			break;
		case 42: /*   "range" */
			fmpq_set(z    , x);
			fmpq_set(z + 1, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z + 1, x + j) < 0)
					fmpq_set(z + 1, x + j);
				else if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
			break;
		case 43: /*    "prod" */
			fmpq_one(z);
			for (j = 0; j < n; ++j)
				fmpq_mul(z, z, x + j);
			break;
		case 44: /*     "sum" */
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpq_add(z, z, x + j);
			break;
		}
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		return ans;
	}
	case 45: /*     "any" */
	case 46: /*     "all" */
	{
		SEXP ans = Rf_allocVector(LGLSXP, 1);
		int *z = LOGICAL(ans);
		switch (op) {
		case 45: /*     "any" */
			for (j = 0; j < n &&  fmpq_is_zero(x + j); ++j) ;
			z[0] = j <  n;
			break;
		case 46: /*     "all" */
			for (j = 0; j < n && !fmpq_is_zero(x + j); ++j) ;
			z[0] = j >= n;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
}
