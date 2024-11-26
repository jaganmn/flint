#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
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

SEXP R_flint_fmpz_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	unsigned long long int
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const fmpz
		*x = (fmpz *) R_flint_get_pointer(s_x),
		*y = (fmpz *) R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	unsigned long long int j, n = RECYCLE2(nx, ny);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  4: /*  "%%" */
	case  5: /* "%/%" */
	{
		SEXP ans = newObject("fmpz");
		fmpz *z = (fmpz *) ((n) ? flint_calloc((size_t) n, sizeof(fmpz)) : 0);
		switch (op) {
		case 1: /*   "+" */
			for (j = 0; j < n; ++j)
				fmpz_add(z + j, x + j % nx, y + j % ny);
			break;
		case 2: /*   "-" */
			for (j = 0; j < n; ++j)
				fmpz_sub(z + j, x + j % nx, y + j % ny);
			break;
		case 3: /*   "*" */
			for (j = 0; j < n; ++j)
				fmpz_mul(z + j, x + j % nx, y + j % ny);
			break;
		case 4: /*  "%%" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_r(z + j, x + j % nx, y + j % ny);
			break;
		case 5: /* "%/%" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_q(z + j, x + j % nx, y + j % ny);
			break;
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		return ans;
	}
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = newObject("fmpq");
		fmpq *z = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
		switch (op) {
		case 6: /*   "/" */
			for (j = 0; j < n; ++j)
				if (fmpz_is_zero(y + j % ny))
				Rf_error(_("quotient with 0 undefined"));
				else {
				fmpz_set(fmpq_numref(z + j), x + j % nx);
				fmpz_set(fmpq_denref(z + j), y + j % ny);
				fmpq_canonicalise(z + j);
				}
			break;
		case 7: /*   "^" */
			for (j = 0; j < n; ++j)
				if (fmpz_sgn(y + j % ny) >= 0) {
				fmpz_pow_fmpz(fmpq_numref(z + j), x + j % nx, y + j % ny);
				fmpz_one(fmpq_denref(z + j));
				} else {
				fmpz_one(fmpq_numref(z + j));
				fmpz_neg(fmpq_denref(z + j), y + j % ny);
				fmpz_pow_fmpz(fmpq_denref(z + j), x + j % nx, fmpq_denref(z + j));
				fmpq_canonicalise(z + j);
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
			Rf_error(_("value length would exceed R maximum (%lld)"),
			         (long long int) R_XLEN_T_MAX);
		SEXP ans = Rf_allocVector(LGLSXP, (R_xlen_t) n);
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_equal(x + j % nx, y + j % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_equal(x + j % nx, y + j % ny) == 0;
			break;
		case 10: /*   "<" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) < 0;
			break;
		case 11: /*   ">" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (j = 0; j < n; ++j)
				z[j] = fmpz_cmp(x + j % nx, y + j % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpz_is_zero(x + j % nx) && !fmpz_is_zero(y + j % ny);
			break;
		case 15: /*   "|" */
			for (j = 0; j < n; ++j)
				z[j] = !fmpz_is_zero(x + j % nx) || !fmpz_is_zero(y + j % ny);
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
}

SEXP R_flint_fmpz_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  3: /*     "abs" */
	case  4: /*    "sign" */
	case  5: /*    "sqrt" */
	case  9: /*  "cummax" */
	case 10: /*  "cummin" */
	case 11: /* "cumprod" */
	case 12: /*  "cumsum" */
	case 38: /*   "round" */
	case 39: /*  "signif" */
	{
		SEXP ans = newObject("fmpz");
		fmpz *z = (fmpz *) ((n) ? flint_calloc((size_t) n, sizeof(fmpz)) : 0);
		switch (op) {
		case  1: /*       "+" */
			for (j = 0; j < n; ++j)
				fmpz_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				fmpz_neg(z + j, x + j);
			break;
		case  3: /*     "abs" */
			for (j = 0; j < n; ++j)
				fmpz_abs(z + j, x + j);
			break;
		case  4: /*    "sign" */
			for (j = 0; j < n; ++j)
				fmpz_set_si(z + j, fmpz_sgn(x + j));
			break;
		case  5: /*    "sqrt" */
			for (j = 0; j < n; ++j)
				fmpz_sqrt(z + j, x + j);
			break;
		case  9: /*  "cummax" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_set(z + j, (fmpz_cmp(z + j - 1, x + j) < 0) ? x + j : z + j - 1);
			break;
		case 10: /*  "cummin" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_set(z + j, (fmpz_cmp(z + j - 1, x + j) < 0) ? z + j - 1 : x + j);
			break;
		case 11: /* "cumprod" */
			if (n == 0)
				fmpz_one(z);
			else
				fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_mul(z + j, z + j - 1, x + j);
			break;
		case 12: /*  "cumsum" */
			if (n == 0)
				fmpz_zero(z);
			else
				fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				fmpz_add(z + j, z + j - 1, x + j);
			break;
		case 38: /*   "round" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0];
			if (digits >= 0) {
			for (j = 0; j < n; ++j)
				fmpz_set(z + j, x + j);
			} else {
			fmpz_t a, d, h, p, q, r;
			fmpz_init(a);
			fmpz_init(d);
			fmpz_init(h);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			fmpz_set_si(d, digits);
			fmpz_neg(d, d);
			fmpz_set_si(p, 10);
			fmpz_pow_fmpz(p, p, d);
			fmpz_divexact_si(h, p, 2);
			for (j = 0; j < n; ++j) {
				fmpz_ndiv_qr(q, r, x + j, p);
				fmpz_abs(a, r);
				if (fmpz_equal(a, h) && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + j, q, p);
			}
			fmpz_clear(a);
			fmpz_clear(d);
			fmpz_clear(h);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			}
			break;
		}
		case 39: /*  "signif" */
		{
			slong digits = ((slong *) R_flint_get_pointer(s_dots))[0],
				clog;
			if (digits <= 0)
				digits = 1;
			fmpz_t a, d, h, p, q, r;
			fmpz_init(a);
			fmpz_init(d);
			fmpz_init(h);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpz_is_zero(x + j))
				fmpz_zero(z + j);
				else {
				fmpz_abs(a, x + j);
				clog = fmpz_clog_ui(a, 10);
				if (clog <= digits)
				fmpz_set(z + j, x + j);
				else {
				fmpz_set_si(d, clog - digits);
				fmpz_set_si(p, 10);
				fmpz_pow_fmpz(p, p, d);
				fmpz_divexact_si(h, p, 2);
				fmpz_ndiv_qr(q, r, x + j, p);
				fmpz_abs(a, r);
				if (fmpz_equal(a, h) && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + j, q, p);
				}
				}
			}
			fmpz_clear(a);
			fmpz_clear(d);
			fmpz_clear(h);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			break;
		}
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		return ans;
	}
	case 40: /*     "max" */
	case 41: /*     "min" */
	case 42: /*   "range" */
		if (n == 0)
			Rf_error(_("argument of length zero to '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
	case 43: /*    "prod" */
	case 44: /*     "sum" */
	{
		SEXP ans = newObject("fmpz");
		size_t s = (op == 40) ? 2 : 1;
		fmpz *z = (fmpz *) flint_calloc(s, sizeof(fmpz));
		switch (op) {
		case 40: /*     "max" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z, x + j) < 0)
					fmpz_set(z, x + j);
			break;
		case 41: /*     "min" */
			fmpz_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z, x + j) > 0)
					fmpz_set(z, x + j);
			break;
		case 42: /*   "range" */
			fmpz_set(z    , x);
			fmpz_set(z + 1, x);
			for (j = 1; j < n; ++j)
				if (fmpz_cmp(z + 1, x + j) < 0)
					fmpz_set(z + 1, x + j);
				else if (fmpz_cmp(z, x + j) > 0)
					fmpz_set(z, x + j);
			break;
		case 43: /*    "prod" */
			fmpz_one(z);
			for (j = 0; j < n; ++j)
				fmpz_mul(z, z, x + j);
			break;
		case 44: /*     "sum" */
			fmpz_zero(z);
			for (j = 0; j < n; ++j)
				fmpz_add(z, z, x + j);
			break;
		}
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_fmpz_finalize);
		return ans;
	}
	case 45: /*     "any" */
	case 46: /*     "all" */
	{
		SEXP ans = Rf_allocVector(LGLSXP, 1);
		int *z = LOGICAL(ans);
		switch (op) {
		case 45: /*     "any" */
			for (j = 0; j < n &&  fmpz_is_zero(x + j); ++j) ;
			z[0] = j <  n;
			break;
		case 46: /*     "all" */
			for (j = 0; j < n && !fmpz_is_zero(x + j); ++j) ;
			z[0] = j >= n;
			break;
		}
		return ans;
	}
	default:
		Rf_error(_("operation '%s' not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
}
