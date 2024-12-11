#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/mag.h>
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
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_num != R_NilValue)
			np = R_flint_get_length(s_num);
		if (s_den != R_NilValue)
			nq = R_flint_get_length(s_den);
		n = RECYCLE2(np, nq);
	} else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
		n = (unsigned long long int) XLENGTH(s_x);
		else if ((class = R_flint_get_class(s_x)) != R_FLINT_CLASS_INVALID)
		n = R_flint_get_length(s_x);
		else
		n = 0;
	} else
		n = asLength(s_length, __func__);
	fmpq *y = (fmpq *) ((n) ? flint_calloc((size_t) n, sizeof(fmpq)) : 0);
	R_flint_set(object, y, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_num != R_NilValue) {
			const fmpz *xp = (fmpz *) R_flint_get_pointer(s_num);
			if (s_den != R_NilValue) {
				const fmpz *xq = (fmpz *) R_flint_get_pointer(s_den);
				for (j = 0; j < n; ++j) {
					if (fmpz_is_zero(xq + j % nq))
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
					else {
					fmpz_set(fmpq_numref(y + j), xp + j % np);
					fmpz_set(fmpq_denref(y + j), xq + j % nq);
					fmpq_canonicalise(y + j);
					}
				}
			} else {
				for (j = 0; j < n; ++j) {
					fmpz_set(fmpq_numref(y + j), xp + j);
					fmpz_one(fmpq_denref(y + j));
				}
			}
		} else {
			if (s_den != R_NilValue) {
				const fmpz *xq = (fmpz *) R_flint_get_pointer(s_den);
				for (j = 0; j < n; ++j) {
					if (fmpz_is_zero(xq + j))
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
					else
					fmpz_one(fmpq_denref(y + j));
				}
			}
		}
	} else {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < n; ++j)
				fmpz_one(fmpq_denref(y + j));
			break;
		case RAWSXP:
		case LGLSXP:
			s_x = Rf_coerceVector(s_x, INTSXP);
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < n; ++j) {
				if (x[j] == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpz");
				else {
				fmpz_set_si(fmpq_numref(y + j), x[j]);
				fmpz_one(fmpq_denref(y + j));
				}
			}
			break;
		}
		case CPLXSXP:
			s_x = Rf_coerceVector(s_x, REALSXP);
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			int e;
			for (j = 0; j < n; ++j) {
				if (!R_FINITE(x[j]))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
				else {
				fmpz_set_d(fmpq_numref(y + j), ldexp(frexp(x[j], &e), DBL_MANT_DIG));
				e -= DBL_MANT_DIG;
				if (e < 0) {
				fmpz_one_2exp(fmpq_denref(y + j),
				              (ulong) -e); /* fear not as e > INT_MIN */
				fmpq_canonicalise(y + j);
				} else {
				fmpz_mul_2exp(fmpq_numref(y + j), fmpq_numref(y + j),
				              (ulong)  e);
				fmpz_one(fmpq_denref(y + j));
				}
				}
			}
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = (slong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					fmpz_set_si(fmpq_numref(y + j), x[j]);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = (ulong *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					fmpz_set_ui(fmpq_numref(y + j), x[j]);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = (fmpz *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					fmpz_set(fmpq_numref(y + j), x + j);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j)
					fmpq_set(y + j, x + j);
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = (arf_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					if (!arf_is_finite(x + j))
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
					else
					arf_get_fmpq(y + j, x + j);
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = (mag_ptr) R_flint_get_pointer(s_x);
				for (j = 0; j < n; ++j) {
					if (mag_is_inf(x + j))
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
					else
					mag_get_fmpq(y + j, x + j);
				}
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
	}
	return object;
}

SEXP R_flint_fmpq_nfmpq(SEXP from)
{
	unsigned long long int j, n = R_flint_get_length(from);
	ERROR_TOO_LONG(n);
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
	ERROR_TOO_LONG(n);
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
			const fmpq *b, *e;
			ulong u;
			slong s;
			fmpz_t a, p;
			int exact;
			fmpz_init(a);
			fmpz_init(p);
			for (j = 0; j < n; ++j) {
				b = x + j % nx;
				e = y + j % ny;
				if ((fmpq_sgn(b) == 0 && fmpq_sgn(e) < 0) ||
				    (fmpq_sgn(b) <  0 && fmpz_is_even(fmpq_denref(e))))
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "fmpq", "^", "fmpq", "fmpq");
				if (!fmpz_abs_fits_ui(fmpq_numref(e)))
				Rf_error(_("<%s> %s <%s>: exponent numerator exceeds maximum %llu in absolute value"),
				         "fmpq", "^", "fmpq", (unsigned long long int) (ulong) -1);
				if (!fmpz_fits_si(fmpq_denref(e)))
				Rf_error(_("<%s> %s <%s>: exponent denominator exceeds maximum %llu"),
				         "fmpq", "^", "fmpq", (unsigned long long int) ((ulong) -1 >> 1));
				s = fmpz_get_si(fmpq_denref(e));
				if (fmpz_sgn(fmpq_numref(e)) >= 0) {
				u = fmpz_get_ui(fmpq_numref(e));
				fmpz_pow_ui(p, fmpq_numref(b), u);
				exact = fmpz_root(fmpq_numref(z + j), p, s);
				if (exact) {
				fmpz_pow_ui(p, fmpq_denref(b), u);
				exact = fmpz_root(fmpq_denref(z + j), p, s);
				}
				} else {
				fmpz_neg(a, fmpq_numref(e));
				u = fmpz_get_ui(fmpq_numref(e));
				fmpz_pow_ui(p, fmpq_denref(b), u);
				exact = fmpz_root(fmpq_numref(z + j), p, s);
				if (exact) {
				fmpz_pow_ui(p, fmpq_numref(b), u);
				exact = fmpz_root(fmpq_denref(z + j), p, s);
				if (exact)
				fmpq_canonicalise(z + j);
				}
				}
				if (!exact)
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "fmpq", "^", "fmpq", "fmpq");
			}
			fmpz_clear(a);
			fmpz_clear(p);
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
		ERROR_TOO_LONG(n);
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

#ifndef HAVE_FMPQ_CLOG_UI
/* TODO: use configure to conditionally define HAVE_FMPQ_CLOG_UI */
slong fmpq_clog_ui(const fmpq_t x, ulong b)
{
	slong clog = fmpz_clog_ui(fmpq_numref(x), b) -
		fmpz_flog_ui(fmpq_denref(x), b);
	/* 'clog' can be off by 1: test if b^(clog-1) < x  */
	clog -= 1;
	fmpz_t p;
	fmpz_init(p);
	fmpz_set_si(p, 10);
	if (clog >= 0) {
		fmpz_pow_ui(p, p, (ulong) clog);
		if (fmpq_cmp_fmpz(x, p) > 0)
			clog += 1;
	} else {
		fmpq_t y;
		fmpq_init(y);
		fmpq_inv(y, x);
		fmpz_pow_ui(p, p, (ulong) -1 - (ulong) clog + 1);
		if (fmpq_cmp_fmpz(y, p) < 0)
			clog += 1;
		fmpq_clear(y);
	}
	fmpz_clear(p);
	return clog;
}
#endif

SEXP R_flint_fmpq_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	unsigned long long int j, n = R_flint_get_length(s_x);
	const fmpq *x = (fmpq *) R_flint_get_pointer(s_x);
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  3: /*     "abs" */
	case  4: /*    "sign" */
	case  6: /*   "floor" */
	case  7: /* "ceiling" */
	case  8: /*   "trunc" */
	case  9: /*  "cummin" */
	case 10: /*  "cummax" */
	case 11: /*  "cumsum" */
	case 12: /* "cumprod" */
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
		case  4: /*    "sign" */
			for (j = 0; j < n; ++j) {
				fmpz_set_si(fmpq_numref(z + j), fmpq_sgn(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case  6: /*   "floor" */
			for (j = 0; j < n; ++j) {
				fmpz_fdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case  7: /* "ceiling" */
			for (j = 0; j < n; ++j) {
				fmpz_cdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case  8: /*   "trunc" */
			for (j = 0; j < n; ++j) {
				fmpz_tdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case  9: /*  "cummin" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) <= 0) ? z + j - 1 : x + j);
			}
			break;
		case 10: /*  "cummax" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) >= 0) ? z + j - 1 : x + j);
			}
			break;
		case 11: /*  "cumsum" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_add(z + j, z + j - 1, x + j);
			}
			break;
		case 12: /* "cumprod" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_mul(z + j, z + j - 1, x + j);
			}
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
			/* a/b ~ c/10^+digits   <=>   c ~ (a*10^+digits)/b */
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
			/* a/b ~ c*10^-digits   <=>   c ~ a/(b*10^-digits) */
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
			fmpq_t a;
			fmpz_t p, q, r;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpq_is_zero(x + j))
				fmpq_zero(z + j);
				else {
				fmpq_abs(a, x + j);
				clog = fmpq_clog_ui(a, 10);
				if (fmpq_sgn(x + j) < 0)
					fmpq_neg(a, a);
				fmpz_set_si(p, 10);
				if (clog <= digits) {
				if (clog >= 0)
				fmpz_pow_ui(p, p, (ulong) (digits - clog));
				else
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1));
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_set(fmpq_numref(z + j), q);
				fmpz_set(fmpq_denref(z + j), p);
				fmpq_canonicalise(z + j);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(fmpq_numref(z + j), q, p);
				fmpz_one(fmpq_denref(z + j));
				}
				}
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			break;
		}
		}
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		return ans;
	}
	case 40: /*     "min" */
	case 41: /*     "max" */
	case 42: /*   "range" */
		if (n == 0)
			Rf_error(_("argument of length zero in '%s'"),
			         CHAR(STRING_ELT(s_op, 0)));
	case 43: /*     "sum" */
	case 44: /*    "prod" */
	{
		SEXP ans = newObject("fmpq");
		size_t s = (op == 42) ? 2 : 1;
		fmpq *z = (fmpq *) flint_calloc(s, sizeof(fmpq));
		switch (op) {
		case 40: /*     "min" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
			break;
		case 41: /*     "max" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) < 0)
					fmpq_set(z, x + j);
			break;
		case 42: /*   "range" */
			fmpq_set(z, x);
			fmpq_set(z + 1, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
				else if (fmpq_cmp(z + 1, x + j) < 0)
					fmpq_set(z + 1, x + j);
			break;
		case 43: /*     "sum" */
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpq_add(z, z, x + j);
			break;
		case 44: /*    "prod" */
			fmpq_one(z);
			for (j = 0; j < n; ++j)
				fmpq_mul(z, z, x + j);
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
