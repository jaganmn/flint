#include "flint.h"

#ifndef HAVE_FMPQ_CLOG_UI
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

void R_flint_fmpq_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	fmpq *p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		fmpq_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_fmpq_initialize(SEXP object, SEXP s_length, SEXP s_x,
                             SEXP s_num, SEXP s_den)
{
	mp_limb_t j, np = 1, nq = 1, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "num", "den");
		if (s_num != R_NilValue)
			np = R_flint_get_length(s_num);
		if (s_den != R_NilValue)
			nq = R_flint_get_length(s_den);
		if (s_length == R_NilValue)
			ny = RECYCLE2(np, nq);
		else {
			ny = asLength(s_length, __func__);
			if (ny > 0 && (np == 0 || nq == 0))
				Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
				         (np == 0) ? "num" : "den");
		}
	}
	else if (s_x != R_NilValue) {
		checkType(s_x, R_flint_sexptypes, __func__);
		if (TYPEOF(s_x) != OBJSXP)
			nx = (mp_limb_t) XLENGTH(s_x);
		else {
			class = R_flint_get_class(s_x);
			if (class == R_FLINT_CLASS_INVALID)
				Rf_error(_("foreign external pointer"));
			nx = R_flint_get_length(s_x);
		}
		if (s_length == R_NilValue)
			ny = nx;
		else {
			ny = asLength(s_length, __func__);
			if (ny > 0 && nx == 0)
				Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
				         "x");
		}
	}
	else if (s_length != R_NilValue)
		ny = asLength(s_length, __func__);
	else
		ny = 0;
	fmpq *y = (ny) ? flint_calloc(ny, sizeof(fmpq)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_num != R_NilValue) {
			const fmpz *xp = R_flint_get_pointer(s_num);
			if (s_den != R_NilValue) {
				const fmpz *xq = R_flint_get_pointer(s_den);
				for (j = 0; j < ny; ++j) {
					if (fmpz_is_zero(xq + j % nq))
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
					else {
					fmpz_set(fmpq_numref(y + j), xp + j % np);
					fmpz_set(fmpq_denref(y + j), xq + j % nq);
					fmpq_canonicalise(y + j);
					}
				}
			} else {
				for (j = 0; j < ny; ++j) {
					fmpz_set(fmpq_numref(y + j), xp + j % np);
					fmpz_one(fmpq_denref(y + j));
				}
			}
		} else {
			if (s_den != R_NilValue) {
				const fmpz *xq = R_flint_get_pointer(s_den);
				for (j = 0; j < ny; ++j) {
					if (fmpz_is_zero(xq + j % nq))
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
					else
					fmpz_one(fmpq_denref(y + j));
				}
			}
		}
	} else {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (j = 0; j < ny; ++j)
				fmpz_one(fmpq_denref(y + j));
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (j = 0; j < ny; ++j) {
				fmpz_set_ui(fmpq_numref(y + j), x[j % nx]);
				fmpz_one(fmpq_denref(y + j));
			}
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j % nx] == NA_LOGICAL)
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
				else {
				fmpz_set_si(fmpq_numref(y + j), x[j % nx]);
				fmpz_one(fmpq_denref(y + j));
				}
			}
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (j = 0; j < ny; ++j) {
				if (x[j % nx] == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
				else {
				fmpz_set_si(fmpq_numref(y + j), x[j % nx]);
				fmpz_one(fmpq_denref(y + j));
				}
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			int e;
			for (j = 0; j < ny; ++j) {
				if (!R_FINITE(x[j % nx]))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
				else {
				fmpz_set_d(fmpq_numref(y + j), ldexp(frexp(x[j % nx], &e), DBL_MANT_DIG));
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
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			int e;
			for (j = 0; j < ny; ++j) {
				if (!R_FINITE(x[j % nx].r))
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
				else {
				fmpz_set_d(fmpq_numref(y + j), ldexp(frexp(x[j % nx].r, &e), DBL_MANT_DIG));
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
		case STRSXP:
		{
			mpq_t r;
			mpq_init(r);
			const char *s;
			for (j = 0; j < ny; ++j) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (j % nx)));
				if (mpq_set_str(r, s, 0) != 0) {
					mpq_clear(r);
					Rf_error(_("invalid input in string conversion"));
				}
				fmpq_set_mpq(y + j, r);
				fmpq_canonicalise(y + j);
			}
			mpq_clear(r);
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					fmpz_set_ui(fmpq_numref(y + j), x[j % nx]);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					fmpz_set_si(fmpq_numref(y + j), x[j % nx]);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					fmpz_set(fmpq_numref(y + j), x + j % nx);
					fmpz_one(fmpq_denref(y + j));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j)
					fmpq_set(y + j, x + j % nx);
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					if (mag_is_inf(x + j % nx))
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
					else
					mag_get_fmpq(y + j, x + j % nx);
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					if (!arf_is_finite(x + j % nx))
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
					else
					arf_get_fmpq(y + j, x + j % nx);
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (j = 0; j < ny; ++j) {
					if (!arf_is_finite(acf_realref(x + j % nx)))
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
					else
					arf_get_fmpq(y + j, acf_realref(x + j % nx));
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
		if (s_x != R_NilValue && ny > 0 && ny <= R_XLEN_T_MAX) {
		SEXP sx = Rf_getAttrib(s_x, R_NamesSymbol);
		if (sx != R_NilValue && XLENGTH(sx) > 0) {
		PROTECT(sx);
		if (nx == ny)
		R_do_slot_assign(object, R_flint_symbol_names, sx);
		else {
		SEXP sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
		for (j = 0; j < ny; ++j)
			SET_STRING_ELT(sy, (R_xlen_t) j,
			               STRING_ELT(sx, (R_xlen_t) (j % nx)));
		R_do_slot_assign(object, R_flint_symbol_names, sy);
		}
		UNPROTECT(1);
		}
		}
	}
	return object;
}

SEXP R_flint_fmpq_part(SEXP object, SEXP s_op)
{
	mp_limb_t j, n = R_flint_get_length(object);
	const fmpq *x = R_flint_get_pointer(object);
	int op = INTEGER_RO(s_op)[0];
	SEXP ans = PROTECT(newObject("fmpz"));
	fmpz *y = (n) ? flint_calloc(n, sizeof(fmpz)) : 0;
	R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	if (op == 0)
	for (j = 0; j < n; ++j)
		fmpz_set(y + j, fmpq_numref(x + j));
	else
	for (j = 0; j < n; ++j)
		fmpz_set(y + j, fmpq_denref(x + j));
	SEXP nms = R_do_slot(object, R_flint_symbol_names);
	if (XLENGTH(nms) > 0) {
		PROTECT(nms);
		R_do_slot_assign(ans, R_flint_symbol_names, nms);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_fmpq_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const fmpq *x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (fmpq_cmp_fmpz(x + j, lb) >= 0 && fmpq_cmp_fmpz(x + j, ub) <= 0)
			y[j] = fmpq_get_d(x + j);
		else {
			y[j] = (fmpq_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_fmpq_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y);
	const fmpq
		*x = R_flint_get_pointer(s_x),
		*y = R_flint_get_pointer(s_y);
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	mp_limb_t j, n = RECYCLE2(nx, ny);
#define COMMON \
	do { \
	SEXP nms; \
	if ((nx == n && XLENGTH(nms = R_do_slot(s_x, R_flint_symbol_names)) > 0) || \
	    (ny == n && XLENGTH(nms = R_do_slot(s_y, R_flint_symbol_names)) > 0)) { \
		PROTECT(nms); \
		R_do_slot_assign(ans, R_flint_symbol_names, nms); \
		UNPROTECT(1); \
	} \
	} while (0)
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  4: /*  "%%" */
	case  5: /* "%/%" */
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (n) ? flint_calloc(n, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
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
		case 4: /*  "%%" */
			for (j = 0; j < n; ++j) {
				if (fmpz_is_zero(fmpq_numref(y + j % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpq_div(z + j, x + j % nx, y + j % ny);
				fmpz_fdiv_r(fmpq_numref(z + j), fmpq_numref(z + j), fmpq_denref(z + j));
				fmpq_mul(z + j, z + j, y + j % ny);
				}
			}
			break;
		case 5: /* "%/%" */
			for (j = 0; j < n; ++j) {
				if (fmpz_is_zero(fmpq_numref(y + j % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpq_div(z + j, x + j % nx, y + j % ny);
				fmpz_fdiv_q(fmpq_numref(z + j), fmpq_numref(z + j), fmpq_denref(z + j));
				fmpz_one(fmpq_denref(z + j));
				}
			}
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
				    (fmpq_sgn(b) <  0 && fmpz_is_even(fmpq_denref(e)))) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "fmpq", "^", "fmpq", "fmpq");
				}
				if (!fmpz_abs_fits_ui(fmpq_numref(e))) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: exponent numerator exceeds maximum %llu in absolute value"),
				         "fmpq", "^", "fmpq", (unsigned long long int) UWORD_MAX);
				}
				if (!fmpz_fits_si(fmpq_denref(e))) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: exponent denominator exceeds maximum %lld"),
				         "fmpq", "^", "fmpq", (long long int) WORD_MAX);
				}
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
				if (!exact) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
				         "fmpq", "^", "fmpq", "fmpq");
				}
			}
			fmpz_clear(a);
			fmpz_clear(p);
			break;
		}
		}
		COMMON;
		UNPROTECT(1);
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
		ERROR_TOO_LONG(n, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) n));
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
#undef COMMON
}

SEXP R_flint_fmpq_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	const fmpq *x = R_flint_get_pointer(s_x);
#define COMMON \
	do { \
	SEXP nms = R_do_slot(s_x, R_flint_symbol_names); \
	if (XLENGTH(nms) > 0) { \
		PROTECT(nms); \
		R_do_slot_assign(ans, R_flint_symbol_names, nms); \
		UNPROTECT(1); \
	} \
	} while (0)
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  8: /*    "Conj" */
	case  9: /*      "Re" */
	case 10: /*      "Im" */
	case 11: /*     "Mod" */
	case 13: /*     "abs" */
	case 14: /*    "sign" */
	case 15: /*    "sqrt" */
	case 16: /*   "floor" */
	case 17: /* "ceiling" */
	case 18: /*   "trunc" */
	case 19: /*  "cummin" */
	case 20: /*  "cummax" */
	case 21: /*  "cumsum" */
	case 22: /* "cumprod" */
	case 48: /*   "round" */
	case 49: /*  "signif" */
	{
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (n) ? flint_calloc(n, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
			for (j = 0; j < n; ++j)
				fmpq_set(z + j, x + j);
			break;
		case  2: /*       "-" */
			for (j = 0; j < n; ++j)
				fmpq_neg(z + j, x + j);
			break;
		case 10: /*      "Im" */
			for (j = 0; j < n; ++j)
				fmpz_one(fmpq_denref(z + j));
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (j = 0; j < n; ++j)
				fmpq_abs(z + j, x + j);
			break;
		case 14: /*    "sign" */
			for (j = 0; j < n; ++j) {
				fmpz_set_si(fmpq_numref(z + j), fmpq_sgn(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case 15: /*    "sqrt" */
		{
			fmpz_t r;
			fmpz_init(r);
			for (j = 0; j < n; ++j) {
				if (fmpq_sgn(x + j) >= 0) {
				fmpz_sqrtrem(fmpq_numref(z + j), r, fmpq_numref(x + j));
				if (fmpz_is_zero(r))
				fmpz_sqrtrem(fmpq_denref(z + j), r, fmpq_denref(x + j));
				}
				if (!(fmpq_sgn(x + j) >= 0 && fmpz_is_zero(r))) {
				fmpz_clear(r);
				Rf_error(_("%s(<%s>): value is not in the range of '%s'"),
				         "sqrt", "fmpq", "fmpq");
				}
			}
			fmpz_clear(r);
			break;
		}
		case 16: /*   "floor" */
			for (j = 0; j < n; ++j) {
				fmpz_fdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case 17: /* "ceiling" */
			for (j = 0; j < n; ++j) {
				fmpz_cdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case 18: /*   "trunc" */
			for (j = 0; j < n; ++j) {
				fmpz_tdiv_q(fmpq_numref(z + j), fmpq_numref(x + j), fmpq_denref(x + j));
				fmpz_one(fmpq_denref(z + j));
			}
			break;
		case 19: /*  "cummin" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) <= 0) ? z + j - 1 : x + j);
			}
			break;
		case 20: /*  "cummax" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_set(z + j, (fmpq_cmp(z + j - 1, x + j) >= 0) ? z + j - 1 : x + j);
			}
			break;
		case 21: /*  "cumsum" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_add(z + j, z + j - 1, x + j);
			}
			break;
		case 22: /* "cumprod" */
			if (n) {
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				fmpq_mul(z + j, z + j - 1, x + j);
			}
			break;
		case 48: /*   "round" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
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
		case 49: /*  "signif" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0],
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
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
	case 55: /*    "mean" */
		if (n == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		SEXP ans = PROTECT(newObject("fmpq"));
		mp_limb_t s = (op == 52) ? 2 : 1;
		fmpq *z = flint_calloc(s, sizeof(fmpq));
		R_flint_set(ans, z, s, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 50: /*     "min" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
			break;
		case 51: /*     "max" */
			fmpq_set(z, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) < 0)
					fmpq_set(z, x + j);
			break;
		case 52: /*   "range" */
			fmpq_set(z, x);
			fmpq_set(z + 1, x);
			for (j = 1; j < n; ++j)
				if (fmpq_cmp(z, x + j) > 0)
					fmpq_set(z, x + j);
				else if (fmpq_cmp(z + 1, x + j) < 0)
					fmpq_set(z + 1, x + j);
			break;
		case 53: /*     "sum" */
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpq_add(z, z, x + j);
			break;
		case 54: /*    "prod" */
			fmpq_one(z);
			for (j = 0; j < n; ++j)
				fmpq_mul(z, z, x + j);
			break;
		case 55: /*    "mean" */
		{
			fmpq_zero(z);
			for (j = 0; j < n; ++j)
				fmpq_add(z, z, x + j);
			fmpz_mul_ui(fmpq_denref(z), fmpq_denref(z), n);
			fmpq_canonicalise(z);
			break;
		}
		}
		UNPROTECT(1);
		return ans;
	}
	case 56: /*         "any" */
	case 57: /*         "all" */
	case 58: /*       "anyNA" */
	case 59: /* "is.unsorted" */
	{
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*         "any" */
			for (j = 0; j < n &&  fmpq_is_zero(x + j); ++j) ;
			z[0] = j <  n;
			break;
		case 57: /*         "all" */
			for (j = 0; j < n && !fmpq_is_zero(x + j); ++j) ;
			z[0] = j >= n;
			break;
		case 58: /*       "anyNA" */
			z[0] = 0;
			break;
		case 59: /* "is.unsorted" */
		{
			SEXP s_strict = VECTOR_ELT(s_dots, 1);
			if (XLENGTH(s_strict) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "strictly", CHAR(STRING_ELT(s_op, 0)));
			int strict = LOGICAL_RO(s_strict)[0];
			if (strict)
			for (j = 1; j < n && fmpq_cmp(x, x + 1) <  0; ++j, ++x) ;
			else
			for (j = 1; j < n && fmpq_cmp(x, x + 1) <= 0; ++j, ++x) ;
			z[0] = j <  n;
			break;
		}
		}
		UNPROTECT(1);
		return ans;
	}
	case  3: /*       "is.na" */
	case  4: /*      "is.nan" */
	case  5: /* "is.infinite" */
	case  6: /*   "is.finite" */
	case  7: /*           "!" */
	{
		ERROR_TOO_LONG(n, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) n));
		int *z = LOGICAL(ans);
		switch (op) {
		case  3: /*       "is.na" */
		case  4: /*      "is.nan" */
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] = 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] = 1;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				z[j] = fmpq_is_zero(x + j) != 0;
			break;
		}
		COMMON;
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
#undef COMMON
}
