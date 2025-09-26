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
	fmpq *p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		fmpq_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_fmpq_initialize(SEXP object, SEXP s_x, SEXP s_length,
                             SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                             SEXP s_num, SEXP s_den)
{
	mp_limb_t jy, nx = 0, ny = 0, np = 1, nq = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	PROTECT(s_dim = validDim(s_dim));
	PROTECT(s_dimnames = validDimNames(s_dimnames, s_dim));
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "num", "den");
		if (s_num != R_NilValue)
			np = R_flint_get_length(s_num);
		if (s_den != R_NilValue)
			nq = R_flint_get_length(s_den);
		ny = validLength(s_length, s_dim, RECYCLE2(np, nq));
		if (ny > 0 && (np == 0 || nq == 0))
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         (np == 0) ? "num" : "den");
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
		ny = validLength(s_length, s_dim, nx);
		if (ny > 0 && nx == 0)
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         "x");
	}
	else
		ny = validLength(s_length, s_dim, nx);
	PROTECT(s_names = validNames(s_names, ny));
	fmpq *y = (ny) ? flint_calloc(ny, sizeof(fmpq)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_fmpq_finalize);
	if (s_num != R_NilValue || s_den != R_NilValue) {
		if (s_num != R_NilValue) {
			const fmpz *xp = R_flint_get_pointer(s_num);
			if (s_den != R_NilValue) {
				const fmpz *xq = R_flint_get_pointer(s_den);
				for (jy = 0; jy < ny; ++jy) {
					if (fmpz_is_zero(xq + jy % nq))
					Rf_error(_("zero denominator not valid in canonical \"%s\""), "fmpq");
					else {
					fmpz_set(fmpq_numref(y + jy), xp + jy % np);
					fmpz_set(fmpq_denref(y + jy), xq + jy % nq);
					fmpq_canonicalise(y + jy);
					}
				}
			} else {
				for (jy = 0; jy < ny; ++jy) {
					fmpz_set(fmpq_numref(y + jy), xp + jy % np);
					fmpz_one(fmpq_denref(y + jy));
				}
			}
		} else {
			if (s_den != R_NilValue) {
				const fmpz *xq = R_flint_get_pointer(s_den);
				for (jy = 0; jy < ny; ++jy) {
					if (fmpz_is_zero(xq + jy % nq))
					Rf_error(_("zero denominator not valid in canonical \"%s\""), "fmpq");
					else
					fmpz_one(fmpq_denref(y + jy));
				}
			}
		}
	} else {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (jy = 0; jy < ny; ++jy)
				fmpz_one(fmpq_denref(y + jy));
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				fmpz_set_ui(fmpq_numref(y + jy), x[jy % nx]);
				fmpz_one(fmpq_denref(y + jy));
			}
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (x[jy % nx] == NA_LOGICAL)
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
				else {
				fmpz_set_si(fmpq_numref(y + jy), x[jy % nx]);
				fmpz_one(fmpq_denref(y + jy));
				}
			}
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (x[jy % nx] == NA_INTEGER)
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
				else {
				fmpz_set_si(fmpq_numref(y + jy), x[jy % nx]);
				fmpz_one(fmpq_denref(y + jy));
				}
			}
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			int e;
			for (jy = 0; jy < ny; ++jy) {
				if (!R_FINITE(x[jy % nx]))
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
				else {
				fmpz_set_d(fmpq_numref(y + jy), ldexp(frexp(x[jy % nx], &e), DBL_MANT_DIG));
				e -= DBL_MANT_DIG;
				if (e < 0) {
				fmpz_one_2exp(fmpq_denref(y + jy),
				              (ulong) -e); /* fear not as e > INT_MIN */
				fmpq_canonicalise(y + jy);
				} else {
				fmpz_mul_2exp(fmpq_numref(y + jy), fmpq_numref(y + jy),
				              (ulong)  e);
				fmpz_one(fmpq_denref(y + jy));
				}
				}
			}
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			int e;
			for (jy = 0; jy < ny; ++jy) {
				if (!R_FINITE(x[jy % nx].r))
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
				else {
				fmpz_set_d(fmpq_numref(y + jy), ldexp(frexp(x[jy % nx].r, &e), DBL_MANT_DIG));
				e -= DBL_MANT_DIG;
				if (e < 0) {
				fmpz_one_2exp(fmpq_denref(y + jy),
				              (ulong) -e); /* fear not as e > INT_MIN */
				fmpq_canonicalise(y + jy);
				} else {
				fmpz_mul_2exp(fmpq_numref(y + jy), fmpq_numref(y + jy),
				              (ulong)  e);
				fmpz_one(fmpq_denref(y + jy));
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
			for (jy = 0; jy < ny; ++jy) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (jy % nx)));
				if (mpq_set_str(r, s, 0) != 0) {
					mpq_clear(r);
					Rf_error(_("invalid input in string conversion"));
				}
				fmpq_set_mpq(y + jy, r);
				fmpq_canonicalise(y + jy);
			}
			mpq_clear(r);
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					fmpz_set_ui(fmpq_numref(y + jy), x[jy % nx]);
					fmpz_one(fmpq_denref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					fmpz_set_si(fmpq_numref(y + jy), x[jy % nx]);
					fmpz_one(fmpq_denref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					fmpz_set(fmpq_numref(y + jy), x + jy % nx);
					fmpz_one(fmpq_denref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					fmpq_set(y + jy, x + jy % nx);
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					if (mag_is_inf(x + jy % nx))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
					else
					mag_get_fmpq(y + jy, x + jy % nx);
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					if (!arf_is_finite(x + jy % nx))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
					else
					arf_get_fmpq(y + jy, x + jy % nx);
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					if (!arf_is_finite(acf_realref(x + jy % nx)))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpq");
					else
					arf_get_fmpq(y + jy, acf_realref(x + jy % nx));
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
	setDDNN(object, s_dim, s_dimnames, s_names);
	UNPROTECT(3);
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
	setDDNN1(ans, object);
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

SEXP R_flint_fmpq_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	const fmpq
		*x = R_flint_get_pointer(s_x),
		*y = R_flint_get_pointer(s_y);
	int dz[3];
	int mop = checkConformable(s_x, s_y, nx, ny, matrixop(op), dz);
	if (mop >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
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
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_add(z + jz, x + jz % nx, y + jz % ny);
			break;
		case 2: /*   "-" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_sub(z + jz, x + jz % nx, y + jz % ny);
			break;
		case 3: /*   "*" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_mul(z + jz, x + jz % nx, y + jz % ny);
			break;
		case 4: /*  "%%" */
			for (jz = 0; jz < nz; ++jz) {
				if (fmpz_is_zero(fmpq_numref(y + jz % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpq_div(z + jz, x + jz % nx, y + jz % ny);
				fmpz_fdiv_r(fmpq_numref(z + jz), fmpq_numref(z + jz), fmpq_denref(z + jz));
				fmpq_mul(z + jz, z + jz, y + jz % ny);
				}
			}
			break;
		case 5: /* "%/%" */
			for (jz = 0; jz < nz; ++jz) {
				if (fmpz_is_zero(fmpq_numref(y + jz % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpq_div(z + jz, x + jz % nx, y + jz % ny);
				fmpz_fdiv_q(fmpq_numref(z + jz), fmpq_numref(z + jz), fmpq_denref(z + jz));
				fmpz_one(fmpq_denref(z + jz));
				}
			}
			break;
		case 6: /*   "/" */
			for (jz = 0; jz < nz; ++jz)
				if (fmpz_is_zero(fmpq_numref(y + jz % ny)))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpq_div(z + jz, x + jz % nx, y + jz % ny);
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
			for (jz = 0; jz < nz; ++jz) {
				b = x + jz % nx;
				e = y + jz % ny;
				if ((fmpq_sgn(b) == 0 && fmpq_sgn(e) < 0) ||
				    (fmpq_sgn(b) <  0 && fmpz_is_even(fmpq_denref(e)))) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: value is not in the range of \"%s\""),
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
				exact = fmpz_root(fmpq_numref(z + jz), p, s);
				if (exact) {
				fmpz_pow_ui(p, fmpq_denref(b), u);
				exact = fmpz_root(fmpq_denref(z + jz), p, s);
				}
				} else {
				fmpz_neg(a, fmpq_numref(e));
				u = fmpz_get_ui(fmpq_numref(e));
				fmpz_pow_ui(p, fmpq_denref(b), u);
				exact = fmpz_root(fmpq_numref(z + jz), p, s);
				if (exact) {
				fmpz_pow_ui(p, fmpq_numref(b), u);
				exact = fmpz_root(fmpq_denref(z + jz), p, s);
				if (exact)
				fmpq_canonicalise(z + jz);
				}
				}
				if (!exact) {
				fmpz_clear(a);
				fmpz_clear(p);
				Rf_error(_("<%s> %s <%s>: value is not in the range of \"%s\""),
				         "fmpq", "^", "fmpq", "fmpq");
				}
			}
			fmpz_clear(a);
			fmpz_clear(p);
			break;
		}
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
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
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case  8: /*  "==" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_equal(x + jz % nx, y + jz % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_equal(x + jz % nx, y + jz % ny) == 0;
			break;
		case 10: /*   "<" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_cmp(x + jz % nx, y + jz % ny) < 0;
			break;
		case 11: /*   ">" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_cmp(x + jz % nx, y + jz % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_cmp(x + jz % nx, y + jz % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_cmp(x + jz % nx, y + jz % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = !fmpq_is_zero(x + jz % nx) && !fmpq_is_zero(y + jz % ny);
			break;
		case 15: /*   "|" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = !fmpq_is_zero(x + jz % nx) || !fmpq_is_zero(y + jz % ny);
			break;
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case 16: /*        "%*%" */
	case 17: /*  "crossprod" */
	case 18: /* "tcrossprod" */
	{
		/* C = A B                            */
		/*                                    */
		/*        %*%: C = Z', A = Y', B = X' */
		/*  crossprod: C = Z', A = Y', B = X  */
		/* tcrossprod: C = Z', A = Y , B = X' */

		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
		mp_limb_t jx, jy, ja, jb;
		fmpq_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(fmpq)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(fmpq)) : 0) : (void *) x;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += ma->r)
					fmpq_set(ma->entries + ja, y + jy);
		}
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r)
					fmpq_set(mb->entries + jb, x + jx);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpq *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpq *)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(fmpq *)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
		if (mb->r) {
			mb->rows[0] = mb->entries;
			for (i = 1; i < mb->r; ++i)
				mb->rows[i] = mb->rows[i - 1] + mb->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = ma->c;
#endif
		fmpq_mat_mul(mc, ma, mb);
		if (ty) {
			for (ja = 0; ja < ny; ++ja)
				fmpq_clear(ma->entries + ja);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jb = 0; jb < nx; ++jb)
				fmpq_clear(mb->entries + jb);
			flint_free(mb->entries);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case 19: /*      "solve" */
	case 20: /*  "backsolve" */
	case 21: /* "tbacksolve" */
	{
		/* A C = B                          */
		/*                                  */
		/*      solve: C = Z, A = X , B = Y */
		/*  backsolve: C = Z, A = X , B = Y */
		/* tbacksolve: C = Z, A = X', B = Y */
		int uplo = 'N';
		if (op == 20 || op == 21) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		fmpq_mat_t mc, ma, mb;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(fmpq)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(fmpq)) : 0;
		if (op == 21)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				fmpq_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += ma->r - (++i))
				for (j = 0; j <= i; ++j, ++ja)
					fmpq_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					fmpq_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += ma->c - (++i), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += mb->r)
				fmpq_set(mb->entries + jb, y + jy);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpq *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpq *)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(fmpq *)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
		if (mb->r) {
			mb->rows[0] = mb->entries;
			for (i = 1; i < mb->r; ++i)
				mb->rows[i] = mb->rows[i - 1] + mb->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = mb->c;
#endif
		singular = !fmpq_mat_solve(mc, ma, mb);
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += mc->c) {
				fmpq_set(z + jz, mc->entries + jc);
				fmpq_clear(mc->entries + jc);
			}
		for (ja = 0; ja < nx; ++ja)
			fmpq_clear(ma->entries + ja);
		for (jb = 0; jb < ny; ++jb)
			fmpq_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular"));
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
}

SEXP R_flint_fmpq_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	const fmpq *x = R_flint_get_pointer(s_x);
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
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_set(z + jz, x + jz);
			break;
		case  2: /*       "-" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_neg(z + jz, x + jz);
			break;
		case 10: /*      "Im" */
			for (jz = 0; jz < nz; ++jz)
				fmpz_one(fmpq_denref(z + jz));
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (jz = 0; jz < nz; ++jz)
				fmpq_abs(z + jz, x + jz);
			break;
		case 14: /*    "sign" */
			for (jz = 0; jz < nz; ++jz) {
				fmpz_set_si(fmpq_numref(z + jz), fmpq_sgn(x + jz));
				fmpz_one(fmpq_denref(z + jz));
			}
			break;
		case 15: /*    "sqrt" */
		{
			fmpz_t r;
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (fmpq_sgn(x + jz) >= 0) {
				fmpz_sqrtrem(fmpq_numref(z + jz), r, fmpq_numref(x + jz));
				if (fmpz_is_zero(r))
				fmpz_sqrtrem(fmpq_denref(z + jz), r, fmpq_denref(x + jz));
				}
				if (!(fmpq_sgn(x + jz) >= 0 && fmpz_is_zero(r))) {
				fmpz_clear(r);
				Rf_error(_("%s(<%s>): value is not in the range of \"%s\""),
				         "sqrt", "fmpq", "fmpq");
				}
			}
			fmpz_clear(r);
			break;
		}
		case 16: /*   "floor" */
			for (jz = 0; jz < nz; ++jz) {
				fmpz_fdiv_q(fmpq_numref(z + jz), fmpq_numref(x + jz), fmpq_denref(x + jz));
				fmpz_one(fmpq_denref(z + jz));
			}
			break;
		case 17: /* "ceiling" */
			for (jz = 0; jz < nz; ++jz) {
				fmpz_cdiv_q(fmpq_numref(z + jz), fmpq_numref(x + jz), fmpq_denref(x + jz));
				fmpz_one(fmpq_denref(z + jz));
			}
			break;
		case 18: /*   "trunc" */
			for (jz = 0; jz < nz; ++jz) {
				fmpz_tdiv_q(fmpq_numref(z + jz), fmpq_numref(x + jz), fmpq_denref(x + jz));
				fmpz_one(fmpq_denref(z + jz));
			}
			break;
		case 19: /*  "cummin" */
			if (nz) {
			fmpq_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpq_set(z + jz, (fmpq_cmp(z + jz - 1, x + jz) <= 0) ? z + jz - 1 : x + jz);
			}
			break;
		case 20: /*  "cummax" */
			if (nz) {
			fmpq_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpq_set(z + jz, (fmpq_cmp(z + jz - 1, x + jz) >= 0) ? z + jz - 1 : x + jz);
			}
			break;
		case 21: /*  "cumsum" */
			if (nz) {
			fmpq_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpq_add(z + jz, z + jz - 1, x + jz);
			}
			break;
		case 22: /* "cumprod" */
			if (nz) {
			fmpq_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpq_mul(z + jz, z + jz - 1, x + jz);
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
			for (jz = 0; jz < nz; ++jz) {
				fmpz_mul(fmpq_numref(z + jz), fmpq_numref(x + jz), p);
				fmpz_set(fmpq_denref(z + jz), fmpq_denref(x + jz));
				fmpz_ndiv_qr(q, r, fmpq_numref(z + jz), fmpq_denref(z + jz));
				if (fmpz_cmp2abs(fmpq_denref(z + jz), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_set(fmpq_numref(z + jz), q);
				fmpz_set(fmpq_denref(z + jz), p);
				fmpq_canonicalise(z + jz);
			}
			} else {
			/* a/b ~ c*10^-digits   <=>   c ~ a/(b*10^-digits) */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (jz = 0; jz < nz; ++jz) {
				fmpz_set(fmpq_numref(z + jz), fmpq_numref(x + jz));
				fmpz_mul(fmpq_denref(z + jz), fmpq_denref(x + jz), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(z + jz), fmpq_denref(z + jz));
				if (fmpz_cmp2abs(fmpq_denref(z + jz), r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(fmpq_numref(z + jz), q, p);
				fmpz_one(fmpq_denref(z + jz));
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
			for (jz = 0; jz < nz; ++jz) {
				if (fmpq_is_zero(x + jz))
				fmpq_zero(z + jz);
				else {
				fmpq_abs(a, x + jz);
				clog = fmpq_clog_ui(a, 10);
				if (fmpq_sgn(x + jz) < 0)
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
				fmpz_set(fmpq_numref(z + jz), q);
				fmpz_set(fmpq_denref(z + jz), p);
				fmpq_canonicalise(z + jz);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(fmpq_numref(z + jz), q, p);
				fmpz_one(fmpq_denref(z + jz));
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
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case 50: /*     "min" */
	case 51: /*     "max" */
	case 52: /*   "range" */
	case 55: /*    "mean" */
		if (nx == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	{
		nz = (op == 52) ? 2 : 1;
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = flint_calloc(nz, sizeof(fmpq));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		switch (op) {
		case 50: /*     "min" */
			fmpq_set(z, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpq_cmp(z, x + jx) > 0)
					fmpq_set(z, x + jx);
			break;
		case 51: /*     "max" */
			fmpq_set(z, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpq_cmp(z, x + jx) < 0)
					fmpq_set(z, x + jx);
			break;
		case 52: /*   "range" */
			fmpq_set(z, x);
			fmpq_set(z + 1, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpq_cmp(z, x + jx) > 0)
					fmpq_set(z, x + jx);
				else if (fmpq_cmp(z + 1, x + jx) < 0)
					fmpq_set(z + 1, x + jx);
			break;
		case 53: /*     "sum" */
			fmpq_zero(z);
			for (jx = 0; jx < nx; ++jx)
				fmpq_add(z, z, x + jx);
			break;
		case 54: /*    "prod" */
			fmpq_one(z);
			for (jx = 0; jx < nx; ++jx)
				fmpq_mul(z, z, x + jx);
			break;
		case 55: /*    "mean" */
		{
			fmpq_zero(z);
			for (jx = 0; jx < nx; ++jx)
				fmpq_add(z, z, x + jx);
			fmpz_mul_ui(fmpq_denref(z), fmpq_denref(z), nx);
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
			for (jx = 0; jx < nx &&  fmpq_is_zero(x + jx); ++jx) ;
			z[0] = jx <  nx;
			break;
		case 57: /*         "all" */
			for (jx = 0; jx < nx && !fmpq_is_zero(x + jx); ++jx) ;
			z[0] = jx >= nx;
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
			for (jx = 1; jx < nx && fmpq_cmp(x, x + 1) <  0; ++jx, ++x) ;
			else
			for (jx = 1; jx < nx && fmpq_cmp(x, x + 1) <= 0; ++jx, ++x) ;
			z[0] = jx <  nx;
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
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case  3: /*       "is.na" */
		case  4: /*      "is.nan" */
		case  5: /* "is.infinite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 0;
			break;
		case  6: /*   "is.finite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 1;
			break;
		case  7: /*           "!" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpq_is_zero(x + jz) != 0;
			break;
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case 60: /*     "colSums" */
	case 61: /*     "rowSums" */
	case 62: /*    "colMeans" */
	case 63: /*    "rowMeans" */
	{
		int byrow = op == 61 || op == 63, domean = op == 62 || op == 63;

		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		if (dimx == R_NilValue || XLENGTH(dimx) < 2)
			Rf_error(_("'%s' is not a matrix or a higher dimensional array"),
			         "x");
		if (domean && nx == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
		const int *dx = INTEGER_RO(dimx);
		int ndx = LENGTH(dimx);

		SEXP s_off = VECTOR_ELT(s_dots, 1);
		if (XLENGTH(s_off) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "dims", CHAR(STRING_ELT(s_op, 0)));
		int off = INTEGER_RO(s_off)[0];
		if (off < 1 || off >= ndx)
			Rf_error(_("'%s' is not in 1:(length(dim(%s))-1)"),
			         "dims", "x");

		SEXP dimz = PROTECT(Rf_allocVector(INTSXP, (byrow) ? off : ndx - off));
		int *dz = INTEGER(dimz), ndz = LENGTH(dimz), k;

		nz = 1;
		for (k = 0; k < ndz; ++k)
			nz *= (mp_limb_t) (dz[k] = dx[(byrow) ? k : off + k]);
		mp_limb_t jt, nt = (nz == 0) ? 0 : nx/nz;

		SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
			dimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue) {
			PROTECT(dimnamesx);
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, ndz));
			for (k = 0; k < ndz; ++k)
				SET_VECTOR_ELT(dimnamesz, k, VECTOR_ELT(dimnamesx, (byrow) ? k : off + k));
			SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol),
				namesdimnamesz = R_NilValue;
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesx);
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, ndz));
				for (k = 0; k < ndz; ++k)
					SET_STRING_ELT(namesdimnamesz, k, STRING_ELT(namesdimnamesx, (byrow) ? k : off + k));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(2);
			}
			UNPROTECT(2);
		}
		PROTECT(dimnamesz);

		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		jx = 0;
		if (byrow) {
			for (jz = 0; jz < nz; ++jz)
				fmpq_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					fmpq_add(z + jz, z + jz, x + jx);
			if (domean)
			for (jz = 0; jz < nz; ++jz) {
				fmpz_mul_ui(fmpq_denref(z + jz), fmpq_denref(z + jz), nt);
				fmpq_canonicalise(z + jz);
			}
		} else {
			for (jz = 0; jz < nz; ++jz) {
				fmpq_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					fmpq_add(z + jz, z + jz, x + jx);
				if (domean) {
				fmpz_mul_ui(fmpq_denref(z + jz), fmpq_denref(z + jz), nt);
				fmpq_canonicalise(z + jz);
				}
			}
		}
		if (ndz > 1)
			setDDNN(ans, dimz, dimnamesz, R_NilValue);
		else if (dimnamesz != R_NilValue)
			setDDNN(ans, R_NilValue, R_NilValue, VECTOR_ELT(dimnamesz, 0));
		UNPROTECT(4);
		return ans;
	}
	case 64: /*      "solve" */
	case 65: /*  "backsolve" */
	case 66: /* "tbacksolve" */
	{
		/* A C = I                    */
		/*                            */
		/*      solve: C = Z', A = X' */
		/*  backsolve: C = Z', A = X' */
		/* tbacksolve: C = Z', A = X  */
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("first argument is not a square matrix"));
		int uplo = 'N';
		if (op == 65 || op == 66) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		int i, j, singular;
		mp_limb_t ja;
		fmpq_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(fmpq)) : 0;
		if (op == 64 || op == 65)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				fmpq_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += ma->r - (++i))
				for (j = 0; j <= i; ++j, ++ja)
					fmpq_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					fmpq_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += ma->c - (++i), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					fmpq_set(ma->entries + ja, x + jx);
			break;
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpq *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpq *)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i - 1] + mc->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i - 1] + ma->c;
		}
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
#endif
		singular = !fmpq_mat_inv(mc, ma);
		for (ja = 0; ja < nx; ++ja)
			fmpq_clear(ma->entries + ja);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular"));
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
			dimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue) {
			PROTECT(dimnamesx);
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 0, VECTOR_ELT(dimnamesx, 1));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 0));
			SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol),
				namesdimnamesz = R_NilValue;
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesx);
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 0, STRING_ELT(namesdimnamesx, 1));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 0));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(2);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(2);
		}
		UNPROTECT(2);
		return ans;
	}
	case 69: /*        "det" */
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = flint_calloc(1, sizeof(fmpq));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		fmpq_mat_t mx;
		mx->r = mx->c = dx[0];
		mx->entries = (void *) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(fmpq *)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		fmpq_mat_det(z, mx);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mx->rows);
#endif
		UNPROTECT(2);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
}
