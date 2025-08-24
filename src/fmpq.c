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
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
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
					Rf_error(_("zero denominator not valid in canonical '%s'"), "fmpq");
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
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
				Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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
					Rf_error(_("NaN, -Inf, Inf are not representable by '%s'"), "fmpq");
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

SEXP R_flint_fmpq_ops2(SEXP s_op, SEXP s_x, SEXP s_y)
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
				Rf_error(_("<%s> %s <%s>: value is not in the range of '%s'"),
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
		/*        %*%: Z = X Y  = (Y'X')' = (A B)', A := Y', B := X' */
		/*  crossprod: Z = X'Y  = (Y'X )' = (A B)', A := Y', B := X  */
		/* tcrossprod: Z = X Y' = (Y X')' = (A B)', A := Y , B := X' */
		SEXP ans = PROTECT(newObject("fmpq"));
		fmpq *z = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
		mp_limb_t jx = 0, jy = 0, ja = 0, jb = 0;
		fmpq_mat_t mz, ma, mb;
		mz->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(fmpq)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(fmpq)) : 0) : (void *) x;
		mz->r = mb->c = dz[0];
		mz->c = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mz->rows = (mz->r) ? flint_calloc((size_t) mz->r, sizeof(fmpq *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpq *)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(fmpq *)) : 0;
		if (mz->r) {
			mz->rows[0] = mz->entries;
			for (i = 1; i < mz->r; ++i)
				mz->rows[i] = mz->rows[i-1] + mz->c;
		}
		if (ma->r) {
			ma->rows[0] = ma->entries;
			for (i = 1; i < ma->r; ++i)
				ma->rows[i] = ma->rows[i-1] + ma->c;
		}
		if (mb->r) {
			mb->rows[0] = mb->entries;
			for (i = 1; i < mb->r; ++i)
				mb->rows[i] = mb->rows[i-1] + mb->c;
		}
		if (ty)
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += ma->r)
					fmpq_set(ma->entries + ja, y + jy);
		if (tx)
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r)
					fmpq_set(mb->entries + jb, x + jx);
		fmpq_mat_mul(mz, ma, mb);
		if (ty) {
			for (jy = 0; jy < ny; ++jy)
				fmpq_clear(ma->entries + jy);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jx = 0; jx < nx; ++jx)
				fmpq_clear(mb->entries + jx);
			flint_free(mb->entries);
		}
		flint_free(mz->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
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
	mp_limb_t j, n = R_flint_get_length(s_x);
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
		setDDNN1(ans, s_x);
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
		mp_limb_t nz = (op == 52) ? 2 : 1;
		fmpq *z = flint_calloc(nz, sizeof(fmpq));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_fmpq_finalize);
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
		if (domean && n == 0)
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

		mp_limb_t jx = 0, jz, nx = n, nz = 1;
		for (k = 0; k < ndz; ++k)
			nz *= (mp_limb_t) (dz[k] = dx[(byrow) ? k : off + k]);
		mp_limb_t jt, nt = nx/nz;

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
	default:
		Rf_error(_("operation '%s' is not yet implemented for class '%s'"),
		         CHAR(STRING_ELT(s_op, 0)), "fmpq");
		return R_NilValue;
	}
}
