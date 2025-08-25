#include "flint.h"

arf_rnd_t remapRnd(mpfr_rnd_t);

void R_flint_arb_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	arb_ptr p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		arb_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP s_x, SEXP s_length,
                            SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                            SEXP s_mid, SEXP s_rad)
{
	mp_limb_t jy, nx = 0, ny = 0, nm = 1, nr = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	PROTECT(s_dim = validDim(s_dim));
	PROTECT(s_dimnames = validDimNames(s_dimnames, s_dim));
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "mid", "rad");
		if (s_mid != R_NilValue)
			nm = R_flint_get_length(s_mid);
		if (s_rad != R_NilValue)
			nr = R_flint_get_length(s_rad);
		ny = validLength(s_length, s_dim, RECYCLE2(nm, nr));
		if (ny > 0 && (nm == 0 || nr == 0))
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         (nm == 0) ? "mid" : "rad");
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
	arb_ptr y = (ny) ? flint_calloc(ny, sizeof(arb_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_arb_finalize);
	if (s_mid != R_NilValue || s_rad != R_NilValue) {
		if (s_mid != R_NilValue) {
			arf_srcptr xm = R_flint_get_pointer(s_mid);
			if (s_rad != R_NilValue) {
				mag_srcptr xr = R_flint_get_pointer(s_rad);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(y + jy), xm + jy % nm);
					mag_set(arb_radref(y + jy), xr + jy % nr);
				}
			} else {
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(y + jy), xm + jy % nm);
					mag_zero(arb_radref(y + jy));
				}
			}
		} else {
			if (s_rad != R_NilValue) {
				mag_srcptr xr = R_flint_get_pointer(s_rad);
				for (jy = 0; jy < ny; ++jy) {
					arf_zero(arb_midref(y + jy));
					mag_set(arb_radref(y + jy), xr + jy % nr);
				}
			}
		}
	} else {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (jy = 0; jy < ny; ++jy)
				arb_zero(y + jy);
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				arb_set_ui(y + jy, x[jy % nx]);
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				if (x[jy % nx] == NA_LOGICAL)
				arb_set_d(y + jy, R_NaN);
				else
				arb_set_si(y + jy, x[jy % nx]);
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				if (x[jy % nx] == NA_INTEGER)
				arb_set_d(y + jy, R_NaN);
				else
				arb_set_si(y + jy, x[jy % nx]);
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				arb_set_d(y + jy, x[jy % nx]);
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				arb_set_d(y + jy, x[jy % nx].r);
			break;
		}
		case STRSXP:
		{
			mpfr_prec_t prec = asPrec(R_NilValue, __func__);
			mpfr_rnd_t rnd = asRnd(R_NilValue, __func__);
			mpfr_t m, r;
			arf_t tmp;
			mpfr_init2(m, prec);
			mpfr_init2(r, MAG_BITS << 1);
			arf_init(tmp);
			const char *s;
			char *t;
			int negate;
			for (jy = 0; jy < ny; ++jy) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) (jy % nx)));
				while (isspace(*s))
					s++;
				negate = *s == '-';
				if (*s == '+' || negate)
					s++;
				while (isspace(*s))
					s++;
				if (*(s++) != '(')
					break;
				mpfr_strtofr(m, s, &t, 0, rnd);
				if (t <= s)
					break;
				s = t;
				if (negate)
					mpfr_neg(m, m, MPFR_RNDZ);
				while (isspace(*s))
					s++;
				if (*(s++) != '+' || *(s++) != '/' || *(s++) != '-')
					break;
				while (isspace(*s))
					s++;
				if (*s == '+' || *s == '-')
					break;
				mpfr_strtofr(r, s, &t, 0, MPFR_RNDA);
				if (t <= s)
					break;
				s = t;
				while (isspace(*s))
					s++;
				if (*(s++) != ')')
					break;
				while (isspace(*s))
					s++;
				if (*s != '\0')
					break;
				arf_set_mpfr(arb_midref(y + jy), m);
				arf_set_mpfr(tmp, r);
				arf_get_mag(arb_radref(y + jy), tmp);
			}
			mpfr_clear(m);
			mpfr_clear(r);
			arf_clear(tmp);
			if (jy < ny)
				Rf_error(_("invalid input in string conversion"));
			break;
		}
		case OBJSXP:
			switch (class) {
			case R_FLINT_CLASS_ULONG:
			{
				const ulong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					arb_set_ui(y + jy, x[jy % nx]);
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					arb_set_si(y + jy, x[jy % nx]);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					arb_set_fmpz(y + jy, x + jy % nx);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				for (jy = 0; jy < ny; ++jy)
					arb_fmpz_div_fmpz(y + jy, fmpq_numref(x + jy % nx), fmpq_denref(x + jy % nx), prec);
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_mag(arb_midref(y + jy), x + jy % nx);
					mag_zero(arb_radref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(y + jy), x + jy % nx);
					mag_zero(arb_radref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(y + jy), acf_realref(x + jy % nx));
					mag_zero(arb_radref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					arb_set(y + jy, x + jy % nx);
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					arb_set(y + jy, acb_realref(x + jy % nx));
				break;
			}
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

SEXP R_flint_arb_part(SEXP object, SEXP s_op)
{
	mp_limb_t j, n = R_flint_get_length(object);
	arb_srcptr x = R_flint_get_pointer(object);
	int op = INTEGER_RO(s_op)[0];
	SEXP ans = PROTECT(newObject((op == 0) ? "arf" : "mag"));
	if (op == 0) {
		arf_ptr y = (n) ? flint_calloc(n, sizeof(arf_t)) : 0;
		R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_arf_finalize);
		for (j = 0; j < n; ++j)
			arf_set(y + j, arb_midref(x + j));
	} else {
		mag_ptr y = (n) ? flint_calloc(n, sizeof(mag_t)) : 0;
		R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_mag_finalize);
		for (j = 0; j < n; ++j)
			mag_set(y + j, arb_radref(x + j));
	}
	setDDNN1(ans, object);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arb_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arb_srcptr x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = arb_midref(x + j);
		if (arf_is_nan(p))
			y[j] = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j] = arf_get_d(p, rnd);
		else {
			y[j] = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arb_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	arb_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	int dz[3];
	int mop = checkConformable(s_x, s_y, nx, ny, matrixop(op), dz);
	if (mop >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
	slong prec = asPrec(R_NilValue, __func__);
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	case  7: /*   "^" */
	{
		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (jz = 0; jz < nz; ++jz)
				arb_add(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 2: /*   "-" */
			for (jz = 0; jz < nz; ++jz)
				arb_sub(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 3: /*   "*" */
			for (jz = 0; jz < nz; ++jz)
				arb_mul(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 6: /*   "/" */
			for (jz = 0; jz < nz; ++jz)
				arb_div(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 7: /*   "^" */
			for (jz = 0; jz < nz; ++jz)
				arb_pow(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
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
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_eq(x + jz % nx, y + jz % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_ne(x + jz % nx, y + jz % ny) != 0;
			break;
		case 10: /*   "<" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_lt(x + jz % nx, y + jz % ny) != 0;
			break;
		case 11: /*   ">" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_gt(x + jz % nx, y + jz % ny) != 0;
			break;
		case 12: /*  "<=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_le(x + jz % nx, y + jz % ny) != 0;
			break;
		case 13: /*  ">=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: arb_ge(x + jz % nx, y + jz % ny) != 0;
			break;
		case 14: /*   "&" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_ZERO(x + jz % nx) ||
				 ARB_CONTAINS_ZERO(y + jz % ny))
				? 0
				:
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ARB_CONTAINS_NONZERO(x + jz % nx) ||
				 ARB_CONTAINS_NONZERO(y + jz % ny))
				? 1
				:
				(ARB_CONTAINS_NAN(x + jz % nx) ||
				 ARB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: 0;
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
		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arb_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
		mp_limb_t jx = 0, jy = 0, ja = 0, jb = 0;
		arb_mat_t mz, ma, mb;
		mz->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(arb_t)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(arb_t)) : 0) : (void *) x;
		mz->r = mb->c = dz[0];
		mz->c = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mz->rows = (mz->r) ? flint_calloc((size_t) mz->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(arb_ptr)) : 0;
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
					arb_set(ma->entries + ja, y + jy);
		if (tx)
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r)
					arb_set(mb->entries + jb, x + jx);
		arb_mat_mul(mz, ma, mb, prec);
		if (ty) {
			for (jy = 0; jy < ny; ++jy)
				arb_clear(ma->entries + jy);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jx = 0; jx < nx; ++jx)
				arb_clear(mb->entries + jx);
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
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}

SEXP R_flint_arb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t j, n = R_flint_get_length(s_x);
	arb_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	switch (op) {
	case  1: /*        "+" */
	case  2: /*        "-" */
	case  8: /*     "Conj" */
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
	case 14: /*     "sign" */
	case 15: /*     "sqrt" */
	case 16: /*    "floor" */
	case 17: /*  "ceiling" */
	case 18: /*    "trunc" */
	case 19: /*   "cummin" */
	case 20: /*   "cummax" */
	case 21: /*   "cumsum" */
	case 22: /*  "cumprod" */
	case 23: /*      "log" */
	case 24: /*    "log10" */
	case 25: /*     "log2" */
	case 26: /*    "log1p" */
	case 27: /*      "exp" */
	case 28: /*    "expm1" */
	case 29: /*      "cos" */
	case 30: /*    "cospi" */
	case 31: /*     "acos" */
	case 32: /*     "cosh" */
	case 33: /*    "acosh" */
	case 34: /*      "sin" */
	case 35: /*    "sinpi" */
	case 36: /*     "asin" */
	case 37: /*     "sinh" */
	case 38: /*    "asinh" */
	case 39: /*      "tan" */
	case 40: /*    "tanpi" */
	case 41: /*     "atan" */
	case 42: /*     "tanh" */
	case 43: /*    "atanh" */
	case 44: /*    "gamma" */
	case 45: /*   "lgamma" */
	case 46: /*  "digamma" */
	case 47: /* "trigamma" */
	case 48: /*    "round" */
	case 49: /*   "signif" */
	{
		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
		R_flint_set(ans, z, n, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case  1: /*        "+" */
		case  8: /*     "Conj" */
		case  9: /*       "Re" */
			for (j = 0; j < n; ++j)
				arb_set(z + j, x + j);
			break;
		case  2: /*        "-" */
			for (j = 0; j < n; ++j)
				arb_neg(z + j, x + j);
			break;
		case 10: /*       "Im" */
			for (j = 0; j < n; ++j)
				arb_zero(z + j);
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (j = 0; j < n; ++j)
				arb_nonnegative_abs(z + j, x + j);
			break;
		case 12: /*      "Arg" */
		{
			arb_t pi;
			arb_init(pi);
			arb_const_pi(pi, prec);
			for (j = 0; j < n; ++j) {
				if (arf_is_nan(arb_midref(x + j)) ||
				    mag_is_inf(arb_radref(x + j)) ||
				    arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) < 0) {
					arf_zero(arb_midref(z + j));
					mag_const_pi(arb_radref(z + j));
				}
				else if (arf_is_zero(arb_midref(x + j)))
					arb_zero(z + j);
				else {
					arb_set(z + j, pi);
					if (arf_sgn(arb_midref(x + j)) < 0)
					arb_neg(z + j, z + j);
				}
			}
			arb_clear(pi);
			break;
		}
		case 14: /*     "sign" */
			for (j = 0; j < n; ++j)
				arb_sgn(z + j, x + j);
			break;
		case 15: /*     "sqrt" */
			for (j = 0; j < n; ++j)
				arb_sqrt(z + j, x + j, prec);
			break;
		case 16: /*    "floor" */
			for (j = 0; j < n; ++j)
				arb_floor(z + j, x + j, prec);
			break;
		case 17: /*  "ceiling" */
			for (j = 0; j < n; ++j)
				arb_ceil(z + j, x + j, prec);
			break;
		case 18: /*    "trunc" */
			for (j = 0; j < n; ++j)
				arb_trunc(z + j, x + j, prec);
			break;
		case 19: /*   "cummin" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
				arb_min(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 20: /*   "cummax" */
			if (n) {
			arb_srcptr last = x;
			for (j = 0; j < n && !ARB_CONTAINS_NAN(x + j); ++j)
				arb_max(z + j, last, x + j, prec);
			for (; j < n; ++j)
				arb_indeterminate(z + j);
			}
			break;
		case 21: /*   "cumsum" */
			if (n) {
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_add(z + j, z + j - 1, x + j, prec);
			}
			break;
		case 22: /*  "cumprod" */
			if (n)
			arb_set(z, x);
			for (j = 1; j < n; ++j)
				arb_mul(z + j, z + j - 1, x + j, prec);
			break;
		case 23: /*      "log" */
		case 24: /*    "log10" */
		case 25: /*     "log2" */
			for (j = 0; j < n; ++j)
				arb_log(z + j, x + j, prec);
			if (op != 23 || s_dots != R_NilValue) {
			arb_t tmp;
			arb_init(tmp);
			if (op != 23)
				arb_set_ui(tmp, (op == 24) ? 10 : 2);
			else {
				SEXP s_base = VECTOR_ELT(s_dots, 0);
				if (R_flint_get_length(s_base) == 0)
					Rf_error(_("'%s' of length zero in '%s'"),
					         "base", CHAR(STRING_ELT(s_op, 0)));
				arb_srcptr base = R_flint_get_pointer(s_base);
				arb_set(tmp, base);
			}
			arb_log(tmp, tmp, prec);
			for (j = 0; j < n; ++j)
				arb_div(z + j, z + j, tmp, prec);
			arb_clear(tmp);
			}
			break;
		case 26: /*    "log1p" */
			for (j = 0; j < n; ++j)
				arb_log1p(z + j, x + j, prec);
			break;
		case 27: /*      "exp" */
			for (j = 0; j < n; ++j)
				arb_exp(z + j, x + j, prec);
			break;
		case 28: /*    "expm1" */
			for (j = 0; j < n; ++j)
				arb_expm1(z + j, x + j, prec);
			break;
		case 29: /*      "cos" */
			for (j = 0; j < n; ++j)
				arb_cos(z + j, x + j, prec);
			break;
		case 30: /*    "cospi" */
			for (j = 0; j < n; ++j)
				arb_cos_pi(z + j, x + j, prec);
			break;
		case 31: /*     "acos" */
			for (j = 0; j < n; ++j)
				arb_acos(z + j, x + j, prec);
			break;
		case 32: /*     "cosh" */
			for (j = 0; j < n; ++j)
				arb_cosh(z + j, x + j, prec);
			break;
		case 33: /*    "acosh" */
			for (j = 0; j < n; ++j)
				arb_acosh(z + j, x + j, prec);
			break;
		case 34: /*      "sin" */
			for (j = 0; j < n; ++j)
				arb_sin(z + j, x + j, prec);
			break;
		case 35: /*    "sinpi" */
			for (j = 0; j < n; ++j)
				arb_sin_pi(z + j, x + j, prec);
			break;
		case 36: /*     "asin" */
			for (j = 0; j < n; ++j)
				arb_asin(z + j, x + j, prec);
			break;
		case 37: /*     "sinh" */
			for (j = 0; j < n; ++j)
				arb_sinh(z + j, x + j, prec);
			break;
		case 38: /*    "asinh" */
			for (j = 0; j < n; ++j)
				arb_asinh(z + j, x + j, prec);
			break;
		case 39: /*      "tan" */
			for (j = 0; j < n; ++j)
				arb_tan(z + j, x + j, prec);
			break;
		case 40: /*    "tanpi" */
			for (j = 0; j < n; ++j)
				arb_tan_pi(z + j, x + j, prec);
			break;
		case 41: /*     "atan" */
			for (j = 0; j < n; ++j)
				arb_atan(z + j, x + j, prec);
			break;
		case 42: /*     "tanh" */
			for (j = 0; j < n; ++j)
				arb_tanh(z + j, x + j, prec);
			break;
		case 43: /*    "atanh" */
			for (j = 0; j < n; ++j)
				arb_atanh(z + j, x + j, prec);
			break;
		case 44: /*    "gamma" */
			for (j = 0; j < n; ++j)
				arb_gamma(z + j, x + j, prec);
			break;
		case 45: /*   "lgamma" */
			for (j = 0; j < n; ++j)
				arb_lgamma(z + j, x + j, prec);
			break;
		case 46: /*  "digamma" */
			for (j = 0; j < n; ++j)
				arb_digamma(z + j, x + j, prec);
			break;
		case 47: /* "trigamma" */
		{
			acb_t tmp0, tmp1, tmp2;
			acb_init(tmp0);
			acb_init(tmp1);
			acb_init(tmp2);
			acb_set_si(tmp1, 1);
			arb_zero(acb_imagref(tmp2));
			for (j = 0; j < n; ++j) {
				arb_set(acb_realref(tmp2), x + j);
				acb_polygamma(tmp0, tmp1, tmp2, prec);
				arb_set(z + j, acb_realref(tmp0));
			}
			acb_clear(tmp0);
			acb_clear(tmp1);
			acb_clear(tmp2);
			break;
		}
		case 48: /*    "round" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
			fmpz_t p, q;
			arf_t s;
			mag_t d;
			arf_srcptr xm;
			mag_srcptr xr;
			arf_ptr zm;
			mag_ptr zr;
			fmpz_init(p);
			fmpz_init(q);
			arf_init(s);
			mag_init(d);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* f ~ c/10^+digits   <=>   c ~ f*10^+digits */
			fmpz_pow_ui(p, p, (ulong) digits);
			fmpz_mul_si(q, p, 2);
			arf_one(s);
			arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP);
			arf_get_mag(d, s);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd);
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			fmpz_divexact_si(q, p, 2);
			mag_set_fmpz(d, q);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_div_fmpz(s, xm, p, prec, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				arf_set_fmpz(zm, q);
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			arf_clear(s);
			mag_clear(d);
			break;
		}
		case 49: /*   "signif" */
		{
			slong fmpq_clog_ui(const fmpq_t, ulong);
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
			arf_t s;
			mag_t d;
			arf_srcptr xm;
			mag_srcptr xr;
			arf_ptr zm;
			mag_ptr zr;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			arf_init(s);
			mag_init(d);
			for (j = 0; j < n; ++j) {
				xm = arb_midref(x + j);
				xr = arb_radref(x + j);
				zm = arb_midref(z + j);
				zr = arb_radref(z + j);
				if (!arf_is_finite(xm)) {
				arf_set(zm, xm);
				mag_inf(zr); /* FIXME: Is there another option? */
				} else {
				arf_get_fmpq(a, xm);
				fmpq_abs(a, a);
				clog = fmpq_clog_ui(a, 10);
				if (arf_sgn(xm) < 0)
					fmpq_neg(a, a);
				fmpz_set_si(p, 10);
				if (clog <= digits) {
				if (clog >= 0)
				fmpz_pow_ui(p, p, (ulong) (digits - clog));
				else
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1));
				fmpz_mul_si(q, p, 2);
				arf_one(s);
				arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP);
				arf_get_mag(d, s);
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_divexact_si(q, p, 2);
				mag_set_fmpz(d, q);
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				arf_set_fmpz(zm, q);
				}
				if (arf_equal(xm, zm) != 0)
				mag_set(zr, xr);
				else
				mag_add(zr, xr, d);
				}
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			arf_clear(s);
			mag_clear(d);
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
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];
		SEXP ans = PROTECT(newObject("arb"));
		mp_limb_t nz = (op == 52) ? 2 : 1;
		arb_ptr z = flint_calloc(nz, sizeof(arb_t));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case 50: /*     "min" */
			arb_pos_inf(z);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j))
					arb_min(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 51: /*     "max" */
			arb_neg_inf(z);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j))
					arb_max(z, z, x + j, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case 52: /*   "range" */
			arb_pos_inf(z);
			arb_neg_inf(z + 1);
			for (j = 0; j < n; ++j)
				if (!ARB_CONTAINS_NAN(x + j)) {
					arb_min(z, z, x + j, prec);
					arb_max(z + 1, z + 1, x + j, prec);
				} else if (!narm) {
					arb_indeterminate(z);
					arb_indeterminate(z + 1);
					break;
				}
			break;
		case 53: /*     "sum" */
			arb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_add(z, z, x + j, prec);
			break;
		case 54: /*    "prod" */
			arb_one(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_mul(z, z, x + j, prec);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = n;
			arb_zero(z);
			for (j = 0; j < n; ++j)
				if (!(narm && ARB_CONTAINS_NAN(x + j)))
				arb_add(z, z, x + j, prec);
				else
				--c;
			if (c == 0)
			arb_indeterminate(z);
			else
			arb_div_ui(z, z, c, prec);
			break;
		}
		}
		UNPROTECT(1);
		return ans;
	}
	case 56: /*     "any" */
	case 57: /*     "all" */
	case 58: /*   "anyNA" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0], anyna = 0;
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*     "any" */
			/* Return 1 if and only if any does not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) >  0)
					break;
			z[0] = (j < n) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*     "all" */
			/* Return 1 if and only if all do   not contain zero */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + j), arb_radref(x + j)) <= 0)
					break;
			z[0] = (j < n) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*   "anyNA" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
					break;
			z[0] = j < n;
			break;
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
			for (j = 0; j < n; ++j)
				z[j] = arf_is_nan(arb_midref(x + j)) != 0;
			break;
		case  5: /* "is.infinite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_inf(arb_midref(x + j)) != 0 ||
					mag_is_inf(arb_radref(x + j)) != 0;
			break;
		case  6: /*   "is.finite" */
			for (j = 0; j < n; ++j)
				z[j] =
					arf_is_finite(arb_midref(x + j)) != 0 &&
					mag_is_finite(arb_radref(x + j)) != 0;
			break;
		case  7: /*           "!" */
			for (j = 0; j < n; ++j)
				if (arf_is_nan(arb_midref(x + j)))
				z[j] = NA_LOGICAL;
				else
				z[j] =
					arf_is_zero(arb_midref(x + j)) != 0 &&
					mag_is_zero(arb_radref(x + j)) != 0;
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
		const int *dx = INTEGER_RO(dimx);
		int ndx = LENGTH(dimx);

		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];

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

		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arb_finalize);
		if (byrow) {
			mp_limb_t *c = 0;
			if (domean && nz) {
				c = (mp_limb_t *) R_alloc(nz, sizeof(mp_limb_t));
				memset(c, 0, nz * sizeof(mp_limb_t));
			}
			for (jz = 0; jz < nz; ++jz)
				arb_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					if (!(narm && ARB_CONTAINS_NAN(z + jz)))
						arb_add(z + jz, z + jz, x + jx, prec);
					else if (domean)
						++c[jz];
			if (domean) {
			for (jz = 0; jz < nz; ++jz)
				if (c[jz] == nt)
					arb_indeterminate(z + jz);
				else
					arb_div_ui(z + jz, z + jz, nt - c[jz], prec);
			}
		} else {
			mp_limb_t c = 0;
			for (jz = 0; jz < nz; ++jz) {
				arb_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					if (!(narm && ARB_CONTAINS_NAN(x + jx)))
						arb_add(z + jz, z + jz, x + jx, prec);
					else if (domean)
						++c;
				if (domean) {
					if (c == nt)
						arb_indeterminate(z + jz);
					else
						arb_div_ui(z + jz, z + jz, nt - c, prec);
					c = 0;
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
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}
