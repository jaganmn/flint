#include "flint.h"

arf_rnd_t remapRnd(mpfr_rnd_t);

void R_flint_acb_finalize(SEXP x)
{
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	acb_ptr p = R_ExternalPtrAddr(x);
	for (j = 0; j < n; ++j)
		acb_clear(p + j);
	flint_free(p);
	return;
}

SEXP R_flint_acb_initialize(SEXP object, SEXP s_x, SEXP s_length,
                            SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                            SEXP s_real, SEXP s_imag)
{
	mp_limb_t jy, nx = 0, ny = 0, nr = 1, ni = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	PROTECT(s_dim = validDim(s_dim));
	PROTECT(s_dimnames = validDimNames(s_dimnames, s_dim));
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_x != R_NilValue)
			Rf_error(_("'%s' usage and '%s', '%s' usage are mutually exclusive"),
			         "x", "real", "imag");
		if (s_real != R_NilValue)
			nr = R_flint_get_length(s_real);
		if (s_imag != R_NilValue)
			ni = R_flint_get_length(s_imag);
		ny = validLength(s_length, s_dim, RECYCLE2(nr, ni));
		if (ny > 0 && (nr == 0 || ni == 0))
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         (nr == 0) ? "real" : "imag");
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
	acb_ptr y = (ny) ? flint_calloc(ny, sizeof(acb_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_acb_finalize);
	if (s_real != R_NilValue || s_imag != R_NilValue) {
		if (s_real != R_NilValue) {
			arb_srcptr xr = R_flint_get_pointer(s_real);
			if (s_imag != R_NilValue) {
				arb_srcptr xi = R_flint_get_pointer(s_imag);
				for (jy = 0; jy < ny; ++jy) {
					arb_set(acb_realref(y + jy), xr + jy % nr);
					arb_set(acb_imagref(y + jy), xi + jy % ni);
				}
			} else {
				for (jy = 0; jy < ny; ++jy) {
					arb_set(acb_realref(y + jy), xr + jy % nr);
					arb_zero(acb_imagref(y + jy));
				}
			}
		} else {
			if (s_imag != R_NilValue) {
				arb_srcptr xi = R_flint_get_pointer(s_imag);
				for (jy = 0; jy < ny; ++jy) {
					arb_zero(acb_realref(y + jy));
					arb_set(acb_imagref(y + jy), xi + jy % ni);
				}
			}
		}
	} else if (s_x != R_NilValue) {
		switch (TYPEOF(s_x)) {
		case NILSXP:
			for (jy = 0; jy < ny; ++jy)
				acb_zero(y + jy);
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				acb_set_ui(y + jy, x[jy % nx]);
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				if (x[jy % nx] == NA_LOGICAL)
				acb_set_d(y + jy, R_NaN);
				else
				acb_set_si(y + jy, x[jy % nx]);
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				if (x[jy % nx] == NA_INTEGER)
				acb_set_d(y + jy, R_NaN);
				else
				acb_set_si(y + jy, x[jy % nx]);
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				acb_set_d(y + jy, x[jy % nx]);
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			for (jy = 0; jy < ny; ++jy)
				acb_set_d_d(y + jy, x[jy % nx].r, x[jy % nx].i);
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
#define COMMON \
				do { \
				while (isspace(*s)) \
					s++; \
				negate = *s == '-'; \
				if (*s == '+' || negate) \
					s++; \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != '(') \
					break; \
				mpfr_strtofr(m, s, &t, 0, rnd); \
				if (t <= s) \
					break; \
				s = t; \
				if (negate) \
					mpfr_neg(m, m, MPFR_RNDZ); \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != '+' || *(s++) != '/' || *(s++) != '-') \
					break; \
				while (isspace(*s)) \
					s++; \
				mpfr_strtofr(r, s, &t, 0, MPFR_RNDA); \
				if (t <= s) \
					break; \
				s = t; \
				while (isspace(*s)) \
					s++; \
				if (*(s++) != ')') \
					break; \
				} while (0)
				COMMON;
				if (*s != 'i')
					while (isspace(*s))
						s++;
				if (*s == '\0') {
					arf_set_mpfr(arb_midref(acb_realref(y + jy)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_realref(y + jy)), tmp);
					arb_zero(acb_imagref(y + jy));
				} else if (*s == 'i') {
					s++;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arb_zero(acb_realref(y + jy));
					arf_set_mpfr(arb_midref(acb_imagref(y + jy)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_imagref(y + jy)), tmp);
				} else if (*s == '+' || *s == '-') {
					arf_set_mpfr(arb_midref(acb_realref(y + jy)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_realref(y + jy)), tmp);
					COMMON;
					if (*s != 'i')
						break;
					s++;
					while (isspace(*s))
						s++;
					if (*s != '\0')
						break;
					arf_set_mpfr(arb_midref(acb_imagref(y + jy)), m);
					arf_set_mpfr(tmp, r);
					arf_get_mag(arb_radref(acb_imagref(y + jy)), tmp);
				} else
					break;
#undef COMMON
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
					acb_set_ui(y + jy, x[jy % nx]);
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					acb_set_si(y + jy, x[jy % nx]);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					acb_set_fmpz(y + jy, x + jy % nx);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				for (jy = 0; jy < ny; ++jy) {
					arb_fmpz_div_fmpz(acb_realref(y + jy), fmpq_numref(x + jy % nx), fmpq_denref(x + jy % nx), prec);
					arb_zero(acb_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set_mag(arb_midref(acb_realref(y + jy)), x + jy % nx);
					arf_zero(arb_midref(acb_imagref(y + jy)));
					mag_zero(arb_radref(acb_realref(y + jy)));
					mag_zero(arb_radref(acb_imagref(y + jy)));
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(acb_realref(y + jy)), x + jy % nx);
					arf_zero(arb_midref(acb_imagref(y + jy)));
					mag_zero(arb_radref(acb_realref(y + jy)));
					mag_zero(arb_radref(acb_imagref(y + jy)));
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arf_set(arb_midref(acb_realref(y + jy)), acf_realref(x + jy % nx));
					arf_set(arb_midref(acb_imagref(y + jy)), acf_imagref(x + jy % nx));
					mag_zero(arb_radref(acb_realref(y + jy)));
					mag_zero(arb_radref(acb_imagref(y + jy)));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy) {
					arb_set(acb_realref(y + jy), x + jy % nx);
					arb_zero(acb_imagref(y + jy));
				}
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = R_flint_get_pointer(s_x);
				for (jy = 0; jy < ny; ++jy)
					acb_set(y + jy, x + jy % nx);
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

SEXP R_flint_acb_part(SEXP object, SEXP s_op)
{
	mp_limb_t j, n = R_flint_get_length(object);
	acb_srcptr x = R_flint_get_pointer(object);
	int op = INTEGER_RO(s_op)[0];
	SEXP ans;
	if (op == NA_INTEGER) {
	PROTECT(ans = newObject("acf"));
	acf_ptr y = (n) ? flint_calloc(n, sizeof(acf_t)) : 0;
	R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_acf_finalize);
	for (j = 0; j < n; ++j) {
		arf_set(acf_realref(y + j), arb_midref(acb_realref(x + j)));
		arf_set(acf_imagref(y + j), arb_midref(acb_imagref(x + j)));
	}
	} else {
	PROTECT(ans = newObject("arb"));
	arb_ptr y = (n) ? flint_calloc(n, sizeof(arb_t)) : 0;
	R_flint_set(ans, y, n, (R_CFinalizer_t) &R_flint_arb_finalize);
	if (op == 0)
	for (j = 0; j < n; ++j)
		arb_set(y + j, acb_realref(x + j));
	else
	for (j = 0; j < n; ++j)
		arb_set(y + j, acb_imagref(x + j));
	}
	setDDNN1(ans, object);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_acb_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(CPLXSXP, (R_xlen_t) n));
	acb_srcptr x = R_flint_get_pointer(object);
	Rcomplex *y = COMPLEX(ans);
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		p = arb_midref(acb_realref(x + j));
		if (arf_is_nan(p))
			y[j].r = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j].r = arf_get_d(p, rnd);
		else {
			y[j].r = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
		p = arb_midref(acb_imagref(x + j));
		if (arf_is_nan(p))
			y[j].i = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j].i = arf_get_d(p, rnd);
		else {
			y[j].i = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_acb_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	acb_srcptr
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
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (jz = 0; jz < nz; ++jz)
				acb_add(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 2: /*   "-" */
			for (jz = 0; jz < nz; ++jz)
				acb_sub(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 3: /*   "*" */
			for (jz = 0; jz < nz; ++jz)
				acb_mul(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 6: /*   "/" */
			for (jz = 0; jz < nz; ++jz)
				acb_div(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		case 7: /*   "^" */
			for (jz = 0; jz < nz; ++jz)
				acb_pow(z + jz, x + jz % nx, y + jz % ny, prec);
			break;
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	case  8: /*  "==" */
	case  9: /*  "!=" */
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
				(ACB_CONTAINS_NAN(x + jz % nx) ||
				 ACB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: acb_eq(x + jz % nx, y + jz % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ACB_CONTAINS_NAN(x + jz % nx) ||
				 ACB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: acb_ne(x + jz % nx, y + jz % ny) != 0;
			break;
		case 14: /*   "&" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ACB_CONTAINS_ZERO(x + jz % nx) ||
				 ACB_CONTAINS_ZERO(y + jz % ny))
				? 0
				:
				(ACB_CONTAINS_NAN(x + jz % nx) ||
				 ACB_CONTAINS_NAN(y + jz % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(ACB_CONTAINS_NONZERO(x + jz % nx) ||
				 ACB_CONTAINS_NONZERO(y + jz % ny))
				? 1
				:
				(ACB_CONTAINS_NAN(x + jz % nx) ||
				 ACB_CONTAINS_NAN(y + jz % ny))
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
		/* C = A B                            */
		/*                                    */
		/*        %*%: C = Z', A = Y', B = X' */
		/*  crossprod: C = Z', A = Y', B = X  */
		/* tcrossprod: C = Z', A = Y , B = X' */

		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
		mp_limb_t jx, jy, ja, jb;
		acb_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(acb_t)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(acb_t)) : 0) : (void *) x;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += ma->r)
					acb_set(ma->entries + ja, y + jy);
		}
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r)
					acb_set(mb->entries + jb, x + jx);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i-1] + mc->c;
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
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = ma->c;
#endif
		acb_mat_mul(mc, ma, mb, prec);
		if (ty) {
			for (ja = 0; ja < ny; ++ja)
				acb_clear(ma->entries + ja);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jb = 0; jb < nx; ++jb)
				acb_clear(mb->entries + jb);
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
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		acb_mat_t mc, ma, mb;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(acb_t)) : 0;
		if (op == 21)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				acb_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += ma->r - (++i))
				for (j = 0; j <= i; ++j, ++ja)
					acb_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					acb_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += ma->c - (++i), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += mb->r)
				acb_set(mb->entries + jb, y + jy);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(acb_ptr)) : 0;
		if (mc->r) {
			mc->rows[0] = mc->entries;
			for (i = 1; i < mc->r; ++i)
				mc->rows[i] = mc->rows[i-1] + mc->c;
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
#else
		mc->stride = mc->c;
		ma->stride = ma->c;
		mb->stride = ma->c;
#endif
		if (uplo == 'N')
			singular = !acb_mat_approx_solve(mc, ma, mb, prec);
		else if ((uplo == 'U') == (op != 21)) {
			acb_mat_solve_triu(mc, ma, mb, 0, prec);
			singular = 0;
		}
		else {
			acb_mat_solve_tril(mc, ma, mb, 0, prec);
			singular = 0;
		}
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += mc->c) {
				acb_set(z + jz, mc->entries + jc);
				acb_clear(mc->entries + jc);
			}
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		for (jb = 0; jb < ny; ++jb)
			acb_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular or precision is insufficient"));
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
}

SEXP R_flint_acb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	acb_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	switch (op) {
	case  1: /*        "+" */
	case  2: /*        "-" */
	case  8: /*     "Conj" */
	case 14: /*     "sign" */
	case 15: /*     "sqrt" */
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
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		switch (op) {
		case  1: /*        "+" */
			for (jz = 0; jz < nz; ++jz)
				acb_set(z + jz, x + jz);
			break;
		case  2: /*        "-" */
			for (jz = 0; jz < nz; ++jz)
				acb_neg(z + jz, x + jz);
			break;
		case  8: /*     "Conj" */
			for (jz = 0; jz < nz; ++jz)
				acb_conj(z + jz, x + jz);
			break;
		case 14: /*     "sign" */
			for (jz = 0; jz < nz; ++jz)
				acb_sgn(z + jz, x + jz, prec);
			break;
		case 15: /*     "sqrt" */
			for (jz = 0; jz < nz; ++jz)
				acb_sqrt(z + jz, x + jz, prec);
			break;
		case 21: /*   "cumsum" */
			if (nz) {
			acb_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				acb_add(z + jz, z + jz - 1, x + jz, prec);
			}
			break;
		case 22: /*  "cumprod" */
			if (nz)
			acb_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				acb_mul(z + jz, z + jz - 1, x + jz, prec);
			break;
		case 23: /*      "log" */
		case 24: /*    "log10" */
		case 25: /*     "log2" */
			for (jz = 0; jz < nz; ++jz)
				acb_log(z + jz, x + jz, prec);
			if (op != 23 || s_dots != R_NilValue) {
			acb_t tmp;
			acb_init(tmp);
			if (op != 23)
				acb_set_ui(tmp, (op == 24) ? 10 : 2);
			else {
				SEXP s_base = VECTOR_ELT(s_dots, 0);
				if (R_flint_get_length(s_base) == 0)
					Rf_error(_("'%s' of length zero in '%s'"),
					         "base", CHAR(STRING_ELT(s_op, 0)));
				acb_srcptr base = R_flint_get_pointer(s_base);
				acb_set(tmp, base);
			}
			acb_log(tmp, tmp, prec);
			for (jz = 0; jz < nz; ++jz)
				acb_div(z + jz, z + jz, tmp, prec);
			acb_clear(tmp);
			}
			break;
		case 26: /*    "log1p" */
			for (jz = 0; jz < nz; ++jz)
				acb_log1p(z + jz, x + jz, prec);
			break;
		case 27: /*      "exp" */
			for (jz = 0; jz < nz; ++jz)
				acb_exp(z + jz, x + jz, prec);
			break;
		case 28: /*    "expm1" */
			for (jz = 0; jz < nz; ++jz)
				acb_expm1(z + jz, x + jz, prec);
			break;
		case 29: /*      "cos" */
			for (jz = 0; jz < nz; ++jz)
				acb_cos(z + jz, x + jz, prec);
			break;
		case 30: /*    "cospi" */
			for (jz = 0; jz < nz; ++jz)
				acb_cos_pi(z + jz, x + jz, prec);
			break;
		case 31: /*     "acos" */
			for (jz = 0; jz < nz; ++jz)
				acb_acos(z + jz, x + jz, prec);
			break;
		case 32: /*     "cosh" */
			for (jz = 0; jz < nz; ++jz)
				acb_cosh(z + jz, x + jz, prec);
			break;
		case 33: /*    "acosh" */
			for (jz = 0; jz < nz; ++jz)
				acb_acosh(z + jz, x + jz, prec);
			break;
		case 34: /*      "sin" */
			for (jz = 0; jz < nz; ++jz)
				acb_sin(z + jz, x + jz, prec);
			break;
		case 35: /*    "sinpi" */
			for (jz = 0; jz < nz; ++jz)
				acb_sin_pi(z + jz, x + jz, prec);
			break;
		case 36: /*     "asin" */
			for (jz = 0; jz < nz; ++jz)
				acb_asin(z + jz, x + jz, prec);
			break;
		case 37: /*     "sinh" */
			for (jz = 0; jz < nz; ++jz)
				acb_sinh(z + jz, x + jz, prec);
			break;
		case 38: /*    "asinh" */
			for (jz = 0; jz < nz; ++jz)
				acb_asinh(z + jz, x + jz, prec);
			break;
		case 39: /*      "tan" */
			for (jz = 0; jz < nz; ++jz)
				acb_tan(z + jz, x + jz, prec);
			break;
		case 40: /*    "tanpi" */
			for (jz = 0; jz < nz; ++jz)
				acb_tan_pi(z + jz, x + jz, prec);
			break;
		case 41: /*     "atan" */
			for (jz = 0; jz < nz; ++jz)
				acb_atan(z + jz, x + jz, prec);
			break;
		case 42: /*     "tanh" */
			for (jz = 0; jz < nz; ++jz)
				acb_tanh(z + jz, x + jz, prec);
			break;
		case 43: /*    "atanh" */
			for (jz = 0; jz < nz; ++jz)
				acb_atanh(z + jz, x + jz, prec);
			break;
		case 44: /*    "gamma" */
			for (jz = 0; jz < nz; ++jz)
				acb_gamma(z + jz, x + jz, prec);
			break;
		case 45: /*   "lgamma" */
			for (jz = 0; jz < nz; ++jz)
				acb_lgamma(z + jz, x + jz, prec);
			break;
		case 46: /*  "digamma" */
			for (jz = 0; jz < nz; ++jz)
				acb_digamma(z + jz, x + jz, prec);
			break;
		case 47: /* "trigamma" */
		{
			acb_t tmp;
			acb_init(tmp);
			acb_set_si(tmp, 1);
			for (jz = 0; jz < nz; ++jz)
				acb_polygamma(z + jz, tmp, x + jz, prec);
			acb_clear(tmp);
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
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + jz)); \
				xr = arb_radref(acb_partref(x + jz)); \
				zm = arb_midref(acb_partref(z + jz)); \
				zr = arb_radref(acb_partref(z + jz)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_mul_fmpz(s, xm, p, ARF_PREC_EXACT, rnd); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
#undef TEMPLATE
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			fmpz_divexact_si(q, p, 2);
			mag_set_fmpz(d, q);
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + jz)); \
				xr = arb_radref(acb_partref(x + jz)); \
				zm = arb_midref(acb_partref(z + jz)); \
				zr = arb_radref(acb_partref(z + jz)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_div_fmpz(s, xm, p, prec, rnd); \
				arf_get_fmpz(q, s, ARF_RND_NEAR); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
#undef TEMPLATE
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
			for (jz = 0; jz < nz; ++jz) {
#define TEMPLATE(acb_partref) \
				do { \
				xm = arb_midref(acb_partref(x + jz)); \
				xr = arb_radref(acb_partref(x + jz)); \
				zm = arb_midref(acb_partref(z + jz)); \
				zr = arb_radref(acb_partref(z + jz)); \
				if (!arf_is_finite(xm)) { \
				arf_set(zm, xm); \
				mag_inf(zr); /* FIXME: Is there another option? */ \
				} else { \
				arf_get_fmpq(a, xm); \
				fmpq_abs(a, a); \
				clog = fmpq_clog_ui(a, 10); \
				if (arf_sgn(xm) < 0) \
					fmpq_neg(a, a); \
				fmpz_set_si(p, 10); \
				if (clog <= digits) { \
				if (clog >= 0) \
				fmpz_pow_ui(p, p, (ulong) (digits - clog)); \
				else \
				fmpz_pow_ui(p, p, (ulong) digits + ((ulong) -1 - (ulong) clog + 1)); \
				fmpz_mul_si(q, p, 2); \
				arf_one(s); \
				arf_div_fmpz(s, s, q, MAG_BITS << 1, ARF_RND_UP); \
				arf_get_mag(d, s); \
				fmpz_mul(fmpq_numref(a), fmpq_numref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				arf_fmpz_div_fmpz(zm, q, p, prec, rnd); \
				} else { \
				fmpz_pow_ui(p, p, (ulong) (clog - digits)); \
				fmpz_divexact_si(q, p, 2); \
				mag_set_fmpz(d, q); \
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p); \
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a)); \
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 && \
				    fmpz_is_odd(q)) \
					fmpz_add_si(q, q, fmpz_sgn(r)); \
				fmpz_mul(q, q, p); \
				arf_set_fmpz(zm, q); \
				} \
				if (arf_equal(xm, zm) != 0) \
				mag_set(zr, xr); \
				else \
				mag_add(zr, xr, d); \
				} \
				} while (0)
				TEMPLATE(acb_realref);
				TEMPLATE(acb_imagref);
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
	case 53: /*     "sum" */
	case 54: /*    "prod" */
	case 55: /*    "mean" */
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];
		nz = (op == 52) ? 2 : 1;
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = flint_calloc(nz, sizeof(acb_t));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		switch (op) {
		case 53: /*     "sum" */
			acb_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ACB_CONTAINS_NAN(x + jx)))
				acb_add(z, z, x + jx, prec);
			break;
		case 54: /*    "prod" */
			acb_one(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ACB_CONTAINS_NAN(x + jx)))
				acb_mul(z, z, x + jx, prec);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = nx;
			acb_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ACB_CONTAINS_NAN(x + jx)))
				acb_add(z, z, x + jx, prec);
				else
				--c;
			if (c == 0)
			acb_indeterminate(z);
			else
			acb_div_ui(z, z, c, prec);
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
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(acb_realref(x + jx))) ||
					arf_is_nan(arb_midref(acb_imagref(x + jx))))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(acb_realref(x + jx)), arb_radref(acb_realref(x + jx))) >  0 ||
						 arf_cmpabs_mag(arb_midref(acb_imagref(x + jx)), arb_radref(acb_imagref(x + jx))) >  0)
					break;
			z[0] = (jx < nx) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*     "all" */
			/* Return 1 if and only if all do   not contain zero */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(acb_realref(x + jx))) ||
					arf_is_nan(arb_midref(acb_imagref(x + jx))))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(acb_realref(x + jx)), arb_radref(acb_realref(x + jx))) <= 0 ||
						 arf_cmpabs_mag(arb_midref(acb_imagref(x + jx)), arb_radref(acb_imagref(x + jx))) <= 0)
					break;
			z[0] = (jx < nx) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*   "anyNA" */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(acb_realref(x + jx))) ||
				    arf_is_nan(arb_midref(acb_imagref(x + jx))))
					break;
			z[0] = jx < nx;
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
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case  3: /*       "is.na" */
		case  4: /*      "is.nan" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
					arf_is_nan(arb_midref(acb_realref(x + jz))) != 0 ||
					arf_is_nan(arb_midref(acb_imagref(x + jz))) != 0;
			break;
		case  5: /* "is.infinite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
					arf_is_inf(arb_midref(acb_realref(x + jz))) != 0 ||
					mag_is_inf(arb_radref(acb_realref(x + jz))) != 0 ||
					arf_is_inf(arb_midref(acb_imagref(x + jz))) != 0 ||
					mag_is_inf(arb_radref(acb_imagref(x + jz))) != 0;
			break;
		case  6: /*   "is.finite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
					arf_is_finite(arb_midref(acb_realref(x + jz))) != 0 &&
					mag_is_finite(arb_radref(acb_realref(x + jz))) != 0 &&
					arf_is_finite(arb_midref(acb_imagref(x + jz))) != 0 &&
					mag_is_finite(arb_radref(acb_imagref(x + jz))) != 0;
			break;
		case  7: /*           "!" */
			for (jz = 0; jz < nz; ++jz)
				if (arf_is_nan(arb_midref(acb_imagref(x + jz))) ||
				    arf_is_nan(arb_midref(acb_imagref(x + jz))))
				z[jz] = NA_LOGICAL;
				else
				z[jz] =
					arf_is_zero(arb_midref(acb_realref(x + jz))) != 0 &&
					mag_is_zero(arb_radref(acb_realref(x + jz))) != 0 &&
					arf_is_zero(arb_midref(acb_imagref(x + jz))) != 0 &&
					mag_is_zero(arb_radref(acb_imagref(x + jz))) != 0;
			break;
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case  9: /*       "Re" */
	case 10: /*       "Im" */
	case 11: /*      "Mod" */
	case 12: /*      "Arg" */
	case 13: /*      "abs" */
	{
		SEXP ans = PROTECT(newObject("arb"));
		arb_ptr z = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arb_finalize);
		switch (op) {
		case  9: /*       "Re" */
			for (jz = 0; jz < nz; ++jz)
				arb_set(z + jz, acb_realref(x + jz));
			break;
		case 10: /*       "Im" */
			for (jz = 0; jz < nz; ++jz)
				arb_set(z + jz, acb_imagref(x + jz));
			break;
		case 11: /*      "Mod" */
		case 13: /*      "abs" */
			for (jz = 0; jz < nz; ++jz)
				acb_abs(z + jz, x + jz, prec);
			break;
		case 12: /*      "Arg" */
			for (jz = 0; jz < nz; ++jz)
				acb_arg(z + jz, x + jz, prec);
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

		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		jx = 0;
		if (byrow) {
			mp_limb_t *c = 0;
			if (domean && nz) {
				c = (void *) R_alloc(nz, sizeof(mp_limb_t));
				memset(c, 0, nz * sizeof(mp_limb_t));
			}
			for (jz = 0; jz < nz; ++jz)
				acb_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					if (!(narm && ACB_CONTAINS_NAN(z + jz)))
						acb_add(z + jz, z + jz, x + jx, prec);
					else if (domean)
						++c[jz];
			if (domean) {
			for (jz = 0; jz < nz; ++jz)
				if (c[jz] == nt)
					acb_indeterminate(z + jz);
				else
					acb_div_ui(z + jz, z + jz, nt - c[jz], prec);
			}
		} else {
			mp_limb_t c = 0;
			for (jz = 0; jz < nz; ++jz) {
				acb_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					if (!(narm && ACB_CONTAINS_NAN(x + jx)))
						acb_add(z + jz, z + jz, x + jx, prec);
					else if (domean)
						++c;
				if (domean) {
					if (c == nt)
						acb_indeterminate(z + jz);
					else
						acb_div_ui(z + jz, z + jz, nt - c, prec);
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
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		int i, j, singular;
		mp_limb_t ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		if (op == 64 || op == 65)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				acb_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j <= i; ++j, ++ja)
					acb_set(ma->entries + ja, x + ja);
				for (; j < ma->c; ++j, ++ja)
					acb_zero(ma->entries + ja);
			}
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					acb_zero(ma->entries + ja);
				for (; j < ma->c; ++j, ++ja)
					acb_set(ma->entries + ja, x + ja);
			}
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					acb_zero(ma->entries + ja);
				jx = ja;
				for (; j < ma->c; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
			}
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				jx = ja;
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					acb_set(ma->entries + ja, x + jx);
				for (; j < ma->c; ++j, ++ja)
					acb_zero(ma->entries + ja);
			}
			break;
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
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
		singular = !acb_mat_inv(mc, ma, prec);
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		if (singular)
			Rf_error(_("matrix is exactly singular or precision is insufficient"));
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
#ifdef HAVE_ACB_MAT_INV_CHO_PRECOMP
	case 67: /*   "chol2inv" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		int i;
		mp_limb_t jc, ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		for (ja = 0; ja < nx; ++ja)
			acb_conj(ma->entries + ja, x + ja);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
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
		acb_mat_inv_cho_precomp(mc, ma, prec);
		for (jc = 0; jc < nz; ++jc)
			acb_conj(mc->entries + jc, mc->entries + jc);
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = PROTECT(R_do_slot(s_x, R_flint_symbol_dimnames)),
			dimnamesz = R_NilValue;
		SEXP namesdimnamesx = PROTECT(Rf_getAttrib(dimnamesx, R_NamesSymbol)),
			namesdimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue &&
		    (VECTOR_ELT(dimnamesx, 1) != R_NilValue || namesdimnamesx != R_NilValue)) {
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 0, VECTOR_ELT(dimnamesx, 1));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 0, STRING_ELT(namesdimnamesx, 1));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 1));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(1);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(1);
		}
		UNPROTECT(4);
		return ans;
	}
#endif
#ifdef HAVE_ACB_MAT_CHO
	case 68: /*       "chol" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = (nz) ? flint_calloc(nz, sizeof(acb_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_acb_finalize);
		int i, posdef;
		mp_limb_t jc, ja;
		acb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(acb_t)) : 0;
		for (ja = 0; ja < nx; ++ja)
			acb_conj(ma->entries + ja, x + ja);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(acb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(acb_ptr)) : 0;
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
		posdef = acb_mat_cho(mc, ma, prec);
		for (jc = 0; jc < nz; ++jc)
			acb_conj(mc->entries + jc, mc->entries + jc);
		for (ja = 0; ja < nx; ++ja)
			acb_clear(ma->entries + ja);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		if (!posdef)
			Rf_error(_("matrix is not positive definite or precision is insufficient"));
		R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
		SEXP dimnamesx = PROTECT(R_do_slot(s_x, R_flint_symbol_dimnames)),
			dimnamesz = R_NilValue;
		SEXP namesdimnamesx = PROTECT(Rf_getAttrib(dimnamesx, R_NamesSymbol)),
			namesdimnamesz = R_NilValue;
		if (dimnamesx != R_NilValue &&
		    (VECTOR_ELT(dimnamesx, 1) != R_NilValue || namesdimnamesx != R_NilValue)) {
			PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
			SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
			if (namesdimnamesx != R_NilValue) {
				PROTECT(namesdimnamesz = Rf_allocVector(STRSXP, 2));
				SET_STRING_ELT(namesdimnamesz, 1, STRING_ELT(namesdimnamesx, 1));
				Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesz);
				UNPROTECT(1);
			}
			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
			UNPROTECT(1);
		}
		UNPROTECT(4);
		return ans;
	}
#endif
	case 69: /*        "det" */
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("acb"));
		acb_ptr z = flint_calloc(1, sizeof(acb_t));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_acb_finalize);
		acb_mat_t mx;
		mx->r = mx->c = dx[0];
		mx->entries = (void *) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(acb_ptr)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		acb_mat_det(z, mx, prec);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mx->rows);
#endif
		UNPROTECT(2);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "acb");
		return R_NilValue;
	}
}
