#include "flint.h"

void R_flint_arb_finalize(SEXP x)
{
	arb_ptr p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		arb_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_arb_initialize(SEXP object, SEXP s_x, SEXP s_length,
                            SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                            SEXP s_mid, SEXP s_rad,
                            SEXP s_prec)
{
	mp_limb_t jx, jy, jm, jr, nx = 0, ny = 0, nm = 1, nr = 1;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	int exact = s_prec == R_NilValue;
	slong prec = asPrec(s_prec, __func__);
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
				FOR_RECYCLE2(jy, ny, jm, nm, jr, nr) {
					arf_set(arb_midref(y + jy), xm + jm);
					mag_set(arb_radref(y + jy), xr + jr);
				}
			} else {
				FOR_RECYCLE1(jy, ny, jm, nm) {
					arf_set(arb_midref(y + jy), xm + jm);
					mag_zero(arb_radref(y + jy));
				}
			}
		} else {
			if (s_rad != R_NilValue) {
				mag_srcptr xr = R_flint_get_pointer(s_rad);
				FOR_RECYCLE1(jy, ny, jr, nr) {
					arf_zero(arb_midref(y + jy));
					mag_set(arb_radref(y + jy), xr + jr);
				}
			}
		}
	} else {
		int seenimag = 0;
		switch (TYPEOF(s_x)) {
		case NILSXP:
			FOR_RECYCLE0(jy, ny)
				arb_zero(y + jy);
			break;
		case RAWSXP:
		{
			const Rbyte *x = RAW_RO(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				if (exact)
				arb_set_ui      (y + jy, x[jx]);
				else
				arb_set_round_ui(y + jy, x[jx], prec);
			break;
		}
		case LGLSXP:
		{
			const int *x = LOGICAL_RO(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				if (x[jx] == NA_LOGICAL)
				arb_set_d(y + jy, R_NaN);
				else if (exact)
				arb_set_si      (y + jy, x[jx] != 0);
				else
				arb_set_round_si(y + jy, x[jx] != 0, prec);
			break;
		}
		case INTSXP:
		{
			const int *x = INTEGER_RO(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				if (x[jx] == NA_INTEGER)
				arb_set_d(y + jy, R_NaN);
				else if (exact)
				arb_set_si      (y + jy, x[jx]);
				else
				arb_set_round_si(y + jy, x[jx], prec);
			break;
		}
		case REALSXP:
		{
			const double *x = REAL_RO(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				arb_set_d(y + jy, x[jx]);
				if (!exact)
				arb_set_round(y + jy, y + jy, prec);
			}
			break;
		}
		case CPLXSXP:
		{
			const Rcomplex *x = COMPLEX_RO(s_x);
			int seenimag = 0;
			FOR_RECYCLE1(jy, ny, jx, nx) {
				arb_set_d(y + jy, x[jx].r);
				if (!exact)
				arb_set_round(y + jy, y + jy, prec);
				seenimag = seenimag || x[jx].i != 0.0;
			}
			break;
		}
		case STRSXP:
		{
			mpfr_prec_t prec__ = mpfrPrec(prec);
			mpfr_t m, r;
			arf_t tmp;
			mpfr_init2(m, prec__);
			mpfr_init2(r, MAG_BITS << 1);
			arf_init(tmp);
			const char *s;
			char *t;
			int negate;
			FOR_RECYCLE1(jy, ny, jx, nx) {
				s = CHAR(STRING_ELT(s_x, (R_xlen_t) jx));
				while (isspace(*s))
					s++;
				negate = *s == '-';
				if (*s == '+' || negate)
					s++;
				while (isspace(*s))
					s++;
				if (*(s++) != '(')
					break;
				mpfr_strtofr(m, s, &t, 0, MPFR_RNDZ);
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
				FOR_RECYCLE1(jy, ny, jx, nx)
					if (exact)
					arb_set_ui      (y + jy, x[jx]);
					else
					arb_set_round_ui(y + jy, x[jx], prec);
				break;
			}
			case R_FLINT_CLASS_SLONG:
			{
				const slong *x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx)
					if (exact)
					arb_set_si      (y + jy, x[jx]);
					else
					arb_set_round_si(y + jy, x[jx], prec);
				break;
			}
			case R_FLINT_CLASS_FMPZ:
			{
				const fmpz *x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx)
					if (exact)
					arb_set_fmpz      (y + jy, x + jx);
					else
					arb_set_round_fmpz(y + jy, x + jx, prec);
				break;
			}
			case R_FLINT_CLASS_FMPQ:
			{
				const fmpq *x = R_flint_get_pointer(s_x);
				slong prec = asPrec(R_NilValue, __func__);
				FOR_RECYCLE1(jy, ny, jx, nx)
					arb_fmpz_div_fmpz(y + jy, fmpq_numref(x + jx), fmpq_denref(x + jx), prec);
				break;
			}
			case R_FLINT_CLASS_MAG:
			{
				mag_srcptr x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx) {
					arb_set_mag(y + jy, x + jx);
					if (!exact)
					arb_set_round(y + jy, y + jy, prec);
				}
				break;
			}
			case R_FLINT_CLASS_ARF:
			{
				arf_srcptr x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx) {
					arb_set_arf(y + jy, x + jx);
					if (!exact)
					arb_set_round(y + jy, y + jy, prec);
				}
				break;
			}
			case R_FLINT_CLASS_ACF:
			{
				acf_srcptr x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx) {
					arb_set_arf(y + jy, acf_realref(x + jx));
					if (!exact)
					arb_set_round(y + jy, y + jy, prec);
					seenimag = seenimag || !arf_is_zero(acf_imagref(x + jx));
				}
				break;
			}
			case R_FLINT_CLASS_ARB:
			{
				arb_srcptr x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx)
					if (exact)
					arb_set      (y + jy, x + jx);
					else
					arb_set_round(y + jy, x + jx, prec);
				break;
			}
			case R_FLINT_CLASS_ACB:
			{
				acb_srcptr x = R_flint_get_pointer(s_x);
				FOR_RECYCLE1(jy, ny, jx, nx) {
					if (exact)
					arb_set      (y + jy, acb_realref(x + jx));
					else
					arb_set_round(y + jy, acb_realref(x + jx), prec);
					seenimag = seenimag || !arb_is_zero(acb_imagref(x + jx));
				}
				break;
			}
			case R_FLINT_CLASS_INVALID:
				Rf_error(_("foreign external pointer"));
				break;
			}
			break;
		}
		if (seenimag) WARNING_LOST_IMAG;
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
	SEXP ans;
	if (op == 0) {
		PROTECT(ans = newFlint(R_FLINT_CLASS_ARF, 0, n));
		arf_ptr y = R_flint_get_pointer(ans);
		for (j = 0; j < n; ++j)
			arf_set(y + j, arb_midref(x + j));
	} else {
		PROTECT(ans = newFlint(R_FLINT_CLASS_MAG, 0, n));
		mag_ptr y = R_flint_get_pointer(ans);
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
	arf_rnd_t rnd = asRnd(R_NilValue, 1, __func__);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arb_srcptr x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	int seenoob = 0, seenrad = 0;
	arf_t lb, ub;
	arf_srcptr p;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	for (j = 0; j < n; ++j) {
		p = arb_midref(x + j);
		if (arf_is_nan(p))
			y[j] = R_NaN;
		else if (arf_cmp(p, lb) >= 0 && arf_cmp(p, ub) <= 0)
			y[j] = arf_get_d(p, rnd);
		else {
			y[j] = (arf_sgn(p) < 0) ? R_NegInf : R_PosInf;
			seenoob = 1;
		}
		seenrad = seenrad || !arb_is_exact(x + j);
	}
	arf_clear(lb);
	arf_clear(ub);
	if (seenoob) WARNING_OOB_DOUBLE;
	if (seenrad) WARNING_LOST_RAD;
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arb_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	R_flint_ops2_t op = ops2match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops2info(op);
	mp_limb_t jx, jy, jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	arb_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	int dz[3];
	info = checkConformable(s_x, s_y, nx, ny, info, dz);
	if (info >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
	slong prec = asPrec(R_NilValue, __func__);
	switch (op) {
	case R_FLINT_OPS2_ADD:
	case R_FLINT_OPS2_SUB:
	case R_FLINT_OPS2_MUL:
	case R_FLINT_OPS2_FDR:
	case R_FLINT_OPS2_FDQ:
	case R_FLINT_OPS2_DIV:
	case R_FLINT_OPS2_POW:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS2_ADD:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_add(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_SUB:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_sub(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_MUL:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_mul(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_FDR:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_fdiv_r(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_FDQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_fdiv_q(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_DIV:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_div(z + jz, x + jx, y + jy, prec);
			break;
		case R_FLINT_OPS2_POW:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				arb_pow(z + jz, x + jx, y + jy, prec);
			break;
		default: /* -Wswitch */
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS2_EQ:
	case R_FLINT_OPS2_NEQ:
	case R_FLINT_OPS2_L:
	case R_FLINT_OPS2_G:
	case R_FLINT_OPS2_LEQ:
	case R_FLINT_OPS2_GEQ:
	case R_FLINT_OPS2_AND:
	case R_FLINT_OPS2_OR:
	{
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case R_FLINT_OPS2_EQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_eq(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_NEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_ne(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_L:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_lt(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_G:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_gt(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_LEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_le(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_GEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: arb_ge(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_AND:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_ZERO(x + jx) ||
				 ARB_CONTAINS_ZERO(y + jy))
				? 0
				:
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: 1;
			break;
		case R_FLINT_OPS2_OR:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] =
				(ARB_CONTAINS_NONZERO(x + jx) ||
				 ARB_CONTAINS_NONZERO(y + jy))
				? 1
				:
				(ARB_CONTAINS_NAN(x + jx) ||
				 ARB_CONTAINS_NAN(y + jy))
				? NA_LOGICAL
				: 0;
			break;
		default: /* -Wswitch */
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS2_PROD:
	case R_FLINT_OPS2_CROSSPROD:
	case R_FLINT_OPS2_TCROSSPROD:
	{
		/* C = A B                            */
		/*                                    */
		/*        %*%: C = Z', A = Y', B = X' */
		/*  crossprod: C = Z', A = Y', B = X  */
		/* tcrossprod: C = Z', A = Y , B = X' */
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		int tx = (info & 1) != 0, ty = (info & 2) != 0, i, j;
		mp_limb_t jx, jy, ja, jb;
		arb_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(arb_t)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(arb_t)) : 0) : (void *) x;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, y + jy);
		}
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += (mp_limb_t) mb->r)
					arb_set(mb->entries + jb, x + jx);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(arb_ptr)) : 0;
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
		arb_mat_mul(mc, ma, mb, prec);
		if (ty) {
			for (ja = 0; ja < ny; ++ja)
				arb_clear(ma->entries + ja);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jb = 0; jb < nx; ++jb)
				arb_clear(mb->entries + jb);
			flint_free(mb->entries);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS2_SOLVE:
	case R_FLINT_OPS2_BACKSOLVE:
	case R_FLINT_OPS2_TBACKSOLVE:
	{
		/* A C = B                          */
		/*                                  */
		/*      solve: C = Z, A = X , B = Y */
		/*  backsolve: C = Z, A = X , B = Y */
		/* tbacksolve: C = Z, A = X', B = Y */
		int uplo = 'N';
		if (op != R_FLINT_OPS2_SOLVE) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		arb_mat_t mc, ma, mb;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(arb_t)) : 0;
		if (op == R_FLINT_OPS2_TBACKSOLVE)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				arb_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->r - (++i)))
				for (j = 0; j <= i; ++j, ++ja)
					arb_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					arb_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->c - (++i)), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += (mp_limb_t) mb->r)
				arb_set(mb->entries + jb, y + jy);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(arb_ptr)) : 0;
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
		if (uplo == 'N')
			singular = !arb_mat_approx_solve(mc, ma, mb, prec);
		else if ((uplo == 'U') == (op != R_FLINT_OPS2_TBACKSOLVE)) {
			arb_mat_solve_triu(mc, ma, mb, 0, prec);
			singular = 0;
		}
		else {
			arb_mat_solve_tril(mc, ma, mb, 0, prec);
			singular = 0;
		}
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += (mp_limb_t) mc->c) {
				arb_set(z + jz, mc->entries + jc);
				arb_clear(mc->entries + jc);
			}
		for (ja = 0; ja < nx; ++ja)
			arb_clear(ma->entries + ja);
		for (jb = 0; jb < ny; ++jb)
			arb_clear(mb->entries + jb);
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
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}

SEXP R_flint_arb_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	R_flint_ops1_t op = ops1match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops1info(op);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	arb_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = asRnd(R_NilValue, 1, __func__);
	switch (op) {
	case R_FLINT_OPS1_PLUS:
	case R_FLINT_OPS1_MINUS:
	case R_FLINT_OPS1_CONJ:
	case R_FLINT_OPS1_REAL:
	case R_FLINT_OPS1_IMAG:
	case R_FLINT_OPS1_MOD:
	case R_FLINT_OPS1_ARG:
	case R_FLINT_OPS1_ABS:
	case R_FLINT_OPS1_SIGN:
	case R_FLINT_OPS1_SQRT:
	case R_FLINT_OPS1_FLOOR:
	case R_FLINT_OPS1_CEILING:
	case R_FLINT_OPS1_TRUNC:
	case R_FLINT_OPS1_CUMMIN:
	case R_FLINT_OPS1_CUMMAX:
	case R_FLINT_OPS1_CUMSUM:
	case R_FLINT_OPS1_CUMPROD:
	case R_FLINT_OPS1_LOG:
	case R_FLINT_OPS1_LOG10:
	case R_FLINT_OPS1_LOG2:
	case R_FLINT_OPS1_LOG1P:
	case R_FLINT_OPS1_EXP:
	case R_FLINT_OPS1_EXPM1:
	case R_FLINT_OPS1_COS:
	case R_FLINT_OPS1_COSPI:
	case R_FLINT_OPS1_ACOS:
	case R_FLINT_OPS1_COSH:
	case R_FLINT_OPS1_ACOSH:
	case R_FLINT_OPS1_SIN:
	case R_FLINT_OPS1_SINPI:
	case R_FLINT_OPS1_ASIN:
	case R_FLINT_OPS1_SINH:
	case R_FLINT_OPS1_ASINH:
	case R_FLINT_OPS1_TAN:
	case R_FLINT_OPS1_TANPI:
	case R_FLINT_OPS1_ATAN:
	case R_FLINT_OPS1_TANH:
	case R_FLINT_OPS1_ATANH:
	case R_FLINT_OPS1_GAMMA:
	case R_FLINT_OPS1_LGAMMA:
	case R_FLINT_OPS1_2GAMMA:
	case R_FLINT_OPS1_3GAMMA:
	case R_FLINT_OPS1_ROUND:
	case R_FLINT_OPS1_SIGNIF:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_PLUS:
		case R_FLINT_OPS1_CONJ:
		case R_FLINT_OPS1_REAL:
			for (jz = 0; jz < nz; ++jz)
				arb_set(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_MINUS:
			for (jz = 0; jz < nz; ++jz)
				arb_neg(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_IMAG:
			for (jz = 0; jz < nz; ++jz)
				arb_zero(z + jz);
			break;
		case R_FLINT_OPS1_MOD:
		case R_FLINT_OPS1_ABS:
			for (jz = 0; jz < nz; ++jz)
				arb_nonnegative_abs(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ARG:
			for (jz = 0; jz < nz; ++jz)
				arb_arg(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_SIGN:
			for (jz = 0; jz < nz; ++jz)
				arb_sgn(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_SQRT:
			for (jz = 0; jz < nz; ++jz)
				arb_sqrt(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_FLOOR:
			for (jz = 0; jz < nz; ++jz)
				arb_floor(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_CEILING:
			for (jz = 0; jz < nz; ++jz)
				arb_ceil(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_TRUNC:
			for (jz = 0; jz < nz; ++jz)
				arb_trunc(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_CUMMIN:
			if (nz) {
			arb_srcptr last = x;
			for (jz = 0; jz < nz && !ARB_CONTAINS_NAN(x + jz); ++jz)
				arb_min(z + jz, last, x + jz, prec);
			for (; jz < nz; ++jz)
				arb_indeterminate(z + jz);
			}
			break;
		case R_FLINT_OPS1_CUMMAX:
			if (nz) {
			arb_srcptr last = x;
			for (jz = 0; jz < nz && !ARB_CONTAINS_NAN(x + jz); ++jz)
				arb_max(z + jz, last, x + jz, prec);
			for (; jz < nz; ++jz)
				arb_indeterminate(z + jz);
			}
			break;
		case R_FLINT_OPS1_CUMSUM:
			if (nz) {
			arb_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				arb_add(z + jz, z + jz - 1, x + jz, prec);
			}
			break;
		case R_FLINT_OPS1_CUMPROD:
			if (nz)
			arb_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				arb_mul(z + jz, z + jz - 1, x + jz, prec);
			break;
		case R_FLINT_OPS1_LOG:
			if (s_dots == R_NilValue)
			for (jz = 0; jz < nz; ++jz)
				arb_log(z + jz, x + jz, prec);
			else {
			SEXP s_base = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_base) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "base", CHAR(STRING_ELT(s_op, 0)));
			arb_srcptr base = R_flint_get_pointer(s_base);
			for (jz = 0; jz < nz; ++jz)
				arb_log_base(z + jz, x + jz, base, prec);
			}
			break;
		case R_FLINT_OPS1_LOG10:
			for (jz = 0; jz < nz; ++jz)
				arb_log_base_ui(z + jz, x + jz, 10, prec);
			break;
		case R_FLINT_OPS1_LOG2:
			for (jz = 0; jz < nz; ++jz)
				arb_log_base_ui(z + jz, x + jz, 2, prec);
			break;
		case R_FLINT_OPS1_LOG1P:
			for (jz = 0; jz < nz; ++jz)
				arb_log1p(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_EXP:
			for (jz = 0; jz < nz; ++jz)
				arb_exp(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_EXPM1:
			for (jz = 0; jz < nz; ++jz)
				arb_expm1(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_COS:
			for (jz = 0; jz < nz; ++jz)
				arb_cos(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_COSPI:
			for (jz = 0; jz < nz; ++jz)
				arb_cos_pi(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ACOS:
			for (jz = 0; jz < nz; ++jz)
				arb_acos(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_COSH:
			for (jz = 0; jz < nz; ++jz)
				arb_cosh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ACOSH:
			for (jz = 0; jz < nz; ++jz)
				arb_acosh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_SIN:
			for (jz = 0; jz < nz; ++jz)
				arb_sin(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_SINPI:
			for (jz = 0; jz < nz; ++jz)
				arb_sin_pi(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ASIN:
			for (jz = 0; jz < nz; ++jz)
				arb_asin(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_SINH:
			for (jz = 0; jz < nz; ++jz)
				arb_sinh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ASINH:
			for (jz = 0; jz < nz; ++jz)
				arb_asinh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_TAN:
			for (jz = 0; jz < nz; ++jz)
				arb_tan(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_TANPI:
			for (jz = 0; jz < nz; ++jz)
				arb_tan_pi(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ATAN:
			for (jz = 0; jz < nz; ++jz)
				arb_atan(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_TANH:
			for (jz = 0; jz < nz; ++jz)
				arb_tanh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_ATANH:
			for (jz = 0; jz < nz; ++jz)
				arb_atanh(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_GAMMA:
			for (jz = 0; jz < nz; ++jz)
				arb_gamma(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_LGAMMA:
			for (jz = 0; jz < nz; ++jz)
				arb_lgamma(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_2GAMMA:
			for (jz = 0; jz < nz; ++jz)
				arb_digamma(z + jz, x + jz, prec);
			break;
		case R_FLINT_OPS1_3GAMMA:
		{
			arb_t s;
			arb_init(s);
			arb_set_si(s, 1);
			for (jz = 0; jz < nz; ++jz)
				arb_polygamma(z + jz, s, x + jz, prec);
			arb_clear(s);
			break;
		}
		case R_FLINT_OPS1_ROUND:
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
				xm = arb_midref(x + jz);
				xr = arb_radref(x + jz);
				zm = arb_midref(z + jz);
				zr = arb_radref(z + jz);
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
			for (jz = 0; jz < nz; ++jz) {
				xm = arb_midref(x + jz);
				xr = arb_radref(x + jz);
				zm = arb_midref(z + jz);
				zr = arb_radref(z + jz);
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
		case R_FLINT_OPS1_SIGNIF:
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
				xm = arb_midref(x + jz);
				xr = arb_radref(x + jz);
				zm = arb_midref(z + jz);
				zr = arb_radref(z + jz);
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
		default: /* -Wswitch */
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_MIN:
	case R_FLINT_OPS1_MAX:
	case R_FLINT_OPS1_RANGE:
	case R_FLINT_OPS1_SUM:
	case R_FLINT_OPS1_PROD:
	case R_FLINT_OPS1_MEAN:
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0];
		nz = (op == R_FLINT_OPS1_RANGE) ? 2 : 1;
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_MIN:
			arb_pos_inf(z);
			for (jx = 0; jx < nx; ++jx)
				if (!ARB_CONTAINS_NAN(x + jx))
					arb_min(z, z, x + jx, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case R_FLINT_OPS1_MAX:
			arb_neg_inf(z);
			for (jx = 0; jx < nx; ++jx)
				if (!ARB_CONTAINS_NAN(x + jx))
					arb_max(z, z, x + jx, prec);
				else if (!narm) {
					arb_indeterminate(z);
					break;
				}
			break;
		case R_FLINT_OPS1_RANGE:
			arb_pos_inf(z);
			arb_neg_inf(z + 1);
			for (jx = 0; jx < nx; ++jx)
				if (!ARB_CONTAINS_NAN(x + jx)) {
					arb_min(z, z, x + jx, prec);
					arb_max(z + 1, z + 1, x + jx, prec);
				} else if (!narm) {
					arb_indeterminate(z);
					arb_indeterminate(z + 1);
					break;
				}
			break;
		case R_FLINT_OPS1_SUM:
			arb_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ARB_CONTAINS_NAN(x + jx)))
				arb_add(z, z, x + jx, prec);
			break;
		case R_FLINT_OPS1_PROD:
			arb_one(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ARB_CONTAINS_NAN(x + jx)))
				arb_mul(z, z, x + jx, prec);
			break;
		case R_FLINT_OPS1_MEAN:
		{
			mp_limb_t c = nx;
			arb_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && ARB_CONTAINS_NAN(x + jx)))
				arb_add(z, z, x + jx, prec);
				else
				--c;
			if (c == 0)
			arb_indeterminate(z);
			else
			arb_div_ui(z, z, c, prec);
			break;
		}
		default: /* -Wswitch */
		}
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_ANY:
	case R_FLINT_OPS1_ALL:
	case R_FLINT_OPS1_ANYNA:
	{
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0], anyna = 0;
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case R_FLINT_OPS1_ANY:
			/* Return 1 if and only if any does not contain zero */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(x + jx)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + jx), arb_radref(x + jx)) >  0)
					break;
			z[0] = (jx < nx) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case R_FLINT_OPS1_ALL:
			/* Return 1 if and only if all do   not contain zero */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(x + jx)))
					anyna = 1;
				else if (arf_cmpabs_mag(arb_midref(x + jx), arb_radref(x + jx)) <= 0)
					break;
			z[0] = (jx < nx) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case R_FLINT_OPS1_ANYNA:
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(arb_midref(x + jx)))
					break;
			z[0] = jx < nx;
			break;
		default: /* -Wswitch */
		}
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_ISNA:
	case R_FLINT_OPS1_ISNAN:
	case R_FLINT_OPS1_ISINF:
	case R_FLINT_OPS1_ISNUM:
	case R_FLINT_OPS1_NOT:
	{
		ERROR_TOO_LONG(nz, R_XLEN_T_MAX);
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t) nz));
		int *z = LOGICAL(ans);
		switch (op) {
		case R_FLINT_OPS1_ISNA:
		case R_FLINT_OPS1_ISNAN:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = arf_is_nan(arb_midref(x + jz)) != 0;
			break;
		case R_FLINT_OPS1_ISINF:
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
					arf_is_inf(arb_midref(x + jz)) != 0 ||
					mag_is_inf(arb_radref(x + jz)) != 0;
			break;
		case R_FLINT_OPS1_ISNUM:
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
					arf_is_finite(arb_midref(x + jz)) != 0 &&
					mag_is_finite(arb_radref(x + jz)) != 0;
			break;
		case R_FLINT_OPS1_NOT:
			for (jz = 0; jz < nz; ++jz)
				if (arf_is_nan(arb_midref(x + jz)))
				z[jz] = NA_LOGICAL;
				else
				z[jz] =
					arf_is_zero(arb_midref(x + jz)) != 0 &&
					mag_is_zero(arb_radref(x + jz)) != 0;
			break;
		default: /* -Wswitch */
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_COLSUM:
	case R_FLINT_OPS1_ROWSUM:
	case R_FLINT_OPS1_COLMEAN:
	case R_FLINT_OPS1_ROWMEAN:
	{
		int byrow = (info & 1) != 0, domean = (info & 2) != 0;

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

		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		jx = 0;
		if (byrow) {
			mp_limb_t *c = 0;
			if (domean && nz) {
				c = (void *) R_alloc(nz, sizeof(mp_limb_t));
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
	case R_FLINT_OPS1_SOLVE:
	case R_FLINT_OPS1_BACKSOLVE:
	case R_FLINT_OPS1_TBACKSOLVE:
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
			Rf_error(_("argument is not a square matrix"));
		int uplo = 'N';
		if (op != R_FLINT_OPS1_SOLVE) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		int i, j, singular;
		mp_limb_t ja;
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		if (op != R_FLINT_OPS1_TBACKSOLVE)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				arb_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j <= i; ++j, ++ja)
					arb_set(ma->entries + ja, x + ja);
				for (; j < ma->c; ++j, ++ja)
					arb_zero(ma->entries + ja);
			}
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					arb_zero(ma->entries + ja);
				for (; j < ma->c; ++j, ++ja)
					arb_set(ma->entries + ja, x + ja);
			}
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					arb_zero(ma->entries + ja);
				jx = ja;
				for (; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
			}
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				jx = ja;
				for (j = 0; j <= i; ++j, ++ja, jx += (mp_limb_t) ma->r)
					arb_set(ma->entries + ja, x + jx);
				for (; j < ma->c; ++j, ++ja)
					arb_zero(ma->entries + ja);
			}
			break;
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
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
		singular = !arb_mat_inv(mc, ma, prec);
		for (ja = 0; ja < nx; ++ja)
			arb_clear(ma->entries + ja);
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
	case R_FLINT_OPS1_CHOL2INV:
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (void *) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
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
		arb_mat_inv_cho_precomp(mc, ma, prec);
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
	case R_FLINT_OPS1_CHOL:
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, nz));
		arb_ptr z = R_flint_get_pointer(ans);
		int posdef;
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = z;
		ma->entries = (arb_ptr) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(arb_ptr)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(arb_ptr)) : 0;
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
		posdef = arb_mat_cho(mc, ma, prec);
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
	case R_FLINT_OPS1_DET:
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, 1));
		arb_ptr z = R_flint_get_pointer(ans);
		arb_mat_t mx;
		mx->r = mx->c = dx[0];
		mx->entries = (void *) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(arb_ptr)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		arb_mat_det(z, mx, prec);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mx->rows);
#endif
		UNPROTECT(2);
		return ans;
	}
	case R_FLINT_OPS1_DIFF:
	{
		SEXP s_lag = VECTOR_ELT(s_dots, 0),
			s_ord = VECTOR_ELT(s_dots, 1);
		if (XLENGTH(s_lag) == 0 || (INTEGER(s_lag)[0]) <= 0)
			Rf_error(_("'%s' is not a positive integer"),
			         "lag");
		if (XLENGTH(s_ord) == 0 || (INTEGER(s_ord)[0]) <= 0)
			Rf_error(_("'%s' is not a positive integer"),
			         "differences");
		mp_limb_t i, j, k, m, n, r, r__,
			lag = (mp_limb_t) INTEGER(s_lag)[0],
			ord = (mp_limb_t) INTEGER(s_ord)[0];
		SEXP dimx = R_do_slot(s_x, R_flint_symbol_dim);
		int ismx = dimx != R_NilValue && XLENGTH(dimx) == 2;
		if (ismx) {
			m = (mp_limb_t) INTEGER(dimx)[0];
			n = (mp_limb_t) INTEGER(dimx)[1];
		} else {
			m = nx;
			n = 1;
		}
		r = (lag >= m / ord) ? m : lag * ord;
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, (m - r) * n));
		if (r < m) {
			SEXP work = newFlint(R_FLINT_CLASS_ARB, 0, m);
			arb_ptr z = R_flint_get_pointer(ans),
				w = R_flint_get_pointer(work);
			for (j = 0; j < n; ++j) {
				for (i = 0; i < m; ++i)
					arb_set(w + i, x + i);
				r__ = 0;
				for (k = 0; k < ord; ++k) {
					r__ += lag;
					for (i = m - 1; i >= r__; --i)
						arb_sub(w + i, w + i, w + i - lag, prec);
				}
				for (i = r; i < m; ++i)
					arb_set(z + i - r, w + i);
				x += m;
				z += m - r;
			}
		}
		if (ismx) {
			SEXP dimz = PROTECT(Rf_allocVector(INTSXP, 2));
			INTEGER(dimz)[0] = (int) (m - r);
			INTEGER(dimz)[1] = (int) n;
			R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
			UNPROTECT(1);
			SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
				dimnamesz = R_NilValue;
			if (dimnamesx != R_NilValue) {
				PROTECT(dimnamesx);
				PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
				SEXP rownamesx = VECTOR_ELT(dimnamesx, 0),
					rownamesz = R_NilValue;
				if (rownamesx != R_NilValue) {
					PROTECT(rownamesx);
					PROTECT(rownamesz = Rf_allocVector(STRSXP, (int) (m - r)));
					for (i = r; i < m; ++i)
						SET_STRING_ELT(rownamesz, (int) (i - r),
						               STRING_ELT(rownamesx, (int) i));
					SET_VECTOR_ELT(dimnamesz, 0, rownamesz);
					UNPROTECT(2);
				}
				SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
				SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol);
				if (namesdimnamesx != R_NilValue) {
					PROTECT(namesdimnamesx);
					Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesx);
					UNPROTECT(1);
				}
				R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
				UNPROTECT(2);
			}
		} else {
			SEXP namesx = R_do_slot(s_x, R_flint_symbol_names),
				namesz = R_NilValue;
			if (namesx != R_NilValue) {
				PROTECT(namesx);
				PROTECT(namesz = Rf_allocVector(STRSXP, (int) (m - r)));
				for (i = r; i < m; ++i)
					SET_STRING_ELT(namesz, (int) (i - r),
					               STRING_ELT(namesx, (int) i));
				R_do_slot_assign(ans, R_flint_symbol_names, namesz);
				UNPROTECT(2);
			}
		}
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_DIFFINV:
	{
		SEXP s_lag = VECTOR_ELT(s_dots, 0),
			s_ord = VECTOR_ELT(s_dots, 1);
		if (XLENGTH(s_lag) == 0 || (INTEGER(s_lag)[0]) <= 0)
			Rf_error(_("'%s' is not a positive integer"),
			         "lag");
		if (XLENGTH(s_ord) == 0 || (INTEGER(s_ord)[0]) <= 0)
			Rf_error(_("'%s' is not a positive integer"),
			         "differences");
		mp_limb_t i, j, k, m, n, r, r__,
			lag = (mp_limb_t) INTEGER(s_lag)[0],
			ord = (mp_limb_t) INTEGER(s_ord)[0];
		SEXP dimx = R_do_slot(s_x, R_flint_symbol_dim);
		int ismx = dimx != R_NilValue && XLENGTH(dimx) == 2;
		if (ismx) {
			m = (mp_limb_t) INTEGER(dimx)[0];
			n = (mp_limb_t) INTEGER(dimx)[1];
			if (lag > (INT_MAX - m) / ord)
				Rf_error(_("dimensions would exceed maximum %d"),
				         INT_MAX);
		} else {
			m = nx;
			n = 1;
			if (lag > (UWORD_MAX - m) / ord)
				Rf_error(_("length would exceed maximum %llu"),
				         (unsigned long long int) UWORD_MAX);
		}
		r = lag * ord;
		SEXP s_y = VECTOR_ELT(s_dots, 2);
		int usey = s_y != R_NilValue;
		if (usey) {
		if (ismx) {
			SEXP dimy = R_do_slot(s_y, R_flint_symbol_dim);
			if (dimy == R_NilValue || XLENGTH(dimy) != 2)
				Rf_error(_("'%s' is not a matrix"),
				         "xi");
			if (INTEGER(dimy)[0] != r)
				Rf_error(_("number of rows of '%s' is not equal to %s"),
				         "xi", "lag * differences");
			if (INTEGER(dimy)[1] != n)
				Rf_error(_("number of columns of '%s' is not equal to %s"),
				         "xi", "ncol(x)");
		} else {
			if (R_flint_get_length(s_y) != r)
				Rf_error(_("length of '%s' is not equal to %s"),
				         "xi", "lag * differences");
		}
		}
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, (m + r) * n));
		arb_ptr z = R_flint_get_pointer(ans);
		arb_srcptr y = (usey) ? R_flint_get_pointer(s_y) : 0;
		for (j = 0; j < n; ++j) {
			if (usey)
			for (i = 0; i < r; ++i)
				arb_set(z + i, y + i);
			else
			for (i = 0; i < r; ++i)
				arb_zero(z + i);
			for (i = 0; i < m; ++i)
				arb_set(z + r + i, x + i);
			r__ = 0;
			for (k = 0; k < ord; ++k) {
				r__ += lag;
				for (i = r - 1; i >= r__; --i)
					arb_sub(z + i, z + i, z + i - lag, prec);
			}
			r__ = r;
			for (k = 0; k < ord; ++k) {
				for (i = r__; i < m + r; ++i)
					arb_add(z + i, z + i, z + i - lag, prec);
				r__ -= lag;
			}
			x += m;
			if (usey)
			y += r;
			z += m + r;
		}
		if (ismx) {
			SEXP dimz = PROTECT(Rf_allocVector(INTSXP, 2));
			INTEGER(dimz)[0] = (int) (m + r);
			INTEGER(dimz)[1] = (int) n;
			R_do_slot_assign(ans, R_flint_symbol_dim, dimz);
			UNPROTECT(1);
			SEXP dimnamesx = R_do_slot(s_x, R_flint_symbol_dimnames),
				dimnamesy = R_NilValue,
				dimnamesz = R_NilValue;
			if (dimnamesx != R_NilValue) {
				PROTECT(dimnamesx);
				PROTECT(dimnamesy = (usey) ? R_do_slot(s_y, R_flint_symbol_dimnames) : R_NilValue);
				PROTECT(dimnamesz = Rf_allocVector(VECSXP, 2));
				SEXP rownamesx = VECTOR_ELT(dimnamesx, 0),
					rownamesy = R_NilValue,
					rownamesz = R_NilValue;
				if (rownamesx != R_NilValue) {
					PROTECT(rownamesx);
					PROTECT(rownamesy = (dimnamesy == R_NilValue) ? R_NilValue : VECTOR_ELT(dimnamesy, 0));
					PROTECT(rownamesz = Rf_allocVector(STRSXP, (int) (m + r)));
					if (rownamesy != R_NilValue)
					for (i = 0; i < r; ++i)
						SET_STRING_ELT(rownamesz, (int) i,
						               STRING_ELT(rownamesy, (int) i));
					for (i = 0; i < m; ++i)
						SET_STRING_ELT(rownamesz, (int) (i + r),
						               STRING_ELT(rownamesx, (int) i));
					SET_VECTOR_ELT(dimnamesz, 0, rownamesz);
					UNPROTECT(3);
				}
				SET_VECTOR_ELT(dimnamesz, 1, VECTOR_ELT(dimnamesx, 1));
				SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol);
				if (namesdimnamesx != R_NilValue) {
					PROTECT(namesdimnamesx);
					Rf_setAttrib(dimnamesz, R_NamesSymbol, namesdimnamesx);
					UNPROTECT(1);
				}
				R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesz);
				UNPROTECT(3);
			}
		} else {
			SEXP namesx = R_do_slot(s_x, R_flint_symbol_names),
				namesy = R_NilValue,
				namesz = R_NilValue;
			if (namesx != R_NilValue) {
				PROTECT(namesx);
				PROTECT(namesy = (usey) ? R_do_slot(s_y, R_flint_symbol_names) : R_NilValue);
				PROTECT(namesz = Rf_allocVector(STRSXP, (int) (m + r)));
				if (namesy != R_NilValue)
				for (i = 0; i < r; ++i)
					SET_STRING_ELT(namesz, (int) i,
					               STRING_ELT(namesy, (int) i));
				for (i = r; i < m; ++i)
					SET_STRING_ELT(namesz, (int) i,
					               STRING_ELT(namesx, (int) (i + r)));
				R_do_slot_assign(ans, R_flint_symbol_names, namesz);
				UNPROTECT(3);
			}
		}
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "arb");
		return R_NilValue;
	}
}
