#include "flint.h"

void R_flint_fmpz_finalize(SEXP x)
{
	fmpz *p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		fmpz_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_fmpz_initialize(SEXP object, SEXP s_x, SEXP s_length,
                             SEXP s_dim, SEXP s_dimnames, SEXP s_names)
{
	mp_limb_t jx, jy, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	PROTECT(s_dim = validDim(s_dim));
	PROTECT(s_dimnames = validDimNames(s_dimnames, s_dim));
	if (s_x != R_NilValue) {
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
	fmpz *y = (ny) ? flint_calloc(ny, sizeof(fmpz)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_fmpz_finalize);
	int seenimag = 0, seenrad = 0;
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx)
			fmpz_set_ui(y + jy, x[jx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (x[jx] == NA_LOGICAL)
				Rf_error(_("NA is not representable by \"%s\""),
				         "fmpz");
			fmpz_set_si(y + jy, x[jx] != 0);
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (x[jx] == NA_INTEGER)
				Rf_error(_("NA is not representable by \"%s\""),
				         "fmpz");
			fmpz_set_si(y + jy, x[jx]);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (!R_FINITE(x[jx]))
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
				         "fmpz");
			fmpz_set_d(y + jy, (fabs(x[jx]) < DBL_MIN) ? 0.0 : x[jx]);
		}
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (!R_FINITE(x[jx].r))
				Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
				         "fmpz");
			fmpz_set_d(y + jy, (fabs(x[jx].r) < DBL_MIN) ? 0.0 : x[jx].r);
			seenimag = seenimag || x[jx].i != 0.0;
		}
		break;
	}
	case STRSXP:
	{
		mpz_t r;
		mpz_init(r);
		const char *s;
		FOR_RECYCLE1(jy, ny, jx, nx) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) jx));
			if (mpz_set_str(r, s, 0) != 0) {
				mpz_clear(r);
				Rf_error(_("invalid input in string conversion"));
			}
			fmpz_set_mpz(y + jy, r);
		}
		mpz_clear(r);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				fmpz_set_ui(y + jy, x[jx]);
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				fmpz_set_si(y + jy, x[jx]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				fmpz_set(y + jy, x + jx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				fmpz_tdiv_q(y + jy, fmpq_numref(x + jx), fmpq_denref(x + jx));
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (mag_is_inf(x + jx))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
					         "fmpz");
				mag_get_fmpz_lower(y + jy, x + jx);
			}
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (!arf_is_finite(x + jx))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
					         "fmpz");
				arf_get_fmpz(y + jy, x + jx, ARF_RND_DOWN);
			}
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (!arf_is_finite(acf_realref(x + jx)))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
					         "fmpz");
				arf_get_fmpz(y + jy, acf_realref(x + jx), ARF_RND_DOWN);
				seenimag = seenimag || !arf_is_zero(acf_imagref(x + jx));
			}
			break;
		}
		case R_FLINT_CLASS_ARB:
		{
			arb_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (!arf_is_finite(arb_midref(x + jx)))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
					         "fmpz");
				arf_get_fmpz(y + jy, arb_midref(x + jx), ARF_RND_DOWN);
				seenrad = seenrad || !arb_is_exact(x + jx);
			}
			break;
		}
		case R_FLINT_CLASS_ACB:
		{
			acb_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (!arf_is_finite(arb_midref(acb_realref(x + jx))))
					Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""),
					         "fmpz");
				arf_get_fmpz(y + jy, arb_midref(acb_realref(x + jx)), ARF_RND_DOWN);
				seenimag = seenimag || !arb_is_zero (acb_imagref(x + jx));
				seenrad  = seenrad  || !arb_is_exact(acb_realref(x + jx));
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
	if (seenrad ) WARNING_LOST_RAD ;
	setDDNN(object, s_dim, s_dimnames, s_names);
	UNPROTECT(3);
	return object;
}

SEXP R_flint_fmpz_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const fmpz *x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	int seenoob = 0;
	fmpz_t lb, ub;
	fmpz_init(lb);
	fmpz_init(ub);
	fmpz_set_d(ub, DBL_MAX);
	fmpz_neg(lb, ub);
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, lb) >= 0 && fmpz_cmp(x + j, ub) <= 0)
			y[j] = fmpz_get_d(x + j);
		else {
			y[j] = (fmpz_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			seenoob = 1;
		}
	}
	fmpz_clear(lb);
	fmpz_clear(ub);
	if (seenoob) WARNING_OOB_DOUBLE;
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_fmpz_format(SEXP object, SEXP s_base)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	if (n) {
	const fmpz *x = R_flint_get_pointer(object);
	mp_limb_t jmax = 0, jmin = 0;
	for (j = 0; j < n; ++j) {
		if (fmpz_cmp(x + j, x + jmax) > 0)
			jmax = j;
		else if (fmpz_cmp(x + j, x + jmin) < 0)
			jmin = j;
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init(z);
	fmpz_get_mpz(z, x + ((fmpz_cmpabs(x + jmin, x + jmax) >= 0) ? jmin : jmax));
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, sizeof(char));
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	fmpz_get_mpz(z, x + ((fmpz_cmpabs(x + jmin, x + jmax) <= 0) ? jmin : jmax));
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(z, x + j);
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
		SET_STRING_ELT(ans, (R_xlen_t) j, Rf_mkChar(buffer));
	}
	mpz_clear(z);
	}
	setDDNN1(ans, object);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_fmpz_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	R_flint_ops2_t op = ops2match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops2info(op);
	mp_limb_t jx, jy, jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	const fmpz
		*x = R_flint_get_pointer(s_x),
		*y = R_flint_get_pointer(s_y);
	int dz[3];
	info = checkConformable(s_x, s_y, nx, ny, info, dz);
	if (info >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
	switch (op) {
	case R_FLINT_OPS2_ADD:
	case R_FLINT_OPS2_SUB:
	case R_FLINT_OPS2_MUL:
	case R_FLINT_OPS2_FDR:
	case R_FLINT_OPS2_FDQ:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, nz));
		fmpz *z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS2_ADD:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				fmpz_add(z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_SUB:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				fmpz_sub(z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_MUL:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				fmpz_mul(z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_FDR:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				if (fmpz_is_zero(y + jy))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_r(z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_FDQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				if (fmpz_is_zero(y + jy))
				Rf_error(_("quotient with 0 is undefined"));
				else
				fmpz_fdiv_q(z + jz, x + jx, y + jy);
			break;
		default: /* -Wswitch */
			break;
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS2_DIV:
	case R_FLINT_OPS2_POW:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPQ, 0, nz));
		fmpq *z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS2_DIV:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				if (fmpz_is_zero(y + jy))
				Rf_error(_("quotient with 0 is undefined"));
				else {
				fmpz_set(fmpq_numref(z + jz), x + jx);
				fmpz_set(fmpq_denref(z + jz), y + jy);
				fmpq_canonicalise(z + jz);
				}
			break;
		case R_FLINT_OPS2_POW:
		{
			const fmpz *b, *e;
			ulong u;
			fmpz_t a;
			fmpz_init(a);
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny) {
				b = x + jx;
				e = y + jy;
				if (fmpz_is_zero(b) && fmpz_sgn(e) < 0) {
				fmpz_clear(a);
				Rf_error(_("<%s> %s <%s>: value is not in the range of \"%s\""),
				         "fmpz", "^", "fmpz", "fmpq");
				}
				if (!fmpz_abs_fits_ui(e)) {
				fmpz_clear(a);
				Rf_error(_("<%s> %s <%s>: exponent exceeds maximum %llu in absolute value"),
				         "fmpz", "^", "fmpz", (unsigned long long int) UWORD_MAX);
				}
				if (fmpz_sgn(e) >= 0) {
				u = fmpz_get_ui(e);
				fmpz_pow_ui(fmpq_numref(z + jz), b, u);
				fmpz_one(fmpq_denref(z + jz));
				} else {
				fmpz_neg(a, e);
				u = fmpz_get_ui(a);
				fmpz_one(fmpq_numref(z + jz));
				fmpz_pow_ui(fmpq_denref(z + jz), b, u);
				fmpq_canonicalise(z + jz);
				}
			}
			fmpz_clear(a);
			break;
		}
		default: /* -Wswitch */
			break;
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
				z[jz] = fmpz_equal(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_NEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = fmpz_equal(x + jx, y + jy) == 0;
			break;
		case R_FLINT_OPS2_L:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = fmpz_cmp(x + jx, y + jy) < 0;
			break;
		case R_FLINT_OPS2_G:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = fmpz_cmp(x + jx, y + jy) > 0;
			break;
		case R_FLINT_OPS2_LEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = fmpz_cmp(x + jx, y + jy) <= 0;
			break;
		case R_FLINT_OPS2_GEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = fmpz_cmp(x + jx, y + jy) >= 0;
			break;
		case R_FLINT_OPS2_AND:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = !fmpz_is_zero(x + jx) && !fmpz_is_zero(y + jy);
			break;
		case R_FLINT_OPS2_OR:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = !fmpz_is_zero(x + jx) || !fmpz_is_zero(y + jy);
			break;
		default: /* -Wswitch */
			break;
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
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, nz));
		fmpz *z = R_flint_get_pointer(ans);
		int tx = (info & 1) != 0, ty = (info & 2) != 0, i, j;
		mp_limb_t jx, jy, ja, jb;
		fmpz_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = z;
		ma->entries = (ty) ? ((ny) ? flint_calloc(ny, sizeof(fmpz)) : 0) : (void *) y;
		mb->entries = (tx) ? ((nx) ? flint_calloc(nx, sizeof(fmpz)) : 0) : (void *) x;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, y + jy);
		}
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += (mp_limb_t) mb->r)
					fmpz_set(mb->entries + jb, x + jx);
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpz *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpz *)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(fmpz *)) : 0;
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
		fmpz_mat_mul(mc, ma, mb);
		if (ty) {
			for (ja = 0; ja < ny; ++ja)
				fmpz_clear(ma->entries + ja);
			flint_free(ma->entries);
		}
		if (tx) {
			for (jb = 0; jb < nx; ++jb)
				fmpz_clear(mb->entries + jb);
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
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPQ, 0, nz));
		fmpq *z = R_flint_get_pointer(ans);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		fmpz_mat_t mc, ma, mb;
		fmpz_t den;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(fmpz)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(fmpz)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(fmpz)) : 0;
		if (op == R_FLINT_OPS2_TBACKSOLVE)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				fmpz_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->r - (++i)))
				for (j = 0; j <= i; ++j, ++ja)
					fmpz_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					fmpz_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->c - (++i)), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += (mp_limb_t) mb->r)
				fmpz_set(mb->entries + jb, y + jy);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpz *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpz *)) : 0;
		mb->rows = (mb->r) ? flint_calloc((size_t) mb->r, sizeof(fmpz *)) : 0;
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
		fmpz_init(den);
		singular = !fmpz_mat_solve(mc, den, ma, mb);
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += (mp_limb_t) mc->c) {
				fmpz_set(fmpq_numref(z + jz), mc->entries + jc);
				fmpz_set(fmpq_denref(z + jz), den);
				fmpq_canonicalise(z + jz);
				fmpz_clear(mc->entries + jc);
			}
		for (ja = 0; ja < nx; ++ja)
			fmpz_clear(ma->entries + ja);
		for (jb = 0; jb < ny; ++jb)
			fmpz_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
		flint_free(mb->rows);
#endif
		fmpz_clear(den);
		if (singular)
			Rf_error(_("matrix is exactly singular"));
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
}

SEXP R_flint_fmpz_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	R_flint_ops1_t op = ops1match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops1info(op);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	const fmpz *x = R_flint_get_pointer(s_x);
	switch (op) {
	case R_FLINT_OPS1_PLUS:
	case R_FLINT_OPS1_MINUS:
	case R_FLINT_OPS1_CONJ:
	case R_FLINT_OPS1_REAL:
	case R_FLINT_OPS1_IMAG:
	case R_FLINT_OPS1_MOD:
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
	case R_FLINT_OPS1_ROUND:
	case R_FLINT_OPS1_SIGNIF:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, nz));
		fmpz *z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_PLUS:
		case R_FLINT_OPS1_CONJ:
		case R_FLINT_OPS1_REAL:
		case R_FLINT_OPS1_FLOOR:
		case R_FLINT_OPS1_CEILING:
		case R_FLINT_OPS1_TRUNC:
			for (jz = 0; jz < nz; ++jz)
				fmpz_set(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_MINUS:
			for (jz = 0; jz < nz; ++jz)
				fmpz_neg(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_IMAG:
			break;
		case R_FLINT_OPS1_MOD:
		case R_FLINT_OPS1_ABS:
			for (jz = 0; jz < nz; ++jz)
				fmpz_abs(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_SIGN:
			for (jz = 0; jz < nz; ++jz)
				fmpz_set_si(z + jz, fmpz_sgn(x + jz));
			break;
		case R_FLINT_OPS1_SQRT:
		{
			fmpz_t r;
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (fmpz_sgn(x + jz) >= 0)
				fmpz_sqrtrem(z + jz, r, x + jz);
				if (!(fmpz_sgn(x + jz) >= 0 && fmpz_is_zero(r))) {
				fmpz_clear(r);
				Rf_error(_("%s(<%s>): value is not in the range of \"%s\""),
				         "sqrt", "fmpz", "fmpz");
				}
			}
			fmpz_clear(r);
			break;
		}
		case R_FLINT_OPS1_CUMMIN:
			if (nz) {
			fmpz_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpz_set(z + jz, (fmpz_cmp(z + jz - 1, x + jz) <= 0) ? z + jz - 1 : x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMMAX:
			if (nz) {
			fmpz_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpz_set(z + jz, (fmpz_cmp(z + jz - 1, x + jz) >= 0) ? z + jz - 1 : x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMSUM:
			if (nz) {
			fmpz_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpz_add(z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMPROD:
			if (nz) {
			fmpz_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				fmpz_mul(z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_ROUND:
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
			if (digits >= 0)
			for (jz = 0; jz < nz; ++jz)
				fmpz_set(z + jz, x + jz);
			else {
			fmpz_t p, q, r;
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			fmpz_set_si(p, 10);
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (jz = 0; jz < nz; ++jz) {
				fmpz_ndiv_qr(q, r, x + jz, p);
				if (fmpz_cmp2abs(p, r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + jz, q, p);
			}
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			}
			break;
		}
		case R_FLINT_OPS1_SIGNIF:
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0],
				clog;
			if (digits <= 0)
				digits = 1;
			fmpz_t a, p, q, r;
			fmpz_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (fmpz_is_zero(x + jz))
				fmpz_zero(z + jz);
				else {
				fmpz_abs(a, x + jz);
				clog = fmpz_clog_ui(a, 10);
				if (clog <= digits)
				fmpz_set(z + jz, x + jz);
				else {
				fmpz_set_si(p, 10);
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_ndiv_qr(q, r, x + jz, p);
				if (fmpz_cmp2abs(p, r) == 0 && fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(z + jz, q, p);
				}
				}
			}
			fmpz_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			break;
		}
		default: /* -Wswitch */
			break;
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_MIN:
	case R_FLINT_OPS1_MAX:
	case R_FLINT_OPS1_RANGE:
		if (nx == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case R_FLINT_OPS1_SUM:
	case R_FLINT_OPS1_PROD:
	{
		nz = (op == R_FLINT_OPS1_RANGE) ? 2 : 1;
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, nz));
		fmpz *z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_MIN:
			fmpz_set(z, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpz_cmp(z, x + jx) > 0)
					fmpz_set(z, x + jx);
			break;
		case R_FLINT_OPS1_MAX:
			fmpz_set(z, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpz_cmp(z, x + jx) < 0)
					fmpz_set(z, x + jx);
			break;
		case R_FLINT_OPS1_RANGE:
			fmpz_set(z, x);
			fmpz_set(z + 1, x);
			for (jx = 1; jx < nx; ++jx)
				if (fmpz_cmp(z, x + jx) > 0)
					fmpz_set(z, x + jx);
				else if (fmpz_cmp(z + 1, x + jx) < 0)
					fmpz_set(z + 1, x + jx);
			break;
		case R_FLINT_OPS1_SUM:
			fmpz_zero(z);
			for (jx = 0; jx < nx; ++jx)
				fmpz_add(z, z, x + jx);
			break;
		case R_FLINT_OPS1_PROD:
			fmpz_one(z);
			for (jx = 0; jx < nx; ++jx)
				fmpz_mul(z, z, x + jx);
			break;
		default: /* -Wswitch */
			break;
		}
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_MEAN:
	{
		if (nx == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPQ, 0, 1));
		fmpq *z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_MEAN:
			fmpq_zero(z);
			for (jx = 0; jx < nx; ++jx)
				fmpz_add(fmpq_numref(z), fmpq_numref(z), x + jx);
			fmpz_set_ui(fmpq_denref(z), nx);
			fmpq_canonicalise(z);
			break;
		default: /* -Wswitch */
			break;
		}
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_ANY:
	case R_FLINT_OPS1_ALL:
	case R_FLINT_OPS1_ANYNA:
	case R_FLINT_OPS1_ISUNS:
	{
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case R_FLINT_OPS1_ANY:
			for (jx = 0; jx < nx &&  fmpz_is_zero(x + jx); ++jx) ;
			z[0] = jx <  nx;
			break;
		case R_FLINT_OPS1_ALL:
			for (jx = 0; jx < nx && !fmpz_is_zero(x + jx); ++jx) ;
			z[0] = jx >= nx;
			break;
		case R_FLINT_OPS1_ANYNA:
			z[0] = 0;
			break;
		case R_FLINT_OPS1_ISUNS:
		{
			SEXP s_strict = VECTOR_ELT(s_dots, 1);
			if (XLENGTH(s_strict) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "strictly", CHAR(STRING_ELT(s_op, 0)));
			int strict = LOGICAL_RO(s_strict)[0];
			if (strict)
			for (jx = 1; jx < nx && fmpz_cmp(x, x + 1) <  0; ++jx, ++x) ;
			else
			for (jx = 1; jx < nx && fmpz_cmp(x, x + 1) <= 0; ++jx, ++x) ;
			z[0] = jx <  nx;
			break;
		}
		default: /* -Wswitch */
			break;
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
		case R_FLINT_OPS1_ISINF:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 0;
			break;
		case R_FLINT_OPS1_ISNUM:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 1;
			break;
		case R_FLINT_OPS1_NOT:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = fmpz_is_zero(x + jz) != 0;
			break;
		default: /* -Wswitch */
			break;
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

		R_flint_class_t class;
		void *z;
		jx = 0;
		if (domean) {
			fmpq *z__ = (nz) ? flint_calloc(nz, sizeof(fmpq)) : 0;
			if (byrow) {
				for (jz = 0; jz < nz; ++jz)
					fmpz_zero(fmpq_numref(z__ + jz));
				for (jt = 0; jt < nt; ++jt)
					for (jz = 0; jz < nz; ++jz, ++jx)
						fmpz_add(fmpq_numref(z__ + jz), fmpq_numref(z__ + jz), x + jx);
				for (jz = 0; jz < nz; ++jz) {
					fmpz_set_ui(fmpq_denref(z__ + jz), nt);
					fmpq_canonicalise(z__ + jz);
				}
			} else {
				for (jz = 0; jz < nz; ++jz) {
					fmpz_zero(fmpq_numref(z__ + jz));
					for (jt = 0; jt < nt; ++jt, ++jx)
						fmpz_add(fmpq_numref(z__ + jz), fmpq_numref(z__ + jz), x + jx);
					fmpz_set_ui(fmpq_denref(z__ + jz), nt);
					fmpq_canonicalise(z__ + jz);
				}
			}
			class = R_FLINT_CLASS_FMPQ;
			z = z__;
		} else {
			fmpz *z__ = (nz) ? flint_calloc(nz, sizeof(fmpz)) : 0;
			if (byrow) {
				for (jz = 0; jz < nz; ++jz)
					fmpz_zero(z__ + jz);
				for (jt = 0; jt < nt; ++jt)
					for (jz = 0; jz < nz; ++jz, ++jx)
						fmpz_add(z__ + jz, z__ + jz, x + jx);
			} else {
				for (jz = 0; jz < nz; ++jz) {
					fmpz_zero(z__ + jz);
					for (jt = 0; jt < nt; ++jt, ++jx)
						fmpz_add(z__ + jz, z__ + jz, x + jx);
				}
			}
			class = R_FLINT_CLASS_FMPZ;
			z = z__;
		}
		SEXP ans = PROTECT(newFlint(class, z, nz));
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
			Rf_error(_("first argument is not a square matrix"));
		int uplo = 'N';
		if (op != R_FLINT_OPS1_SOLVE) {
			SEXP s_uppertri = VECTOR_ELT(s_dots, 0);
			if (XLENGTH(s_uppertri) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "upper.tri", CHAR(STRING_ELT(s_op, 0)));
			uplo = (LOGICAL_RO(s_uppertri)[0]) ? 'U' : 'L';
		}
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPQ, 0, nz));
		fmpq *z = R_flint_get_pointer(ans);
		int i, j, singular;
		mp_limb_t jc, ja;
		fmpz_mat_t mc, ma;
		fmpz_t den;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(fmpz)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(fmpz)) : 0;
		if (op != R_FLINT_OPS1_TBACKSOLVE)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				fmpz_set(ma->entries + ja, x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->r - (++i)))
				for (j = 0; j <= i; ++j, ++ja)
					fmpz_set(ma->entries + ja, x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					fmpz_set(ma->entries + ja, x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (mp_limb_t) (ma->c - (++i)), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += (mp_limb_t) ma->r)
					fmpz_set(ma->entries + ja, x + jx);
			break;
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		mc->rows = (mc->r) ? flint_calloc((size_t) mc->r, sizeof(fmpz *)) : 0;
		ma->rows = (ma->r) ? flint_calloc((size_t) ma->r, sizeof(fmpz *)) : 0;
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
		fmpz_init(den);
		singular = !fmpz_mat_inv(mc, den, ma);
		for (jc = 0; jc < nz; ++jc) {
			fmpz_set(fmpq_numref(z + jc), mc->entries + jc);
			fmpz_set(fmpq_denref(z + jc), den);
			fmpq_canonicalise(z + jc);
			fmpz_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			fmpz_clear(ma->entries + ja);
		flint_free(mc->entries);
		flint_free(ma->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mc->rows);
		flint_free(ma->rows);
#endif
		fmpz_clear(den);
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
	case R_FLINT_OPS1_DET:
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, 1));
		fmpz *z = R_flint_get_pointer(ans);
		fmpz_mat_t mx;
		mx->r = mx->c = dx[0];
		mx->entries = (void *) x;
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(fmpz *)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		fmpz_mat_det(z, mx);
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
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, (m - r) * n));
		if (r < m) {
			SEXP work = newFlint(R_FLINT_CLASS_FMPZ, 0, m);
			fmpz *z = R_flint_get_pointer(ans),
				*w = R_flint_get_pointer(work);
			for (j = 0; j < n; ++j) {
				for (i = 0; i < m; ++i)
					fmpz_set(w + i, x + i);
				r__ = 0;
				for (k = 0; k < ord; ++k) {
					r__ += lag;
					for (i = m - 1; i >= r__; --i)
						fmpz_sub(w + i, w + i, w + i - lag);
				}
				for (i = r; i < m; ++i)
					fmpz_set(z + i - r, w + i);
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
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, (m + r) * n));
		fmpz *z = R_flint_get_pointer(ans);
		const fmpz *y = (usey) ? R_flint_get_pointer(s_y) : 0;
		for (j = 0; j < n; ++j) {
			if (usey)
			for (i = 0; i < r; ++i)
				fmpz_set(z + i, y + i);
			else
			for (i = 0; i < r; ++i)
				fmpz_zero(z + i);
			for (i = 0; i < m; ++i)
				fmpz_set(z + r + i, x + i);
			r__ = 0;
			for (k = 0; k < ord; ++k) {
				r__ += lag;
				for (i = r - 1; i >= r__; --i)
					fmpz_sub(z + i, z + i, z + i - lag);
			}
			r__ = r;
			for (k = 0; k < ord; ++k) {
				for (i = r__; i < m + r; ++i)
					fmpz_add(z + i, z + i, z + i - lag);
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
		         CHAR(STRING_ELT(s_op, 0)), "fmpz");
		return R_NilValue;
	}
}
