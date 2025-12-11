#include "flint.h"

#ifdef R_FLINT_ABI_LL
# define __local_mpz_fits_slong_p(op) \
	(((op)->_mp_size ==  0) || \
	 ((op)->_mp_size ==  1 && (op)->_mp_d[0] <= WORD_MAX) || \
	 ((op)->_mp_size == -1 && (op)->_mp_d[0] <= (mp_limb_t) -1 - (mp_limb_t) WORD_MIN + 1))
# define __local_mpz_get_si(op) \
	(((op)->_mp_size == 0) ? 0 : \
	 ((op)->_mp_size >  0) ? (op)->_mp_d[0] & WORD_MAX : \
	 -1 - (mp_limb_signed_t) (((op)->_mp_d[0] - 1) & WORD_MAX))
# define __local_mpz_set_si(rop, op) \
	do { \
		if ((op) == 0) \
			(rop)->_mp_size =  0; \
		else if ((op) > 0) { \
			(rop)->_mp_size =  1; \
			(rop)->_mp_d[0] = (mp_limb_t) (op); \
		} \
		else { \
			(rop)->_mp_size = -1; \
			(rop)->_mp_d[0] = (mp_limb_t) -1 - (mp_limb_t) (op) + 1; \
		} \
	} while (0)
#else
# define __local_mpz_fits_slong_p(op) \
	mpz_fits_slong_p(op)
# define __local_mpz_get_si(op) \
	mpz_get_si(op)
# define __local_mpz_set_si(rop, op) \
	mpz_set_si(rop, op)
#endif

void R_flint_slong_finalize(SEXP x)
{
	slong *p = R_ExternalPtrAddr(x);
	if (p) {
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_slong_initialize(SEXP object, SEXP s_x, SEXP s_length,
                              SEXP s_dim, SEXP s_dimnames, SEXP s_names)
{
	mp_limb_t jy, nx = 0, ny = 0;
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
	slong *y = (ny) ? flint_calloc(ny, sizeof(slong)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_slong_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (jy = 0; jy < ny; ++jy)
			y[jy] = (slong) x[jy % nx];
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (x[jy % nx] == NA_LOGICAL)
			Rf_error(_("NaN is not representable by \"%s\""), "slong");
			else
			y[jy] = x[jy % nx];
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (x[jy % nx] == NA_INTEGER)
			Rf_error(_("NaN is not representable by \"%s\""), "slong");
			else
			y[jy] = x[jy % nx];
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (ISNAN(x[jy % nx]))
			Rf_error(_("NaN is not representable by \"%s\""), "slong");
#ifdef R_FLINT_ABI_64
			else if (x[jy % nx] <  -0x1.0p+63       ||
			         x[jy % nx] >=  0x1.0p+63)
#else
			else if (x[jy % nx] <= -0x1.0p+31 - 1.0 ||
			         x[jy % nx] >=  0x1.0p+31)
#endif
			Rf_error(_("floating-point number not in range of \"%s\""), "slong");
			else
			y[jy] = (slong) x[jy % nx];
		}
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (ISNAN(x[jy % nx].r))
			Rf_error(_("NaN is not representable by \"%s\""), "slong");
#ifdef R_FLINT_ABI_64
			else if (x[jy % nx].r <  -0x1.0p+63       ||
			         x[jy % nx].r >=  0x1.0p+63)
#else
			else if (x[jy % nx].r <= -0x1.0p+31 - 1.0 ||
			         x[jy % nx].r >=  0x1.0p+31)
#endif
			Rf_error(_("floating-point number not in range of \"%s\""), "slong");
			else
			y[jy] = (slong) x[jy % nx].r;
		}
		break;
	}
	case STRSXP:
	{
		mpz_t r;
		mpz_init(r);
		const char *s;
		for (jy = 0; jy < ny; ++jy) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) (jy % nx)));
			if (mpz_set_str(r, s, 0) != 0) {
				mpz_clear(r);
				Rf_error(_("invalid input in string conversion"));
			}
			if (!__local_mpz_fits_slong_p(r)) {
				mpz_clear(r);
				Rf_error(_("converted string not in range of \"%s\""), "slong");
			}
			y[jy] = __local_mpz_get_si(r);
		}
		mpz_clear(r);
		break;
	}
	case OBJSXP:
		switch (class) {
		case R_FLINT_CLASS_ULONG:
		{
			const ulong *x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (x[jy % nx] > WORD_MAX)
				Rf_error(_("integer not in range of \"%s\""), "slong");
				else
				y[jy] = (slong) x[jy % nx];
			}
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				y[jy] = x[jy % nx];
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy) {
				if (!fmpz_fits_si(x + jy % nx))
				Rf_error(_("integer not in range of \"%s\""), "slong");
				else
				y[jy] = fmpz_get_si(x + jy % nx);
			}
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (jy = 0; jy < ny; ++jy) {
				fmpz_tdiv_q(q, fmpq_numref(x + jy % nx), fmpq_denref(x + jy % nx));
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("rational not in range of \"%s\""), "slong");
				}
				else
				y[jy] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (jy = 0; jy < ny; ++jy) {
				mag_get_fmpz_lower(q, x + jy % nx);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of \"%s\""), "slong");
				}
				else
				y[jy] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (jy = 0; jy < ny; ++jy) {
				arf_get_fmpz(q, x + jy % nx, ARF_RND_DOWN);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of \"%s\""), "slong");
				}
				else
				y[jy] = fmpz_get_si(q);
			}
			fmpz_clear(q);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			fmpz_t q;
			fmpz_init(q);
			for (jy = 0; jy < ny; ++jy) {
				arf_get_fmpz(q, acf_realref(x + jy % nx), ARF_RND_DOWN);
				if (!fmpz_fits_si(q)) {
				fmpz_clear(q);
				Rf_error(_("floating-point number not in range of \"%s\""), "slong");
				}
				else
				y[jy] = fmpz_get_si(q);
			}
			fmpz_clear(q);
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
	setDDNN(object, s_dim, s_dimnames, s_names);
	UNPROTECT(3);
	return object;
}

SEXP R_flint_slong_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	const slong *x = R_flint_get_pointer(object);
	double *y = REAL(ans);
#ifdef R_FLINT_ABI_64
	fmpz_t tmp;
	fmpz_init(tmp);
	for (j = 0; j < n; ++j) {
		fmpz_set_si(tmp, x[j]);
		y[j] = fmpz_get_d(tmp);
	}
	fmpz_clear(tmp);
#else
	for (j = 0; j < n; ++j)
		y[j] = (double) x[j];
#endif
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_slong_format(SEXP object, SEXP s_base)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	if (n) {
	const slong *x = R_flint_get_pointer(object);
	slong xmin = 0, xmax = 0;
	for (j = 0; j < n; ++j) {
		if (x[j] > xmax)
			xmax = x[j];
		else if (x[j] < xmin)
			xmin = x[j];
	}
	size_t ns, nc, ncmax;
	mpz_t z;
	mpz_init2(z, 64);
	__local_mpz_set_si(z, (xmin < -xmax) ? xmin : xmax);
	ncmax = mpz_sizeinbase(z, abase);
	char *buffer = R_alloc(ncmax + 2, sizeof(char));
	mpz_get_str(buffer, base, z);
	ncmax = strlen(buffer);
	__local_mpz_set_si(z, (xmin < -xmax) ? xmax : xmin);
	mpz_get_str(buffer, base, z);
	if (buffer[ncmax] != '\0')
		ncmax = strlen(buffer);
	for (j = 0; j < n; ++j) {
		__local_mpz_set_si(z, x[j]);
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

SEXP R_flint_slong_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	R_flint_ops2_t op = ops2match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops2info(op);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	const slong
		*x = R_flint_get_pointer(s_x),
		*y = R_flint_get_pointer(s_y);
	int dz[3];
	info = checkConformable(s_x, s_y, nx, ny, info, dz);
	switch (op) {
	case R_FLINT_OPS2_ADD:
	case R_FLINT_OPS2_SUB:
	case R_FLINT_OPS2_MUL:
	case R_FLINT_OPS2_FDR:
	case R_FLINT_OPS2_FDQ:
	{
		slong *z = (nz) ? flint_calloc(nz, sizeof(slong)) : 0;
		slong a, b;
		int over = 0;
		switch (op) {
		case R_FLINT_OPS2_ADD:
			for (jz = 0; jz < nz; ++jz) {
#if !defined(__GNUC__)
				a = x[jz % nx];
				b = y[jz % ny];
				if ((a >= 0) ? b > WORD_MAX - a : b < WORD_MIN - a)
					break;
				z[jz] = a + b;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_saddll_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#else
				if (__builtin_saddl_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#endif
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz) {
				fmpz_set_si(Z + jz, x[jz % nx]);
				fmpz_add_si(Z + jz, Z + jz, y[jz % ny]);
			}
			}
			break;
		case R_FLINT_OPS2_SUB:
			for (jz = 0; jz < nz; ++jz) {
#if !defined(__GNUC__)
				a = x[jz % nx];
				b = y[jz % ny];
				if ((a >= 0) ? b < a - WORD_MAX : b > a - WORD_MIN)
					break;
				z[jz] = b - a;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_ssubll_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#else
				if (__builtin_ssubl_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#endif
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz) {
				fmpz_set_si(Z + jz, x[jz % nx]);
				fmpz_sub_si(Z + jz, Z + jz, y[jz % ny]);
			}
			}
			break;
		case R_FLINT_OPS2_MUL:
			for (jz = 0; jz < nz; ++jz) {
#if !defined(__GNUC__)
				if (z_mul_checked(&z[jz], x[jz % nx], y[jz % ny]))
					break;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_smulll_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#else
				if (__builtin_smull_overflow(x[jz % nx], y[jz % ny], &z[jz]))
					break;
#endif
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz) {
				fmpz_set_si(Z + jz, x[jz % nx]);
				fmpz_mul_si(Z + jz, Z + jz, y[jz % ny]);
			}
			}
			break;
		case R_FLINT_OPS2_FDR:
		{
			slong t;
			for (jz = 0; jz < nz; ++jz) {
				a = x[jz % nx];
				b = y[jz % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					z[jz] = 0;
				else {
					t = a % b;
					z[jz] = (t && (a >= 0) != (b >= 0)) ? t + b : t;
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			break;
		}
		case R_FLINT_OPS2_FDQ:
		{
			slong t;
			for (jz = 0; jz < nz; ++jz) {
				a = x[jz % nx];
				b = y[jz % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					break;
				else {
					t = a / b;
					z[jz] = (a % b && (a >= 0) != (b >= 0)) ? t - 1 : t;
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz) {
				b = y[jz % ny];
				if (b) {
				if (a == WORD_MIN && b == -1)
					fmpz_set_ui(Z + jz, (ulong) -1 - (ulong) WORD_MIN + 1);
				else {
					t = a / b;
					fmpz_set_si(Z + jz, (a % b && (a >= 0) != (b >= 0)) ? t - 1 : t);
				}
				} else {
				flint_free(z);
				Rf_error(_("quotient with 0 is undefined"));
				}
			}
			}
			break;
		}
		default: /* -Wswitch */
		}
		SEXP ans = PROTECT(newFlint((over) ? R_FLINT_CLASS_FMPZ : R_FLINT_CLASS_SLONG, z, nz));
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
			for (jz = 0; jz < nz; ++jz)
				if (y[jz % ny]) {
				fmpz_set_si(fmpq_numref(z + jz), x[jz % nx]);
				fmpz_set_si(fmpq_denref(z + jz), y[jz % ny]);
				fmpq_canonicalise(z + jz);
				}
				else
				Rf_error(_("quotient with 0 is undefined"));
			break;
		case R_FLINT_OPS2_POW:
		{
			slong b, e;
			fmpz_t t;
			fmpz_init(t);
			for (jz = 0; jz < nz; ++jz) {
				b = x[jz % nx];
				e = y[jz % ny];
				if (b == 0 && e < 0) {
				fmpz_clear(t);
				Rf_error(_("<%s> %s <%s>: value is not in the range of \"%s\""),
				         "slong", "^", "slong", "fmpq");
				}
				fmpz_set_si(t, b);
				if (e >= 0) {
				fmpz_pow_ui(fmpq_numref(z + jz), t, (ulong) e);
				fmpz_one(fmpq_denref(z + jz));
				} else {
				fmpz_one(fmpq_numref(z + jz));
				fmpz_pow_ui(fmpq_denref(z + jz), t, (ulong) -1 - (ulong) e + 1);
				fmpq_canonicalise(z + jz);
				}
			}
			fmpz_clear(t);
			break;
		}
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
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] == y[jz % ny];
			break;
		case R_FLINT_OPS2_NEQ:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] != y[jz % ny];
			break;
		case R_FLINT_OPS2_L:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] < y[jz % ny];
			break;
		case R_FLINT_OPS2_G:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] > y[jz % ny];
			break;
		case R_FLINT_OPS2_LEQ:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] <= y[jz % ny];
			break;
		case R_FLINT_OPS2_GEQ:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] >= y[jz % ny];
			break;
		case R_FLINT_OPS2_AND:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] && y[jz % ny];
			break;
		case R_FLINT_OPS2_OR:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz % nx] || y[jz % ny];
			break;
		default: /* -Wswitch */
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "slong");
		return R_NilValue;
	}
}

SEXP R_flint_slong_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	R_flint_ops1_t op = ops1match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops1info(op);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	const slong *x = R_flint_get_pointer(s_x);
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
		slong *z = (nz) ? flint_calloc(nz, sizeof(slong)) : 0;
		int over = 0;
		switch (op) {
		case R_FLINT_OPS1_PLUS:
		case R_FLINT_OPS1_CONJ:
		case R_FLINT_OPS1_REAL:
		case R_FLINT_OPS1_FLOOR:
		case R_FLINT_OPS1_CEILING:
		case R_FLINT_OPS1_TRUNC:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = x[jz];
			break;
		case R_FLINT_OPS1_MINUS:
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz] == WORD_MIN)
					break;
				z[jz] = -x[jz];
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz)
				if (x[jz] == WORD_MIN)
				fmpz_set_ui(Z + jz, (ulong) -1 - (ulong) WORD_MIN + 1);
				else
				fmpz_set_si(Z + jz, -x[jz]);
			}
			break;
		case R_FLINT_OPS1_IMAG:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 0;
			break;
		case R_FLINT_OPS1_MOD:
		case R_FLINT_OPS1_ABS:
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz] == WORD_MIN)
					break;
				z[jz] = (x[jz] < 0) ? -x[jz] : x[jz];
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			for (jz = 0; jz < nz; ++jz)
				fmpz_set_ui(Z + jz, (x[jz] < 0) ? (ulong) -1 - (ulong) WORD_MIN + 1 : (ulong) x[jz]);
			}
			break;
		case R_FLINT_OPS1_SIGN:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = (x[jz] < 0) ? -1 : (x[jz] > 0);
			break;
		case R_FLINT_OPS1_SQRT:
		{
			ulong r;
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz] >= 0)
				z[jz] = (slong) n_sqrtrem(&r, (ulong) x[jz]);
				else
				r = 1;
				if (r) {
				flint_free(z);
				Rf_error(_("%s(<%s>): value is not in the range of \"%s\""),
				         "sqrt", "slong", "slong");
				}
			}
			break;
		}
		case R_FLINT_OPS1_CUMMIN:
			if (nz) {
			z[0] = x[0];
			for (jz = 1; jz < nz; ++jz)
				z[jz] = (z[jz - 1] <= x[jz]) ? z[jz - 1] : x[jz];
			}
			break;
		case R_FLINT_OPS1_CUMMAX:
			if (nz) {
			z[0] = x[0];
			for (jz = 1; jz < nz; ++jz)
				z[jz] = (z[jz - 1] >= x[jz]) ? z[jz - 1] : x[jz];
			}
			break;
		case R_FLINT_OPS1_CUMSUM:
			if (nz) {
			z[0] = x[0];
			for (jz = 1; jz < nz; ++jz) {
#if !defined(__GNUC__)
				if ((z[jz - 1] >= 0) ? x[jz] > WORD_MAX - z[jz - 1] : x[jz] < WORD_MIN - z[jz - 1])
					break;
				z[jz] = z[jz - 1] + x[jz];
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_saddll_overflow(z[jz - 1], x[jz], z + jz))
					break;
#else
				if (__builtin_saddl_overflow(z[jz - 1], x[jz], z + jz))
					break;
#endif
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_set_si(Z, x[0]);
			for (jz = 1; jz < nz; ++jz)
				fmpz_add_si(Z + jz, Z + jz - 1, x[jz]);
			}
			}
			break;
		case R_FLINT_OPS1_CUMPROD:
			if (nz && x[0]) {
			z[0] = x[0];
			for (jz = 1; jz < nz; ++jz) {
				if (x[jz]) {
#if !defined(__GNUC__)
				if (z_mul_checked(z + jz, z[jz - 1], x[jz]))
					break;
#elif defined(R_FLINT_ABI_LL)
				if (__builtin_smulll_overflow(z[jz - 1], x[jz], z + jz))
					break;
#else
				if (__builtin_smull_overflow(z[jz - 1], x[jz], z + jz))
					break;
#endif
				} else {
					while (jz < nz)
						z[jz++] = 0;
					break;
				}
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_set_si(Z, x[0]);
			for (jz = 1; jz < nz; ++jz)
				if (x[jz])
				fmpz_mul_si(Z + jz, Z + jz - 1, x[jz]);
				else
				break;
			}
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
				z[jz] = x[jz];
#ifdef R_FLINT_ABI_64
			else if (digits <= -20)
#else
			else if (digits <= -10)
#endif
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 0;
			else {
#ifdef R_FLINT_ABI_64
			if (digits == -19) {
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz] <= -5000000000000000000L ||
				    x[jz] >=  5000000000000000000L)
					break;
				z[jz] = 0;
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			fmpz_set_ui(t, 10000000000000000000UL);
			for (jz = 0; jz < nz; ++jz)
				if (x[jz] <= -5000000000000000000L)
				fmpz_neg(Z + jz, t);
				else if (x[jz] >= 5000000000000000000L)
				fmpz_set(Z + jz, t);
			fmpz_clear(t);
			}
			} else {
#endif
			slong i, h, p, q, r, qmin, qmax;
			for (i = digits, p = 1; i < 0; ++i)
				p *= 10;
			h = p / 2; qmin = WORD_MIN / p; qmax = WORD_MAX / p;
			for (jz = 0; jz < nz; ++jz) {
				q = x[jz] / p;
				r = x[jz] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
					if (q > qmax)
						break;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
					if (q < qmin)
						break;
				}
				z[jz] = q * p;
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			fmpz_set_si(t, p);
			for (jz = 0; jz < nz; ++jz) {
				q = x[jz] / p;
				r = x[jz] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
				}
				fmpz_mul_si(Z + jz, t, q);
			}
			fmpz_clear(t);
			}
#ifdef R_FLINT_ABI_64
			}
#endif
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
				clog, i, h, p, q, r;
			if (digits <= 0)
				digits = 1;
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz]) {
				clog = (slong) n_clog((x[jz] >= 0) ? (ulong) x[jz] : (ulong) -1 - (ulong) x[jz] + 1, 10);
				if (clog <= digits)
				z[jz] = x[jz];
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[jz] / p;
				r = x[jz] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
					if (q > WORD_MAX / p)
						break;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
					if (q < WORD_MIN / p)
						break;
				}
				z[jz] = q * p;
				}
				}
				else
				z[jz] = 0;
			}
			over = jz < nz;
			if (over) {
			memset(z, 0, nz * sizeof(slong));
			fmpz *Z = (void *) z;
			fmpz_t t;
			fmpz_init(t);
			for (jz = 0; jz < nz; ++jz) {
				if (x[jz]) {
				clog = (slong) n_clog((x[jz] >= 0) ? (ulong) x[jz] : (ulong) -1 - (ulong) x[jz] + 1, 10);
				if (clog <= digits)
				fmpz_set_si(Z + jz, x[jz]);
				else {
				for (i = digits, p = 1; i < clog; ++i)
					p *= 10;
				h = p / 2;
				q = x[jz] / p;
				r = x[jz] % p;
				if (r >= h) {
					if (r >  h || q % 2)
						q += 1;
				}
				else if (r <= -h) {
					if (r < -h || q % 2)
						q -= 1;
				}
				fmpz_set_si(t, p);
				fmpz_mul_si(Z + jz, t, q);
				}
				}
			}
			}
			break;
		}
		default: /* -Wswitch */
		}
		SEXP ans = PROTECT(newFlint((over) ? R_FLINT_CLASS_FMPZ : R_FLINT_CLASS_SLONG, z, nz));
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
		slong *z = flint_calloc(nz, sizeof(slong));
		int over = 0;
		switch (op) {
		case R_FLINT_OPS1_MIN:
			z[0] = x[0];
			for (jx = 1; jx < nx; ++jx)
				if (z[0] > x[jx])
					z[0] = x[jx];
			break;
		case R_FLINT_OPS1_MAX:
			z[0] = x[0];
			for (jx = 1; jx < nx; ++jx)
				if (z[0] < x[jx])
					z[0] = x[jx];
			break;
		case R_FLINT_OPS1_RANGE:
			z[0] = z[1] = x[0];
			for (jx = 1; jx < nx; ++jx)
				if (z[0] > x[jx])
					z[0] = x[jx];
				else if (z[1] < x[jx])
					z[1] = x[jx];
			break;
		case R_FLINT_OPS1_SUM:
		{
			fmpz_t t;
			fmpz_init(t);
			fmpz_zero(t);
			for (jx = 0; jx < nx; ++jx)
				fmpz_add_si(t, t, x[jx]);
			if (fmpz_fits_si(t))
				z[0] = fmpz_get_si(t);
			else {
				z[0] = 0;
				fmpz_set((void *) z, t);
				over = 1;
			}
			fmpz_clear(t);
			break;
		}
		case R_FLINT_OPS1_PROD:
		{
			fmpz_t t;
			fmpz_init(t);
			fmpz_one(t);
			for (jx = 0; jx < nx; ++jx) {
				if (x[jx])
				fmpz_mul_si(t, t, x[jx]);
				else {
				fmpz_zero(t);
				break;
				}
			}
			if (fmpz_fits_si(t))
				z[0] = fmpz_get_si(t);
			else {
				z[0] = 0;
				fmpz_set((void *) z, t);
				over = 1;
			}
			fmpz_clear(t);
			break;
		}
		default: /* -Wswitch */
		}
		SEXP ans = newFlint((over) ? R_FLINT_CLASS_FMPZ : R_FLINT_CLASS_SLONG, z, nz);
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
				fmpz_add_si(fmpq_numref(z), fmpq_numref(z), x[jx]);
			fmpz_set_ui(fmpq_denref(z), nx);
			fmpq_canonicalise(z);
			break;
		default: /* -Wswitch */
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
			for (jx = 0; jx < nx && x[jx] == 0; ++jx) ;
			z[0] = jx <  nx;
			break;
		case R_FLINT_OPS1_ALL:
			for (jx = 0; jx < nx && x[jx] != 0; ++jx) ;
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
			for (jx = 1; jx < nx && x[0] <  x[1]; ++jx, ++x) ;
			else
			for (jx = 1; jx < nx && x[0] <= x[1]; ++jx, ++x) ;
			z[0] = jx <  nx;
			break;
		}
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
				z[jz] = !x[jz];
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
						fmpz_add_si(fmpq_numref(z__ + jz), fmpq_numref(z__ + jz), x[jx]);
				for (jz = 0; jz < nz; ++jz) {
					fmpz_set_ui(fmpq_denref(z__ + jz), nt);
					fmpq_canonicalise(z__ + jz);
				}
			} else {
				for (jz = 0; jz < nz; ++jz) {
					fmpz_zero(fmpq_numref(z__ + jz));
					for (jt = 0; jt < nt; ++jt, ++jx)
						fmpz_add_si(fmpq_numref(z__ + jz), fmpq_numref(z__ + jz), x[jx]);
					fmpz_set_ui(fmpq_denref(z__ + jz), nt);
					fmpq_canonicalise(z__ + jz);
				}
			}
			class = R_FLINT_CLASS_FMPQ;
			z = z__;
		} else {
			fmpz *z__ = (nz) ? flint_calloc(nz, sizeof(fmpz)) : 0;
			int over = 0;
			if (byrow) {
				for (jz = 0; jz < nz; ++jz)
					fmpz_zero(z__ + jz);
				for (jt = 0; jt < nt; ++jt)
					for (jz = 0; jz < nz; ++jz, ++jx)
						fmpz_add_si(z__ + jz, z__ + jz, x[jx]);
				for (jz = 0; jz < nz; ++jz)
					if (!fmpz_fits_si(z__ + jz))
						break;
				over = jz < nz;
			} else {
				for (jz = 0; jz < nz; ++jz) {
					fmpz_zero(z__ + jz);
					for (jt = 0; jt < nt; ++jt, ++jx)
						fmpz_add_si(z__ + jz, z__ + jz, x[jx]);
					over = over || !fmpz_fits_si(z__ + jz);
				}
			}
			if (over) {
				class = R_FLINT_CLASS_FMPZ;
				z = z__;
			} else {
				for (jz = 0; jz < nz; ++jz)
					((slong *) z__)[jz] = fmpz_get_si(z__ + jz);
				class = R_FLINT_CLASS_SLONG;
				z = z__;
			}
		}
		SEXP ans = PROTECT(newFlint(class, z, nz));
		if (ndz > 1)
			setDDNN(ans, dimz, dimnamesz, R_NilValue);
		else if (dimnamesz != R_NilValue)
			setDDNN(ans, R_NilValue, R_NilValue, VECTOR_ELT(dimnamesz, 0));
		UNPROTECT(4);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "slong");
		return R_NilValue;
	}
}
