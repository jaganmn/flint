#include "flint.h"

#define TERN(op, lower, ...) \
	(lower) ? op##_lower(__VA_ARGS__) : op(__VA_ARGS__)

void R_flint_mag_finalize(SEXP x)
{
	mag_ptr p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		mag_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_mag_initialize(SEXP object, SEXP s_x, SEXP s_length,
                            SEXP s_dim, SEXP s_dimnames, SEXP s_names,
                            SEXP s_rnd)
{
	mp_limb_t jx, jy, nx = 0, ny = 0;
	R_flint_class_t class = R_FLINT_CLASS_INVALID;
	int lower = isRndZ(asRnd(s_rnd, 0, __func__));
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
	mag_ptr y = (ny) ? flint_calloc(ny, sizeof(mag_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_mag_finalize);
	int seenimag = 0, seenrad = 0;
	switch (TYPEOF(s_x)) {
	case NILSXP:
		FOR_RECYCLE0(jy, ny)
			mag_zero(y + jy);
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx)
			TERN(mag_set_ui, lower, y + jy, x[jx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (x[jx] == NA_LOGICAL)
				Rf_error(_("NaN is not representable by \"%s\""),
				         "mag");
			TERN(mag_set_ui, lower, y + jy, (ulong) (x[jx] != 0));
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (x[jx] == NA_INTEGER)
				Rf_error(_("NaN is not representable by \"%s\""),
				         "mag");
			if (x[jx] >= 0)
			TERN(mag_set_ui, lower, y + jy, (ulong)  x[jx]);
			else
			TERN(mag_set_ui, lower, y + jy, (ulong) -x[jx]);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (ISNAN(x[jx]))
				Rf_error(_("NaN is not representable by \"%s\""),
				         "mag");
			TERN(mag_set_d, lower, y + jy, x[jx]);
		}
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		FOR_RECYCLE1(jy, ny, jx, nx) {
			if (ISNAN(x[jx].r))
				Rf_error(_("NaN is not representable by \"%s\""),
				         "mag");
			TERN(mag_set_d, lower, y + jy, x[jx].r);
			seenimag = seenimag || x[jx].i != 0.0;
		}
		break;
	}
	case STRSXP:
	{
		mpfr_prec_t prec = MAG_BITS << 1;
		mpfr_rnd_t rnd = (lower) ? MPFR_RNDZ : MPFR_RNDA;
		mpfr_t r;
		arf_t tmp;
		mpfr_init2(r, prec);
		arf_init(tmp);
		const char *s;
		char *t;
		FOR_RECYCLE1(jy, ny, jx, nx) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) jx));
			mpfr_strtofr(r, s, &t, 0, rnd);
			if (t <= s)
				break;
			s = t;
			while (isspace(*s))
				s++;
			if (*s != '\0')
				break;
			arf_set_mpfr(tmp, r);
			TERN(arf_get_mag, lower, y + jy, tmp);
		}
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
				TERN(mag_set_ui, lower, y + jy, x[jx]);
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				if (x[jx] >= 0)
				TERN(mag_set_ui, lower, y + jy, (ulong) x[jx]);
				else
				TERN(mag_set_ui, lower, y + jy, (ulong) -1 - (ulong) x[jx] + 1);
			}
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				TERN(mag_set_fmpz, lower, y + jy, x + jx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			slong prec = MAG_BITS << 1;
			arf_rnd_t rnd = (lower) ? ARF_RND_DOWN : ARF_RND_UP;
			arf_t q;
			arf_init(q);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				arf_fmpz_div_fmpz(q, fmpq_numref(x + jx), fmpq_denref(x + jx), prec, rnd);
				TERN(arf_get_mag, lower, y + jy, q);
			}
			arf_clear(q);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				mag_set(y + jy, x + jx);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx)
				TERN(arf_get_mag, lower, y + jy, x + jx);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				TERN(arf_get_mag, lower, y + jy, acf_realref(x + jx));
				seenimag = seenimag || !arf_is_zero(acf_imagref(x + jx));
			}
			break;
		}
		case R_FLINT_CLASS_ARB:
		{
			arb_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				TERN(arf_get_mag, lower, y + jy, arb_midref(x + jx));
				seenrad = seenrad || !arb_is_exact(x + jx);
			}
			break;
		}
		case R_FLINT_CLASS_ACB:
		{
			acb_srcptr x = R_flint_get_pointer(s_x);
			FOR_RECYCLE1(jy, ny, jx, nx) {
				TERN(arf_get_mag, lower, y + jy, arb_midref(acb_realref(x + jx)));
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

SEXP R_flint_mag_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int lower = isRndZ(asRnd(R_NilValue, 0, __func__));
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	mag_srcptr x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	int seenoob = 0;
	mag_t ub;
	mag_init(ub);
	mag_set_ui_2exp_si(ub, 1, DBL_MAX_EXP);
	for (j = 0; j < n; ++j) {
		if (mag_cmp(x + j, ub) < 0)
			y[j] = TERN(mag_get_d, lower, x + j);
		else {
			y[j] = R_PosInf;
			seenoob = 1;
		}
	}
	mag_clear(ub);
	if (seenoob) WARNING_OOB_DOUBLE;
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_mag_format(SEXP object, SEXP s_base,
                        SEXP s_sep, SEXP s_digits, SEXP s_rnd)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	int lower = isRndZ(asRnd(s_rnd, 0, __func__));
	mpfr_rnd_t rnd = (lower) ? MPFR_RNDZ : MPFR_RNDA;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	if (n) {
	mag_srcptr x = R_flint_get_pointer(object);
	mpfr_exp_t e__;
	slong p__;
	mpfr_uexp_t e, emax = 0;
	mpfr_prec_t p, pmax = 0;
	char work[4];
	unsigned int flags = 0;
	mpz_t z;
	mpfr_t f;
	arf_t tmp;
	MPFR_ERANGE_SET;
	mpz_init(z);
	mpfr_init2(f, FLINT_BITS);
	arf_init(tmp);
	for (j = 0; j < n; ++j) {
		arf_set_mag(tmp, x + j);
		arf_get_mpfr(f, tmp, rnd);
		if (mpfr_regular_p(f)) {
			flags |= 1;
			mpfr_get_str(work, &e__, base, 2, f, rnd);
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			if (e > emax)
				emax = e;
			p__ = arf_bits(tmp);
			p = (p__ <= MPFR_PREC_MAX) ? (mpfr_prec_t) p__ : MPFR_PREC_MAX;
			if (p > pmax)
				pmax = p;
		}
		else if (mpfr_zero_p(f))
			flags |= 1;
	}

	if (flags & 1) {

	mpz_set_ui(z, emax);
	mpfr_set_prec(f, (pmax == 0) ? 1 : pmax);
	if (digits == 0)
		digits = (pmax == 0) ? 1 : mpfr_get_str_ndigits(abase, pmax);

	size_t ns, nc,
		ncrad = (digits > 1) ? 1 : 0,
		ncman = digits,
		ncsep = strlen(sep),
		ncexp = mpz_sizeinbase(z, abase),
		ncmax = ncrad + ncman + ncsep + 1;
	char *buffer = R_alloc(ncmax + ncexp + 1, sizeof(char));
	mpz_get_str(buffer, base, z);
	ncexp = strlen(buffer);
	ncmax += ncexp;
	char
		*bufman = buffer + (ncrad),
		*bufsep = buffer + (ncrad + ncman),
		*bufexp = buffer + (ncrad + ncman + ncsep + 1),
		*bufnan = buffer + (ncmax - 3);
	buffer[ncmax] = '\0';

	memset(buffer, ' ', ncmax - 3);
	memcpy(bufnan, "Inf", 3);
	SEXP s_pos_inf = PROTECT(Rf_mkChar(buffer));
	memset(buffer, '0', ncmax);
	if (ncrad)
		bufman[0] = '.';
	memcpy(bufsep, sep, ncsep);
	bufexp[-1] = '+';
	SEXP s_zero    = PROTECT(Rf_mkChar(buffer));

	for (j = 0; j < n; ++j) {
		arf_set_mag(tmp, x + j);
		arf_get_mpfr(f, tmp, rnd);
		if (!mpfr_regular_p(f))
			SET_STRING_ELT(ans, (R_xlen_t) j,
			               (mpfr_zero_p(f)) ? s_zero : s_pos_inf);
		else {
			/* Mantissa */
			mpfr_get_str(bufman, &e__, base, ncman, f, rnd);
			/* Radix point */
			if (ncrad) {
				bufman[-1] = bufman[0];
				bufman[0] = '.';
			}
			/* Separator */
			bufsep[0] = sep[0];
			/* Exponent sign */
			bufexp[-1] = (e__ <= 0) ? '-' : '+';
			/* Exponent absolute value */
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			mpz_set_ui(z, e);
			nc = mpz_sizeinbase(z, abase);
			if (nc > ncexp)
				nc = ncexp;
			ns = ncexp - nc;
			if (ns > 0)
				memset(bufexp, '0', ns);
			mpz_get_str(bufexp + ns, base, z);
			if (bufexp[ncexp - 1] == '\0') {
				memmove(bufexp + ns + 1, bufexp + ns, nc);
				bufexp[ns] = '0';
			}
			SET_STRING_ELT(ans, (R_xlen_t) j, Rf_mkChar(buffer));
		}
	}

	UNPROTECT(2);

	} else {

	SEXP s_pos_inf = PROTECT(Rf_mkChar("Inf"));

	for (j = 0; j < n; ++j)
		SET_STRING_ELT(ans, (R_xlen_t) j, s_pos_inf);

	UNPROTECT(1);

	}

	mpz_clear(z);
	mpfr_clear(f);
	arf_clear(tmp);
	MPFR_ERANGE_RESET;
	}
	setDDNN1(ans, object);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_mag_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	R_flint_ops2_t op = ops2match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops2info(op);
	mp_limb_t jx, jy, jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	mag_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	int dz[3];
	info = checkConformable(s_x, s_y, nx, ny, info, dz);
	int lower = isRndZ(asRnd(R_NilValue, 0, __func__));
	switch (op) {
	case R_FLINT_OPS2_ADD:
	case R_FLINT_OPS2_SUB:
	case R_FLINT_OPS2_MUL:
	case R_FLINT_OPS2_FDR:
	case R_FLINT_OPS2_FDQ:
	case R_FLINT_OPS2_DIV:
	case R_FLINT_OPS2_POW:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_MAG, 0, nz));
		mag_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS2_ADD:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				TERN(mag_add, lower, z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_SUB:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				if (mag_cmp(x + jx, y + jy) >= 0)
				TERN(mag_sub, lower, z + jz, x + jx, y + jy);
				else
				TERN(mag_sub, lower, z + jz, y + jy, x + jx);
			break;
		case R_FLINT_OPS2_MUL:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				TERN(mag_mul, lower, z + jz, x + jx, y + jy);
			break;
		case R_FLINT_OPS2_FDR:
		{
			int cmp;
			mag_t q, t;
			fmpz_t f;
			mag_init(q);
			mag_init(t);
			fmpz_init(f);
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny) {
				if (mag_is_zero(y + jy))
					Rf_error(_("NaN is not representable by \"%s\""), "mag");
				cmp = mag_cmp(x + jx, y + jy);
				if (cmp == 0)
					mag_zero(z + jz);
				else if (cmp < 0)
					mag_set(z + jz, x + jx);
				else {
					mag_div_lower(q, x + jx, y + jy);
					mag_get_fmpz_lower(f, q);
					mag_mul_fmpz_lower(t, q, f);
					mag_sub(z + jz, x + jx, t);
				}
			}
			mag_clear(q);
			mag_clear(t);
			fmpz_clear(f);
			break;
		}
		case R_FLINT_OPS2_FDQ:
		{
			int cmp;
			mag_t q;
			fmpz_t f;
			mag_init(q);
			fmpz_init(f);
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny) {
				if (mag_is_zero(y + jy))
					Rf_error(_("NaN is not representable by \"%s\""), "mag");
				cmp = mag_cmp(x + jx, y + jy);
				if (cmp == 0)
					mag_one(z + jz);
				else if (cmp < 0)
					mag_zero(z + jz);
				else {
					mag_div(q, x + jx, y + jy);
					mag_get_fmpz_lower(f, q);
					mag_set_fmpz(z + jz, f);
				}
			}
			mag_clear(q);
			fmpz_clear(f);
			break;
		}
		case R_FLINT_OPS2_DIV:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny) {
				if (mag_is_special(x + jx) &&
				    mag_is_special(y + jy) &&
				    (mag_is_zero(x + jx) != 0) ==
				    (mag_is_zero(y + jy) != 0))
					Rf_error(_("NaN is not representable by \"%s\""), "mag");
				TERN(mag_div, lower, z + jz, x + jx, y + jy);
			}
			break;
		case R_FLINT_OPS2_POW:
		{
			mag_srcptr b, e;
			mag_t a;
			mag_init(a);
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny) {
				b = x + jx;
				e = y + jy;
				if (mag_is_zero(e) || mag_cmp_2exp_si(b, 0) == 0)
					/* b^0, 1^e = 1 */
					mag_one(z + jz);
				else if (mag_cmp_2exp_si(e, 0) == 0)
					/* b^1 = b */
					mag_set(z + jz, b);
				else if (mag_is_inf(e) || mag_is_special(b)) {
					/* b^Inf, 0^e, Inf^e = 0|Inf */
					if (mag_cmp_2exp_si(b, 0) < 0)
						mag_zero(z + jz);
					else
						mag_inf(z + jz);
				}
				else if (mag_cmp_2exp_si(b, 0) > 0) {
					/* b^e = exp(e * log(b)) */
					TERN(mag_log, lower, a, b);
					TERN(mag_mul, lower, a, e, a);
					TERN(mag_exp, lower, z + jz, a);
				}
				else {
					/* b^e = exp(-(e * -log(b))) */
					TERN(mag_neg_log, !lower, a, b);
					TERN(mag_mul, !lower, a, e, a);
					TERN(mag_expinv, lower, z + jz, a);
				}
			}
			mag_clear(a);
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
				z[jz] = mag_equal(x + jx, y + jy) != 0;
			break;
		case R_FLINT_OPS2_NEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = mag_equal(x + jx, y + jy) == 0;
			break;
		case R_FLINT_OPS2_L:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = mag_cmp(x + jx, y + jy) < 0;
			break;
		case R_FLINT_OPS2_G:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = mag_cmp(x + jx, y + jy) > 0;
			break;
		case R_FLINT_OPS2_LEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = mag_cmp(x + jx, y + jy) <= 0;
			break;
		case R_FLINT_OPS2_GEQ:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = mag_cmp(x + jx, y + jy) >= 0;
			break;
		case R_FLINT_OPS2_AND:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = !mag_is_zero(x + jx) && !mag_is_zero(y + jy);
			break;
		case R_FLINT_OPS2_OR:
			FOR_RECYCLE2(jz, nz, jx, nx, jy, ny)
				z[jz] = !mag_is_zero(x + jx) || !mag_is_zero(y + jy);
			break;
		default: /* -Wswitch */
			break;
		}
		setDDNN2(ans, s_x, s_y, nz, nx, ny, info);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "mag");
		return R_NilValue;
	}
}

SEXP R_flint_mag_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	R_flint_ops1_t op = ops1match(CHAR(STRING_ELT(s_op, 0)));
	int info = ops1info(op);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	mag_srcptr x = R_flint_get_pointer(s_x);
	int lower = isRndZ(asRnd(R_NilValue, 0, __func__));
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
	case R_FLINT_OPS1_ROUND:
	case R_FLINT_OPS1_SIGNIF:
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_MAG, 0, nz));
		mag_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_PLUS:
		case R_FLINT_OPS1_MINUS:
		case R_FLINT_OPS1_CONJ:
		case R_FLINT_OPS1_REAL:
		case R_FLINT_OPS1_MOD:
		case R_FLINT_OPS1_ABS:
			for (jz = 0; jz < nz; ++jz)
				mag_set(z + jz, x + jz);
			break;
		case R_FLINT_OPS1_IMAG:
		case R_FLINT_OPS1_ARG:
			for (jz = 0; jz < nz; ++jz)
				mag_zero(z + jz);
			break;
		case R_FLINT_OPS1_SIGN:
			for (jz = 0; jz < nz; ++jz)
				if (mag_is_zero(x + jz))
					mag_zero(z + jz);
				else
					mag_one(z + jz);
			break;
		case R_FLINT_OPS1_SQRT:
			for (jz = 0; jz < nz; ++jz)
				TERN(mag_sqrt, lower, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_FLOOR:
		case R_FLINT_OPS1_TRUNC:
		{
			fmpz_t r;
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_is_inf(x + jz))
					mag_inf(z + jz);
				if (mag_is_zero(x + jz))
					mag_zero(z + jz);
				else if (fmpz_cmp_si(MAG_EXPREF(x + jz),        0) <= 0)
					mag_zero(z + jz);
				else if (fmpz_cmp_si(MAG_EXPREF(x + jz), MAG_BITS) >= 0)
					mag_set(z + jz, x + jz);
				else {
					mag_get_fmpz_lower(r, x + jz);
					TERN(mag_set_fmpz, lower, z + jz, r);
				}
			}
			fmpz_clear(r);
			break;
		}
		case R_FLINT_OPS1_CEILING:
		{
			fmpz_t r;
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_is_inf(x + jz))
					mag_inf(z + jz);
				if (mag_is_zero(x + jz))
					mag_zero(z + jz);
				else if (fmpz_cmp_si(MAG_EXPREF(x + jz),        0) <= 0)
					mag_one(z + jz);
				else if (fmpz_cmp_si(MAG_EXPREF(x + jz), MAG_BITS) >= 0)
					mag_set(z + jz, x + jz);
				else {
					mag_get_fmpz(r, x + jz);
					TERN(mag_set_fmpz, lower, z + jz, r);
				}
			}
			fmpz_clear(r);
			break;
		}
		case R_FLINT_OPS1_CUMMIN:
			if (nz) {
			mag_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				mag_min(z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMMAX:
			if (nz) {
			mag_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				mag_max(z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMSUM:
			if (nz) {
			mag_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				TERN(mag_add, lower, z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_CUMPROD:
			if (nz) {
			mag_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				TERN(mag_mul, lower, z + jz, z + jz - 1, x + jz);
			}
			break;
		case R_FLINT_OPS1_LOG:
			if (s_dots == R_NilValue)
			for (jz = 0; jz < nz; ++jz) {
				if (mag_cmp_2exp_si(x + jz, 0) >= 0)
					TERN(mag_log    , lower, z + jz, x + jz);
				else
					TERN(mag_neg_log, lower, z + jz, x + jz);
			}
			else {
			SEXP s_base = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_base) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "base", CHAR(STRING_ELT(s_op, 0)));
			arf_srcptr base = R_flint_get_pointer(s_base);
			int status = arf_is_nan(base) || arf_sgn(base) < 0;
			if (status)
				Rf_error(_("NaN is not representable by \"%s\""), "mag");
			mag_t t;
			mag_init(t);
			if (arf_cmp_2exp_si(base, 0) >= 0) {
				TERN(arf_get_mag, !lower, t, base);
				TERN(mag_log    , !lower, t, t);
			} else {
				TERN(arf_get_mag,  lower, t, base);
				TERN(mag_neg_log, !lower, t, t);
			}
			for (jz = 0; jz < nz && !status; ++jz) {
				if (mag_cmp_2exp_si(x + jz, 0) >= 0)
					TERN(mag_log    , lower, z + jz, x + jz);
				else
					TERN(mag_neg_log, lower, z + jz, x + jz);
				status =
					mag_is_special(z + jz) &&
					mag_is_special(t     ) &&
					(mag_is_zero(z + jz) != 0) ==
					(mag_is_zero(t     ) != 0);
				if (!status)
				TERN(mag_div, lower, z + jz, z + jz, t);
			}
			mag_clear(t);
			if (status)
				Rf_error(_("NaN is not representable by \"%s\""), "mag");
			}
			break;
		case R_FLINT_OPS1_LOG10:
		case R_FLINT_OPS1_LOG2:
		{
			ulong b = (op == R_FLINT_OPS1_LOG10) ? 10 : 2;
			mag_t t;
			mag_init(t);
			TERN(mag_set_ui, !lower, t, b);
			TERN(mag_log   , !lower, t, t);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_cmp_2exp_si(x + jz, 0) >= 0)
					TERN(mag_log    , lower, z + jz, x + jz);
				else
					TERN(mag_neg_log, lower, z + jz, x + jz);
				TERN(mag_div, lower, z + jz, z + jz, t);
			}
			mag_clear(t);
			break;
		}
		case R_FLINT_OPS1_LOG1P:
			for (jz = 0; jz < nz; ++jz)
				TERN(mag_log1p, lower, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_EXP:
			for (jz = 0; jz < nz; ++jz)
				TERN(mag_exp, lower, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_EXPM1:
			for (jz = 0; jz < nz; ++jz)
				TERN(mag_expm1, lower, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ROUND:
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
			slong prec = MAG_BITS << 1;
			arf_rnd_t rnd = (lower) ? ARF_RND_DOWN : ARF_RND_UP;
			fmpz_t p, q;
			arf_t s;
			fmpz_init(p);
			fmpz_init(q);
			arf_init(s);
			fmpz_set_si(p, 10);
			if (digits >= 0) {
			/* f ~ c/10^+digits   <=>   c ~ f*10^+digits */
			fmpz_pow_ui(p, p, (ulong) digits);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_is_special(x + jz))
				mag_set(z + jz, x + jz);
				else {
				arf_set_mag(s, x + jz);
				arf_mul_fmpz(s, s, p, ARF_PREC_EXACT, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(s, q, p, prec, rnd);
				TERN(arf_get_mag, lower, z + jz, s);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_is_special(x + jz))
				mag_set(z + jz, x + jz);
				else {
				arf_set_mag(s, x + jz);
				arf_div_fmpz(s, s, p, prec, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				TERN(mag_set_fmpz, lower, z + jz, q);
				}
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			arf_clear(s);
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
			slong prec = MAG_BITS << 1;
			arf_rnd_t rnd = (lower) ? ARF_RND_DOWN : ARF_RND_UP;
			fmpq_t a;
			fmpz_t p, q, r;
			arf_t s;
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			arf_init(s);
			for (jz = 0; jz < nz; ++jz) {
				if (mag_is_special(x + jz))
				mag_set(z + jz, x + jz);
				else {
				mag_get_fmpq(a, x + jz);
				clog = fmpq_clog_ui(a, 10);
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
				arf_fmpz_div_fmpz(s, q, p, prec, rnd);
				TERN(arf_get_mag, lower, z + jz, s);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				TERN(mag_set_fmpz, lower, z + jz, q);
				}
				}
			}
			fmpq_clear(a);
			fmpz_clear(p);
			fmpz_clear(q);
			fmpz_clear(r);
			arf_clear(s);
			break;
		}
		default: /* -Wswitch */
			break;
		}
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
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
	{
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_MAG, 0, nz));
		mag_ptr z = R_flint_get_pointer(ans);
		int status = 0;
		arb_t zb, xb;
		arb_init(zb);
		arb_init(xb);

#define WRAP(op, z, x) \
		do { \
			arf_set_mag(arb_midref(xb), x); \
			op(zb, xb, MAG_BITS << 1); \
			status = arf_is_nan(arb_midref(zb)); \
			if (!status) \
			TERN(arf_get_mag, lower, z, arb_midref(zb)); \
		} while (0)

		switch (op) {
		case R_FLINT_OPS1_COS:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_cos, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_COSPI:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_cos_pi, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ACOS:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_acos, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_COSH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_cosh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ACOSH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_acosh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_SIN:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_sin, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_SINPI:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_sin_pi, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ASIN:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_asin, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_SINH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_sinh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ASINH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_asinh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_TAN:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_tan, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_TANPI:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_tan_pi, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ATAN:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_atan, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_TANH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_tanh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_ATANH:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_atanh, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_GAMMA:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_gamma, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_LGAMMA:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_lgamma, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_2GAMMA:
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_digamma, z + jz, x + jz);
			break;
		case R_FLINT_OPS1_3GAMMA:
		{
			arb_t s;
			arb_init(s);
			arb_set_si(s, 1);
#define arb_trigamma(z, x, prec) arb_polygamma(z, s, x, prec)
			for (jz = 0; jz < nz && !status; ++jz)
				WRAP(arb_trigamma, z + jz, x + jz);
#undef arb_trigamma
			arb_clear(s);
			break;
		}
		default: /* -Wswitch */
			break;
		}

#undef WRAP

		arb_clear(zb);
		arb_clear(xb);
		if (status)
			Rf_error(_("NaN is not representable by \"%s\""), "mag");
		setDDNN1(ans, s_x);
		UNPROTECT(1);
		return ans;
	}
	case R_FLINT_OPS1_MEAN:
		if (nx == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "x", CHAR(STRING_ELT(s_op, 0)));
	case R_FLINT_OPS1_MIN:
	case R_FLINT_OPS1_MAX:
	case R_FLINT_OPS1_RANGE:
	case R_FLINT_OPS1_SUM:
	case R_FLINT_OPS1_PROD:
	{
		nz = (op == R_FLINT_OPS1_RANGE) ? 2 : 1;
		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_MAG, 0, nz));
		mag_ptr z = R_flint_get_pointer(ans);
		switch (op) {
		case R_FLINT_OPS1_MIN:
			mag_inf(z);
			for (jx = 0; jx < nx; ++jx)
				if (mag_cmp(z, x + jx) > 0)
					mag_set(z, x + jx);
			break;
		case R_FLINT_OPS1_MAX:
			mag_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (mag_cmp(z, x + jx) < 0)
					mag_set(z, x + jx);
			break;
		case R_FLINT_OPS1_RANGE:
		{
			mag_inf(z);
#ifdef R_FLINT_USE_NAIVE_RANGE
			/* MJ: GCC 14 gives [-Wstringop-overread].  Why?? */
			mag_zero(z + 1);
			for (jx = 0; jx < nx; ++jx)
				if (mag_cmp(z, x + jx) > 0)
					mag_set(z, x + jx);
				else if (mag_cmp(z + 1, x + jx) < 0)
					mag_set(z + 1, x + jx);
#else
			mag_t t;
			mag_init(t);
			mag_zero(t);
			for (jx = 0; jx < nx; ++jx)
				if (mag_cmp(z, x + jx) > 0)
					mag_set(z, x + jx);
				else if (mag_cmp(t, x + jx) < 0)
					mag_set(t, x + jx);
			mag_set(z + 1, t);
			mag_clear(t);
#endif
			break;
		}
		case R_FLINT_OPS1_SUM:
			mag_zero(z);
			for (jx = 0; jx < nx; ++jx)
				TERN(mag_add, lower, z, z, x + jx);
			break;
		case R_FLINT_OPS1_PROD:
			mag_one(z);
			for (jx = 0; jx < nx; ++jx)
				TERN(mag_mul, lower, z, z, x + jx);
			break;
		case R_FLINT_OPS1_MEAN:
			mag_zero(z);
			for (jx = 0; jx < nx; ++jx)
				TERN(mag_add, lower, z, z, x + jx);
			TERN(mag_div_ui, lower, z, z, nx);
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
			for (jx = 0; jx < nx &&  mag_is_zero(x + jx); ++jx) ;
			z[0] = jx <  nx;
			break;
		case R_FLINT_OPS1_ALL:
			for (jx = 0; jx < nx && !mag_is_zero(x + jx); ++jx) ;
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
			for (jx = 1; jx < nx && mag_cmp(x, x + 1) <  0; ++jx, ++x) ;
			else
			for (jx = 1; jx < nx && mag_cmp(x, x + 1) <= 0; ++jx, ++x) ;
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
			for (jz = 0; jz < nz; ++jz)
				z[jz] = 0;
			break;
		case R_FLINT_OPS1_ISINF:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = mag_is_inf(x + jz) != 0;
			break;
		case R_FLINT_OPS1_ISNUM:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = mag_is_finite(x + jz) != 0;
			break;
		case R_FLINT_OPS1_NOT:
			for (jz = 0; jz < nz; ++jz)
				z[jz] = mag_is_zero(x + jz) != 0;
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

		SEXP ans = PROTECT(newFlint(R_FLINT_CLASS_MAG, 0, nz));
		mag_ptr z = R_flint_get_pointer(ans);
		jx = 0;
		if (byrow) {
			for (jz = 0; jz < nz; ++jz)
				mag_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					TERN(mag_add, lower, z + jz, z + jz, x + jx);
			if (domean)
			for (jz = 0; jz < nz; ++jz)
				TERN(mag_div_ui, lower, z + jz, z + jz, nt);
		} else {
			for (jz = 0; jz < nz; ++jz) {
				mag_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					TERN(mag_add, lower, z + jz, z + jz, x + jx);
				if (domean)
				TERN(mag_div_ui, lower, z + jz, z + jz, nt);
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
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "mag");
		return R_NilValue;
	}
}
