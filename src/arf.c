#include "flint.h"

arf_rnd_t remapRnd(mpfr_rnd_t rnd)
{
	switch (rnd) {
	case MPFR_RNDN:
		return ARF_RND_NEAR;
	case MPFR_RNDZ:
		return ARF_RND_DOWN;
	case MPFR_RNDU:
		return ARF_RND_CEIL;
	case MPFR_RNDD:
		return ARF_RND_FLOOR;
	case MPFR_RNDA:
		return ARF_RND_UP;
	default:
		Rf_error(_("should never happen ..."));
		return -1;
	}
}

void R_flint_arf_finalize(SEXP x)
{
	arf_ptr p = R_ExternalPtrAddr(x);
	if (p) {
	mp_limb_t j, n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(R_ExternalPtrProtected(x)));
	for (j = 0; j < n; ++j)
		arf_clear(p + j);
	flint_free(p);
	R_ClearExternalPtr(x);
	}
	return;
}

SEXP R_flint_arf_initialize(SEXP object, SEXP s_x, SEXP s_length,
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
	arf_ptr y = (ny) ? flint_calloc(ny, sizeof(arf_t)) : 0;
	R_flint_set(object, y, ny, (R_CFinalizer_t) &R_flint_arf_finalize);
	switch (TYPEOF(s_x)) {
	case NILSXP:
		for (jy = 0; jy < ny; ++jy)
			arf_zero(y + jy);
		break;
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(s_x);
		for (jy = 0; jy < ny; ++jy)
			arf_set_ui(y + jy, x[jy % nx]);
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (x[jy % nx] == NA_LOGICAL)
			arf_nan(y + jy);
			else
			arf_set_si(y + jy, x[jy % nx]);
		}
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(s_x);
		for (jy = 0; jy < ny; ++jy) {
			if (x[jy % nx] == NA_INTEGER)
			arf_nan(y + jy);
			else
			arf_set_si(y + jy, x[jy % nx]);
		}
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(s_x);
		for (jy = 0; jy < ny; ++jy)
			arf_set_d(y + jy, x[jy % nx]);
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(s_x);
		for (jy = 0; jy < ny; ++jy)
			arf_set_d(y + jy, x[jy % nx].r);
		break;
	}
	case STRSXP:
	{
		mpfr_prec_t prec = asPrec(R_NilValue, __func__);
		mpfr_rnd_t rnd = asRnd(R_NilValue, __func__);
		mpfr_t m;
		mpfr_init2(m, prec);
		const char *s;
		char *t;
		for (jy = 0; jy < ny; ++jy) {
			s = CHAR(STRING_ELT(s_x, (R_xlen_t) (jy % nx)));
			mpfr_strtofr(m, s, &t, 0, rnd);
			if (t <= s)
				break;
			s = t;
			while (isspace(*s))
				s++;
			if (*s != '\0')
				break;
			arf_set_mpfr(y + jy, m);
		}
		mpfr_clear(m);
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
				arf_set_ui(y + jy, x[jy % nx]);
			break;
		}
		case R_FLINT_CLASS_SLONG:
		{
			const slong *x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				arf_set_si(y + jy, x[jy % nx]);
			break;
		}
		case R_FLINT_CLASS_FMPZ:
		{
			const fmpz *x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				arf_set_fmpz(y + jy, x + jy % nx);
			break;
		}
		case R_FLINT_CLASS_FMPQ:
		{
			const fmpq *x = R_flint_get_pointer(s_x);
			slong prec = asPrec(R_NilValue, __func__);
			arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
			for (jy = 0; jy < ny; ++jy)
				arf_fmpz_div_fmpz(y + jy, fmpq_numref(x + jy % nx), fmpq_denref(x + jy % nx), prec, rnd);
			break;
		}
		case R_FLINT_CLASS_MAG:
		{
			mag_srcptr x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				arf_set_mag(y + jy, x + jy % nx);
			break;
		}
		case R_FLINT_CLASS_ARF:
		{
			arf_srcptr x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				arf_set(y + jy, x + jy % nx);
			break;
		}
		case R_FLINT_CLASS_ACF:
		{
			acf_srcptr x = R_flint_get_pointer(s_x);
			for (jy = 0; jy < ny; ++jy)
				arf_set(y + jy, acf_realref(x + jy % nx));
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

SEXP R_flint_arf_atomic(SEXP object)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	SEXP ans = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t) n));
	arf_srcptr x = R_flint_get_pointer(object);
	double *y = REAL(ans);
	arf_t lb, ub;
	arf_init(lb);
	arf_init(ub);
	arf_set_d(ub, DBL_MAX);
	arf_neg(lb, ub);
	int w = 1;
	for (j = 0; j < n; ++j) {
		if (arf_is_nan(x + j))
			y[j] = R_NaN;
		else if (arf_cmp(x + j, lb) >= 0 && arf_cmp(x + j, ub) <= 0)
			y[j] = arf_get_d(x + j, rnd);
		else {
			y[j] = (arf_sgn(x + j) < 0) ? R_NegInf : R_PosInf;
			WARNING_OOB_DOUBLE(w);
		}
	}
	arf_clear(lb);
	arf_clear(ub);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arf_format(SEXP object, SEXP s_base,
                        SEXP s_digits, SEXP s_sep, SEXP s_rnd)
{
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, R_XLEN_T_MAX);
	int base = asBase(s_base, __func__), abase = (base < 0) ? -base : base;
	size_t digits = asDigits(s_digits, __func__);
	const char *sep = asSep(s_sep, __func__);
	mpfr_rnd_t rnd = asRnd(s_rnd, __func__);
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) n));
	if (n) {
	arf_srcptr x = R_flint_get_pointer(object);
	mpfr_exp_t e__;
	slong p__;
	mpfr_uexp_t e, emax = 0;
	mpfr_prec_t p, pmax = 0;
	char work[4];
	unsigned int flags = 0;
	mpz_t z;
	mpfr_t f;
	MPFR_ERANGE_SET;
	mpz_init(z);
	mpfr_init2(f, FLINT_BITS);
	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		if (mpfr_regular_p(f)) {
			flags |= 1;
			if (mpfr_sgn(f) < 0)
				flags |= 2;
			mpfr_get_str(work, &e__, base, 2, f, rnd);
			e = (e__ <= 0) ? (mpfr_uexp_t) -(e__ + 1) + 2 : (mpfr_uexp_t) (e__ - 1);
			if (e > emax)
				emax = e;
			p__ = arf_bits(x + j);
			p = (p__ <= MPFR_PREC_MAX) ? (mpfr_prec_t) p__ : MPFR_PREC_MAX;
			if (p > pmax)
				pmax = p;
		}
		else if (mpfr_zero_p(f))
			flags |= 1;
		else if (mpfr_inf_p(f) && mpfr_sgn(f) < 0)
			flags |= 4;
	}

	if (flags & 1) {

	mpz_set_ui(z, emax);
	mpfr_set_prec(f, (pmax == 0) ? 1 : pmax);
	if (digits == 0)
		digits = (pmax == 0) ? 1 : mpfr_get_str_ndigits(abase, pmax);

	size_t ns, nc,
		ncsgn = (flags & 2) ? 1 : 0,
		ncrad = (digits > 1) ? 1 : 0,
		ncman = digits,
		ncsep = strlen(sep),
		ncexp = mpz_sizeinbase(z, abase),
		ncmax = ncsgn + ncrad + ncman + ncsep + 1;
	char *buffer = R_alloc(ncmax + ncexp + 1, sizeof(char));
	mpz_get_str(buffer, base, z);
	ncexp = strlen(buffer);
	ncmax += ncexp;
	char
		*bufman = buffer + (ncsgn + ncrad),
		*bufsep = buffer + (ncsgn + ncrad + ncman),
		*bufexp = buffer + (ncsgn + ncrad + ncman + ncsep + 1),
		*bufnan = buffer + (ncmax - 4);
	buffer[ncmax] = '\0';

	memset(buffer, ' ', ncmax - 4);
	memcpy(bufnan, "-Inf", 4);
	SEXP s_neg_inf = PROTECT(Rf_mkChar(buffer));
	memcpy(bufnan, " Inf", 4);
	SEXP s_pos_inf = PROTECT(Rf_mkChar(buffer));
	memcpy(bufnan, " NaN", 4);
	SEXP s_nan     = PROTECT(Rf_mkChar(buffer));
	memset(buffer, '0', ncmax);
	if (ncsgn)
		buffer[0] = ' ';
	if (ncrad)
		bufman[0] = '.';
	memcpy(bufsep, sep, ncsep);
	bufexp[-1] = '+';
	SEXP s_zero    = PROTECT(Rf_mkChar(buffer));

	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		if (!mpfr_regular_p(f))
			SET_STRING_ELT(ans, (R_xlen_t) j,
			               (mpfr_zero_p(f)) ? s_zero :
			               (mpfr_nan_p(f)) ? s_nan :
			               (mpfr_sgn(f) < 0) ? s_neg_inf : s_pos_inf);
		else {
			/* Sign */
			if (ncsgn)
				buffer[0] = (mpfr_sgn(f) < 0) ? '-' : ' ';
			/* Mantissa */
			mpfr_abs(f, f, rnd);
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

	UNPROTECT(4);

	} else {

	SEXP
		s_neg_inf = PROTECT(Rf_mkChar(              "-Inf"        )),
		s_pos_inf = PROTECT(Rf_mkChar((flags & 4) ? " Inf" : "Inf")),
		s_nan     = PROTECT(Rf_mkChar((flags & 4) ? " NaN" : "NaN"));

	for (j = 0; j < n; ++j) {
		arf_get_mpfr(f, x + j, rnd);
		SET_STRING_ELT(ans, (R_xlen_t) j,
		               (mpfr_nan_p(f)) ? s_nan :
		               (mpfr_sgn(f) < 0) ? s_neg_inf : s_pos_inf);
	}

	UNPROTECT(3);

	}

	mpz_clear(z);
	mpfr_clear(f);
	MPFR_ERANGE_RESET;
	}
	setDDNN1(ans, object);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_arf_ops2(SEXP s_op, SEXP s_x, SEXP s_y, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops2);
	mp_limb_t jz,
		nx = R_flint_get_length(s_x),
		ny = R_flint_get_length(s_y),
		nz = RECYCLE2(nx, ny);
	arf_srcptr
		x = R_flint_get_pointer(s_x),
		y = R_flint_get_pointer(s_y);
	int dz[3];
	int mop = checkConformable(s_x, s_y, nx, ny, matrixop(op), dz);
	if (mop >= 0) nz = (mp_limb_t) dz[0] * (mp_limb_t) dz[1];
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	switch (op) {
	case  1: /*   "+" */
	case  2: /*   "-" */
	case  3: /*   "*" */
	case  6: /*   "/" */
	{
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case 1: /*   "+" */
			for (jz = 0; jz < nz; ++jz)
				arf_add(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
		case 2: /*   "-" */
			for (jz = 0; jz < nz; ++jz)
				arf_sub(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
		case 3: /*   "*" */
			for (jz = 0; jz < nz; ++jz)
				arf_mul(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
			break;
		case 6: /*   "/" */
			for (jz = 0; jz < nz; ++jz)
				arf_div(z + jz, x + jz % nx, y + jz % ny, prec, rnd);
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
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_equal(x + jz % nx, y + jz % ny) != 0;
			break;
		case  9: /*  "!=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_equal(x + jz % nx, y + jz % ny) == 0;
			break;
		case 10: /*   "<" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_cmp(x + jz % nx, y + jz % ny) < 0;
			break;
		case 11: /*   ">" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_cmp(x + jz % nx, y + jz % ny) > 0;
			break;
		case 12: /*  "<=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_cmp(x + jz % nx, y + jz % ny) <= 0;
			break;
		case 13: /*  ">=" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_nan(x + jz % nx) || arf_is_nan(y + jz % ny))
				? NA_LOGICAL
				: arf_cmp(x + jz % nx, y + jz % ny) >= 0;
			break;
		case 14: /*   "&" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(arf_is_zero(x + jz % nx) || arf_is_zero(y + jz % ny))
				? 0
				:
				(arf_is_nan (x + jz % nx) || arf_is_nan (y + jz % ny))
				? NA_LOGICAL
				: 1;
			break;
		case 15: /*   "|" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] =
				(!(arf_is_nan(x + jz % nx) || arf_is_zero(x + jz % nx)) ||
				 !(arf_is_nan(y + jz % ny) || arf_is_zero(y + jz % ny)))
				? 1
				:
				(arf_is_nan (x + jz % nx) || arf_is_nan (y + jz % ny))
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

		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		int tx = (mop & 1) != 0, ty = (mop & 2) != 0, i, j;
	    mp_limb_t jx, jy, ja, jb;
		arb_mat_t mc, ma, mb;
		mc->c = mb->c = dz[0];
		mc->r = ma->r = dz[1];
		ma->c = mb->r = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (ny) ? flint_calloc(ny, sizeof(arb_t)) : 0;
		mb->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		if (ty) {
			ja = jy = 0;
			for (i = 0; i < ma->r; ++i, jy -= ny - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jy += ma->r)
					arf_set(arb_midref(ma->entries + ja), y + jy);
		}
		else
			for (ja = 0; ja < ny; ++ja)
				arf_set(arb_midref(ma->entries + ja), y + ja);
		if (tx) {
			jb = jx = 0;
			for (i = 0; i < mb->r; ++i, jx -= nx - 1)
				for (j = 0; j < mb->c; ++j, ++jb, jx += mb->r)
					arf_set(arb_midref(mb->entries + jb), x + jx);
		}
		else
			for (jb = 0; jb < nx; ++jb)
				arf_set(arb_midref(mb->entries + jb), x + jb);
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
		arb_mat_approx_mul(mc, ma, mb, prec);
		for (jz = 0; jz < nz; ++jz) {
			arf_set(z + jz, arb_midref(mc->entries + jz));
			arb_clear(mc->entries + jz);
		}
		for (ja = 0; ja < ny; ++ja)
			arb_clear(ma->entries + ja);
		for (jb = 0; jb < nx; ++jb)
			arb_clear(mb->entries + jb);
		flint_free(mc->entries);
		flint_free(ma->entries);
		flint_free(mb->entries);
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
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		int i, j, singular;
		mp_limb_t jx, jy, jc, ja, jb;
		arb_mat_t mc, ma, mb;
		mc->r = mb->r = dz[0];
		mc->c = mb->c = dz[1];
		ma->r = ma->c = dz[2];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		mb->entries = (ny) ? flint_calloc(ny, sizeof(arb_t)) : 0;
		if (op == 21)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				arf_set(arb_midref(ma->entries + ja), x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ja += ma->r - (++i))
				for (j = 0; j <= i; ++j, ++ja)
					arf_set(arb_midref(ma->entries + ja), x + ja);
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ja += (++i))
				for (j = i; j < ma->c; ++j, ++ja)
					arf_set(arb_midref(ma->entries + ja), x + ja);
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += (++i), jx = ja)
				for (j = i; j < ma->c; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ja += ma->c - (++i), jx = ja)
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
			break;
		}
		jb = jy = 0;
		for (i = 0; i < mb->r; ++i, jy -= ny - 1)
			for (j = 0; j < mb->c; ++j, ++jb, jy += mb->r)
				arf_set(arb_midref(mb->entries + jb), y + jy);
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
		else if ((uplo == 'U') == (op != 21)) {
			arb_mat_approx_solve_triu(mc, ma, mb, 0, prec);
			singular = 0;
		}
		else {
			arb_mat_approx_solve_tril(mc, ma, mb, 0, prec);
			singular = 0;
		}
		jc = jz = 0;
		for (j = 0; j < mc->c; ++j, jc -= nz - 1)
			for (i = 0; i < mc->r; ++i, ++jz, jc += mc->c) {
				arf_set(z + jz, arb_midref(mc->entries + jc));
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
		setDDNN2(ans, s_x, s_y, nz, nx, ny, mop);
		UNPROTECT(1);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
}

SEXP R_flint_arf_ops1(SEXP s_op, SEXP s_x, SEXP s_dots)
{
	size_t op = strmatch(CHAR(STRING_ELT(s_op, 0)), R_flint_ops1);
	mp_limb_t jx, jz, nx = R_flint_get_length(s_x), nz = nx;
	arf_srcptr x = R_flint_get_pointer(s_x);
	slong prec = asPrec(R_NilValue, __func__);
	arf_rnd_t rnd = remapRnd(asRnd(R_NilValue, __func__));
	switch (op) {
	case  1: /*       "+" */
	case  2: /*       "-" */
	case  8: /*    "Conj" */
	case  9: /*      "Re" */
	case 10: /*      "Im" */
	case 11: /*     "Mod" */
	case 12: /*     "Arg" */
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
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case  1: /*       "+" */
		case  8: /*    "Conj" */
		case  9: /*      "Re" */
			for (jz = 0; jz < nz; ++jz)
				arf_set(z + jz, x + jz);
			break;
		case  2: /*       "-" */
			for (jz = 0; jz < nz; ++jz)
				arf_neg(z + jz, x + jz);
			break;
		case 10: /*      "Im" */
			for (jz = 0; jz < nz; ++jz)
				arf_zero(z + jz);
			break;
		case 11: /*     "Mod" */
		case 13: /*     "abs" */
			for (jz = 0; jz < nz; ++jz)
				arf_abs(z + jz, x + jz);
			break;
		case 12: /*     "Arg" */
		{
			arb_t pi;
			arb_init(pi);
			arb_const_pi(pi, prec);
			for (jz = 0; jz < nz; ++jz)
				if (arf_is_nan(x + jz) || arf_is_zero(x + jz))
					arf_set(z + jz, x + jz);
				else if (arf_sgn(x + jz) > 0)
					arf_set(z + jz, arb_midref(pi));
				else
					arf_neg(z + jz, arb_midref(pi));
			arb_clear(pi);
			break;
		}
		case 14: /*    "sign" */
			for (jz = 0; jz < nz; ++jz)
				if (arf_is_nan(x + jz))
				arf_nan(z + jz);
				else
				arf_set_si(z + jz, arf_sgn(x + jz));
			break;
		case 15: /*    "sqrt" */
			for (jz = 0; jz < nz; ++jz)
				arf_sqrt(z + jz, x + jz, prec, rnd);
			break;
		case 16: /*   "floor" */
			for (jz = 0; jz < nz; ++jz)
				arf_floor(z + jz, x + jz);
			break;
		case 17: /* "ceiling" */
			for (jz = 0; jz < nz; ++jz)
				arf_ceil(z + jz, x + jz);
			break;
		case 18: /*   "trunc" */
			for (jz = 0; jz < nz; ++jz)
				if (arf_sgn(x + jz) >= 0)
				arf_floor(z + jz, x + jz);
				else
				arf_ceil(z + jz, x + jz);
			break;
		case 19: /*  "cummin" */
			if (nz) {
			arf_srcptr last = x;
			for (jz = 0; jz < nz && !arf_is_nan(x + jz); ++jz)
				arf_min(z + jz, last, x + jz);
			for (; jz < nz; ++jz)
				arf_nan(z + jz);
			}
			break;
		case 20: /*  "cummax" */
			if (nz) {
			arf_srcptr last = x;
			for (jz = 0; jz < nz && !arf_is_nan(x + jz); ++jz)
				arf_max(z + jz, last, x + jz);
			for (; jz < nz; ++jz)
				arf_nan(z + jz);
			}
			break;
		case 21: /*  "cumsum" */
			if (nz) {
			arf_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				arf_add(z + jz, z + jz - 1, x + jz, prec, rnd);
			}
			break;
		case 22: /* "cumprod" */
			if (nz)
			arf_set(z, x);
			for (jz = 1; jz < nz; ++jz)
				arf_mul(z + jz, z + jz - 1, x + jz, prec, rnd);
			break;
		case 48: /*   "round" */
		{
			SEXP s_digits = VECTOR_ELT(s_dots, 0);
			if (R_flint_get_length(s_digits) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "digits", CHAR(STRING_ELT(s_op, 0)));
			slong digits = ((slong *) R_flint_get_pointer(s_digits))[0];
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
				if (!arf_is_finite(x + jz))
				arf_set(z + jz, x + jz);
				else {
				arf_mul_fmpz(s, x + jz, p, ARF_PREC_EXACT, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				arf_fmpz_div_fmpz(z + jz, q, p, prec, rnd);
				}
			}
			} else {
			/* f ~ c*10^-digits   <=>   c ~ f/10^-digits */
			fmpz_pow_ui(p, p, (ulong) -1 - (ulong) digits + 1);
			for (jz = 0; jz < nz; ++jz) {
				if (!arf_is_finite(x + jz))
				arf_set(z + jz, x + jz);
				else {
				arf_div_fmpz(s, x + jz, p, prec, rnd);
				arf_get_fmpz(q, s, ARF_RND_NEAR);
				fmpz_mul(q, q, p);
				arf_set_fmpz(z + jz, q);
				}
			}
			}
			fmpz_clear(p);
			fmpz_clear(q);
			arf_clear(s);
			break;
		}
		case 49: /*  "signif" */
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
			fmpq_init(a);
			fmpz_init(p);
			fmpz_init(q);
			fmpz_init(r);
			for (jz = 0; jz < nz; ++jz) {
				if (!arf_is_finite(x + jz))
				arf_set(z + jz, x + jz);
				else {
				arf_get_fmpq(a, x + jz);
				fmpq_abs(a, a);
				clog = fmpq_clog_ui(a, 10);
				if (arf_sgn(x + jz) < 0)
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
				arf_fmpz_div_fmpz(z + jz, q, p, prec, rnd);
				} else {
				fmpz_pow_ui(p, p, (ulong) (clog - digits));
				fmpz_mul(fmpq_denref(a), fmpq_denref(a), p);
				fmpz_ndiv_qr(q, r, fmpq_numref(a), fmpq_denref(a));
				if (fmpz_cmp2abs(fmpq_denref(a), r) == 0 &&
				    fmpz_is_odd(q))
					fmpz_add_si(q, q, fmpz_sgn(r));
				fmpz_mul(q, q, p);
				arf_set_fmpz(z + jz, q);
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
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = flint_calloc(nz, sizeof(arf_t));
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		switch (op) {
		case 50: /*     "min" */
			arf_pos_inf(z);
			for (jx = 0; jx < nx; ++jx)
				if (!arf_is_nan(x + jx)) {
					if (arf_cmp(z, x + jx) > 0)
						arf_set(z, x + jx);
				}
				else if (!narm) {
					arf_nan(z);
					break;
				}
			break;
		case 51: /*     "max" */
			arf_neg_inf(z);
			for (jx = 0; jx < nx; ++jx)
				if (!arf_is_nan(x + jx)) {
					if (arf_cmp(z, x + jx) < 0)
						arf_set(z, x + jx);
				}
				else if (!narm) {
					arf_nan(z);
					break;
				}
			break;
		case 52: /*   "range" */
			arf_pos_inf(z);
			arf_neg_inf(z + 1);
			for (jx = 0; jx < nx; ++jx)
				if (!arf_is_nan(x + jx)) {
					if (arf_cmp(z, x + jx) > 0)
						arf_set(z, x + jx);
					if (arf_cmp(z + 1, x + jx) < 0)
						arf_set(z + 1, x + jx);
				}
				else if (!narm) {
					arf_nan(z);
					arf_nan(z + 1);
					break;
				}
			break;
		case 53: /*     "sum" */
			arf_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && arf_is_nan(x + jx)))
				arf_add(z, z, x + jx, prec, rnd);
			break;
		case 54: /*    "prod" */
			arf_one(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && arf_is_nan(x + jx)))
				arf_mul(z, z, x + jx, prec, rnd);
			break;
		case 55: /*    "mean" */
		{
			mp_limb_t c = nx;
			arf_zero(z);
			for (jx = 0; jx < nx; ++jx)
				if (!(narm && arf_is_nan(x + jx)))
				arf_add(z, z, x + jx, prec, rnd);
				else
				--c;
			if (c == 0)
			arf_nan(z);
			else
			arf_div_ui(z, z, c, prec, rnd);
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
		SEXP s_narm = VECTOR_ELT(s_dots, 0);
		if (XLENGTH(s_narm) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "na.rm", CHAR(STRING_ELT(s_op, 0)));
		int narm = LOGICAL_RO(s_narm)[0], anyna = 0;
		SEXP ans = PROTECT(Rf_allocVector(LGLSXP, 1));
		int *z = LOGICAL(ans);
		switch (op) {
		case 56: /*         "any" */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(x + jx))
					anyna = 1;
				else if (!arf_is_zero(x + jx))
					break;
			z[0] = (jx < nx) ? 1 : (!narm && anyna) ? NA_LOGICAL : 0;
			break;
		case 57: /*         "all" */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(x + jx))
					anyna = 1;
				else if (arf_is_zero(x + jx))
					break;
			z[0] = (jx < nx) ? 0 : (!narm && anyna) ? NA_LOGICAL : 1;
			break;
		case 58: /*       "anyNA" */
			for (jx = 0; jx < nx; ++jx)
				if (arf_is_nan(x + jx))
					break;
			z[0] = jx < nx;
			break;
		case 59: /* "is.unsorted" */
		{
			SEXP s_strict = VECTOR_ELT(s_dots, 1);
			if (XLENGTH(s_strict) == 0)
				Rf_error(_("'%s' of length zero in '%s'"),
				         "strictly", CHAR(STRING_ELT(s_op, 0)));
			int strict = LOGICAL_RO(s_strict)[0];
			arf_srcptr last = (void *) 0;
			if (strict)
			for (jx = 0; jx < nx; ++jx) {
				if (arf_is_nan(x + jx))
					anyna = 1;
				else if (!last)
					last = x + jx;
				else if (arf_cmp(last, x + jx) >= 0)
					break;
			}
			else
			for (jx = 0; jx < nx; ++jx) {
				if (arf_is_nan(x + jx))
					anyna = 1;
				else if (!last)
					last = x + jx;
				else if (arf_cmp(last, x + jx) >  0)
					break;
			}
			z[0] = (jx < nx) ? 1 : (!narm && anyna && nx > 1) ? NA_LOGICAL : 0;
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
			for (jz = 0; jz < nz; ++jz)
				z[jz] = arf_is_nan(x + jz) != 0;
			break;
		case  5: /* "is.infinite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = arf_is_inf(x + jz) != 0;
			break;
		case  6: /*   "is.finite" */
			for (jz = 0; jz < nz; ++jz)
				z[jz] = arf_is_finite(x + jz) != 0;
			break;
		case  7: /*           "!" */
			for (jz = 0; jz < nz; ++jz)
				if (arf_is_nan(x + jz))
				z[jz] = NA_LOGICAL;
				else
				z[jz] = arf_is_zero(x + jz) != 0;
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

		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		jx = 0;
		if (byrow) {
			mp_limb_t *c = 0;
			if (domean && nz) {
				c = (void *) R_alloc(nz, sizeof(mp_limb_t));
				memset(c, 0, nz * sizeof(mp_limb_t));
			}
			for (jz = 0; jz < nz; ++jz)
				arf_zero(z + jz);
			for (jt = 0; jt < nt; ++jt)
				for (jz = 0; jz < nz; ++jz, ++jx)
					if (!(narm && arf_is_nan(x + jx)))
						arf_add(z + jz, z + jz, x + jx, prec, rnd);
					else if (domean)
						++c[jz];
			if (domean) {
			for (jz = 0; jz < nz; ++jz)
				if (c[jz] == nt)
					arf_nan(z + jz);
				else
					arf_div_ui(z + jz, z + jz, nt - c[jz], prec, rnd);
			}
		} else {
			mp_limb_t c = 0;
			for (jz = 0; jz < nz; ++jz) {
				arf_zero(z + jz);
				for (jt = 0; jt < nt; ++jt, ++jx)
					if (!(narm && arf_is_nan(x + jx)))
						arf_add(z + jz, z + jz, x + jx, prec, rnd);
					else if (domean)
						++c;
				if (domean) {
					if (c == nt)
						arf_nan(z + jz);
					else
						arf_div_ui(z + jz, z + jz, nt - c, prec, rnd);
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
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		int i, j, singular;
		mp_limb_t jc, ja;
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		if (op == 64 || op == 65)
		switch (uplo) {
		case 'N':
			for (ja = 0; ja < nx; ++ja)
				arf_set(arb_midref(ma->entries + ja), x + ja);
			break;
		case 'U':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j <= i; ++j, ++ja)
					arf_set(arb_midref(ma->entries + ja), x + ja);
				for (; j < ma->c; ++j, ++ja)
					arf_zero(arb_midref(ma->entries + ja));
			}
			break;
		case 'L':
			ja = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					arf_zero(arb_midref(ma->entries + ja));
				for (; j < ma->c; ++j, ++ja)
					arf_set(arb_midref(ma->entries + ja), x + ja);
			}
			break;
		}
		else
		switch (uplo) {
		case 'N':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i, jx -= nx - 1)
				for (j = 0; j < ma->c; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
			break;
		case 'U':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				for (j = 0; j < i; ++j, ++ja)
					arf_zero(arb_midref(ma->entries + ja));
				jx = ja;
				for (; j < ma->c; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
			}
			break;
		case 'L':
			ja = jx = 0;
			for (i = 0; i < ma->r; ++i) {
				jx = ja;
				for (j = 0; j <= i; ++j, ++ja, jx += ma->r)
					arf_set(arb_midref(ma->entries + ja), x + jx);
				for (; j < ma->c; ++j, ++ja)
					arf_zero(arb_midref(ma->entries + ja));
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
		singular = !arb_mat_approx_inv(mc, ma, prec);
		for (jc = 0; jc < nz; ++jc) {
			arf_set(z + jc, arb_midref(mc->entries + jc));
			arb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			arb_clear(ma->entries + ja);
		flint_free(mc->entries);
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
	case 67: /*   "chol2inv" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		mp_limb_t jc, ja;
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		for (ja = 0; ja < nx; ++ja) {
			arf_set(arb_midref(ma->entries + ja), x + ja);
			mag_zero(arb_radref(ma->entries + ja));
		}
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
		for (jc = 0; jc < nx; ++jc) {
			arf_set(z + jc, arb_midref(mc->entries + jc));
			arb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			arb_clear(ma->entries + ja);
		flint_free(mc->entries);
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
	case 68: /*       "chol" */
	{
		SEXP dimz = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dz = 0;
		if (dimz == R_NilValue || XLENGTH(dimz) != 2 ||
		    (dz = INTEGER_RO(dimz), dz[0] != dz[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = (nz) ? flint_calloc(nz, sizeof(arf_t)) : 0;
		R_flint_set(ans, z, nz, (R_CFinalizer_t) &R_flint_arf_finalize);
		int posdef;
		mp_limb_t jc, ja;
		arb_mat_t mc, ma;
		mc->r = mc->c = ma->r = ma->c = dz[0];
		mc->entries = (nz) ? flint_calloc(nz, sizeof(arb_t)) : 0;
		ma->entries = (nx) ? flint_calloc(nx, sizeof(arb_t)) : 0;
		for (ja = 0; ja < nx; ++ja) {
			arf_set(arb_midref(ma->entries + ja), x + ja);
			mag_zero(arb_radref(ma->entries + ja));
		}
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
		for (jc = 0; jc < nx; ++jc) {
			arf_set(z + jc, arb_midref(mc->entries + jc));
			arb_clear(mc->entries + jc);
		}
		for (ja = 0; ja < nx; ++ja)
			arb_clear(ma->entries + ja);
		flint_free(mc->entries);
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
	case 69: /*        "det" */
	{
		SEXP dimx = PROTECT(R_do_slot(s_x, R_flint_symbol_dim));
		const int *dx = 0;
		if (dimx == R_NilValue || XLENGTH(dimx) != 2 ||
		    (dx = INTEGER_RO(dimx), dx[0] != dx[1]))
			Rf_error(_("'%s' is not a square matrix"),
			         "x");
		SEXP ans = PROTECT(newObject("arf"));
		arf_ptr z = flint_calloc(1, sizeof(arf_t));
		R_flint_set(ans, z, 1, (R_CFinalizer_t) &R_flint_arf_finalize);
		arb_mat_t mx;
		arb_t det;
		mx->r = mx->c = dx[0];
		mx->entries = (nx) ? flint_calloc((size_t) mx->r, sizeof(arb_t)) : 0;
		for (jx = 0; jx < nx; ++jx) {
			arf_set(arb_midref(mx->entries + jx), x + jx);
			mag_zero(arb_radref(mx->entries + jx));
		}
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		int i;
		mx->rows = (mx->r) ? flint_calloc((size_t) mx->r, sizeof(arb_ptr)) : 0;
		mx->rows[0] = mx->entries;
		for (i = 1; i < mx->r; ++i)
			mx->rows[i] = mx->rows[i - 1] + mx->c;
#else
		mx->stride = mx->c;
#endif
		arb_init(det);
		arb_mat_det(det, mx, prec);
		arf_set(z, arb_midref(det));
		flint_free(mx->entries);
#ifndef HAVE_FMPZ_MAT_STRUCT_STRIDE
		flint_free(mx->rows);
#endif
		arb_clear(det);
		UNPROTECT(2);
		return ans;
	}
	default:
		Rf_error(_("operation '%s' is not yet implemented for class \"%s\""),
		         CHAR(STRING_ELT(s_op, 0)), "arf");
		return R_NilValue;
	}
}
