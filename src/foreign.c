#include "flint.h"

static SEXP
	R_gmp_symbol_denominator = NULL,
	R_Rmpfr_symbol_prec = NULL,
	R_Rmpfr_symbol_sign = NULL,
	R_Rmpfr_symbol_exp  = NULL,
	R_Rmpfr_symbol_d    = NULL;

static void R_flint_coerce_init(void)
{
	if (!R_gmp_symbol_denominator)
		R_gmp_symbol_denominator = Rf_install("denominator");
	if (!R_Rmpfr_symbol_prec)
		R_Rmpfr_symbol_prec = Rf_install("prec");
	if (!R_Rmpfr_symbol_sign)
		R_Rmpfr_symbol_sign = Rf_install("sign");
	if (!R_Rmpfr_symbol_exp)
		R_Rmpfr_symbol_exp  = Rf_install("exp");
	if (!R_Rmpfr_symbol_d)
		R_Rmpfr_symbol_d    = Rf_install("d");
	return;
}

SEXP R_flint_coerce_bigz_fmpz(SEXP from)
{
	R_flint_coerce_init();
	const int *x = (void *) RAW(from);
	int h = *(x++);
	if (h < 0)
		Rf_error(_("invalid"));
	mp_limb_t j, n = (mp_limb_t) h;
	SEXP to = PROTECT(newFlint(R_FLINT_CLASS_FMPZ, 0, n));
	fmpz *y = R_flint_get_pointer(to);
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		if (x[0] <= 0) {
			mpz_clear(t);
			Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpz");
		}
		mpz_import(t, (size_t) x[0], 1, sizeof(int), 0, 0, x + 2);
		if (x[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(y + j, t);
		x += 2LL + x[0];
	}
	mpz_clear(t);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_coerce_fmpz_bigz(SEXP from)
{
	R_flint_coerce_init();
	mp_limb_t j, n = R_flint_get_length(from);
	if (n > INT_MAX)
		Rf_error(_("length would exceed maximum %d"),
		         INT_MAX);
	const fmpz *x = R_flint_get_pointer(from);
	size_t words, count = 0;
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		/* NB: 'bigz' uses one word for zero */
		fmpz_get_mpz(t, x + j);
		words = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		if (words > INT_MAX) {
			mpz_clear(t);
			Rf_error(_("raw length of element would exceed maximum %lld"),
			         (long long int) INT_MAX + 2);
		}
		count += words;
	}
	mpz_clear(t);
	count += 1 + 2 * n;
	if (count > R_XLEN_T_MAX / sizeof(int))
		Rf_error(_("raw length would exceed maximum %lld"),
		         (long long int) R_XLEN_T_MAX);
	SEXP to = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count * sizeof(int))));
	int *y = (void *) RAW(to);
	*(y++) = (int) n;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, x + j);
		words = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(y++) = (int) words;
		*(y++) = mpz_sgn(t);
		if (mpz_sgn(t) == 0)
		memset(y, 0, words * sizeof(int));
		else
		mpz_export(y, 0, 1, sizeof(int), 0, 0, t);
		y += words;
	}
	mpz_clear(t);
	SEXP class = PROTECT(Rf_mkString("bigz"));
	Rf_setAttrib(to, R_ClassSymbol, class);
	UNPROTECT(2);
	return to;
}

SEXP R_flint_coerce_bigq_fmpq(SEXP from)
{
	R_flint_coerce_init();
	SEXP
		sn = from,
		sd = PROTECT(Rf_getAttrib(sn, R_gmp_symbol_denominator));
	const int *xn = (void *) RAW(sn), *xd = (void *) RAW(sd);
	int h = *(xn++);
	if (h < 0 || h != *(xd++))
		Rf_error(_("invalid"));
	mp_limb_t j, n = (mp_limb_t) h;
	SEXP to = PROTECT(newFlint(R_FLINT_CLASS_FMPQ, 0, n));
	fmpq *y = R_flint_get_pointer(to);
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		if (xn[0] <= 0 || xd[0] <= 0) {
			mpz_clear(t);
			Rf_error(_("NaN, -Inf, Inf are not representable by \"%s\""), "fmpz");
		}

		mpz_import(t, (size_t) xn[0], 1, sizeof(int), 0, 0, xn + 2);
		if (xn[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(fmpq_numref(y + j), t);
		xn += 2LL + xn[0];

		mpz_import(t, (size_t) xd[0], 1, sizeof(int), 0, 0, xd + 2);
		if (xd[1] < 0)
			mpz_neg(t, t);
		fmpz_set_mpz(fmpq_denref(y + j), t);
		xd += 2LL + xd[0];

		if (mpz_sgn(t) == 0) {
			mpz_clear(t);
			Rf_error(_("zero denominator not valid in canonical \"%s\""), "fmpq");
		}
		fmpq_canonicalise(y + j);
	}
	mpz_clear(t);
	UNPROTECT(2);
	return to;

}

SEXP R_flint_coerce_fmpq_bigq(SEXP from)
{
	R_flint_coerce_init();
	mp_limb_t j, n = R_flint_get_length(from);
	if (n > INT_MAX)
		Rf_error(_("length would exceed maximum %d"),
		         INT_MAX);
	const fmpq *x = R_flint_get_pointer(from);
	size_t words[2], count[2] = { 0, 0 };
	mpz_t t;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		/* NB: 'bigq' uses one word for zero */
		fmpz_get_mpz(t, fmpq_numref(x + j));
		words[0] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		fmpz_get_mpz(t, fmpq_denref(x + j));
		words[1] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		if (words[0] > INT_MAX || words[1] > INT_MAX) {
			mpz_clear(t);
			Rf_error(_("raw length of vector element would exceed maximum %d"),
			         INT_MAX);
		}
		count[0] += words[0];
		count[1] += words[1];
	}
	mpz_clear(t);
	count[0] += 1 + 2 * n;
	count[1] += 1 + 2 * n;
	if (count[0] > R_XLEN_T_MAX / sizeof(int) ||
	    count[1] > R_XLEN_T_MAX / sizeof(int))
		Rf_error(_("raw length would exceed maximum %lld"),
		         (long long int) R_XLEN_T_MAX);
	SEXP
		sn = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count[0] * sizeof(int)))),
		sd = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) (count[1] * sizeof(int)))),
		to = sn;
	int *yn = (void *) RAW(sn), *yd = (void *) RAW(sd);
	*(yn++) = *(yd++) = (int) n;
	mpz_init(t);
	for (j = 0; j < n; ++j) {
		fmpz_get_mpz(t, fmpq_numref(x + j));
		words[0] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(yn++) = (int) words[0];
		*(yn++) = mpz_sgn(t);
		if (mpz_sgn(t) == 0)
		memset(yn, 0, words[0] * sizeof(int));
		else
		mpz_export(yn, 0, 1, sizeof(int), 0, 0, t);
		yn += words[0];

		fmpz_get_mpz(t, fmpq_denref(x + j));
		words[1] = (mpz_sizeinbase(t, 2) - 1)/(CHAR_BIT * sizeof(int)) + 1;
		*(yd++) = (int) words[1];
		*(yd++) = mpz_sgn(t);
		if (mpz_sgn(t) == 0)
		memset(yd, 0, words[1] * sizeof(int));
		else
		mpz_export(yd, 0, 1, sizeof(int), 0, 0, t);
		yd += words[1];
	}
	mpz_clear(t);
	SEXP class = PROTECT(Rf_mkString("bigq"));
	Rf_setAttrib(to, R_ClassSymbol, class);
	Rf_setAttrib(to, R_gmp_symbol_denominator, sd);
	UNPROTECT(3);
	return to;
}

SEXP R_flint_coerce_mpfr_arf(SEXP from)
{
	R_flint_coerce_init();
	mp_limb_t j, n = (mp_limb_t) XLENGTH(from);
	SEXP to = PROTECT(newFlint(R_FLINT_CLASS_ARF, 0, n));
	arf_ptr y = R_flint_get_pointer(to);
	mpfr_prec_t tprec = MPFR_PREC_MIN;
	for (j = 0; j < n; ++j) {
		mpfr_prec_t tprec1 = INTEGER(R_do_slot(VECTOR_ELT(from, (R_xlen_t) j), R_Rmpfr_symbol_prec))[0];
		if (tprec1 > tprec)
			tprec = tprec1;
	}
	mpfr_t t;
	mpfr_init2(t, tprec);
	mp_size_t nlimb = (tprec - 1)/mp_bits_per_limb + 1, nlimb0, nlimb1;
	mp_limb_t *ptr;
	for (j = 0; j < n; ++j) {
		SEXP elt = PROTECT(VECTOR_ELT(from, (R_xlen_t) j)),
			sprec = PROTECT(R_do_slot(elt, R_Rmpfr_symbol_prec)),
			ssign = PROTECT(R_do_slot(elt, R_Rmpfr_symbol_sign)),
			sexp  = PROTECT(R_do_slot(elt, R_Rmpfr_symbol_exp)),
			sd    = PROTECT(R_do_slot(elt, R_Rmpfr_symbol_d));
		const int
			*prec = (void *) INTEGER(sprec),
			*sign = (void *) INTEGER(ssign);
		const unsigned int
			*exp  = (void *) INTEGER(sexp),
			*d    = (void *) INTEGER(sd);
		t->_mpfr_prec = prec[0];
		t->_mpfr_sign = sign[0];

#define SIGNED(u, utype, stype) \
		(((u) <= ((utype) -1 >> 1)) ? (stype) u : -(stype) ~(u) - 1)
#define SIGNED_EXPONENT(u) \
		SIGNED((u), mpfr_uexp_t, mpfr_exp_t)

#if SIZEOF_MPFR_EXP_T == 4
		t->_mpfr_exp = SIGNED_EXPONENT(exp[0]);
#else
		t->_mpfr_exp = SIGNED_EXPONENT(((mpfr_uexp_t) exp[1] << 32) | ((mpfr_uexp_t) exp[0] & 0x00000000FFFFFFFFu));
#endif
		if (XLENGTH(sd) == 0)
		memset(t->_mpfr_d, 0, (size_t) nlimb * sizeof(mp_limb_t));
		else {
		nlimb1 = (t->_mpfr_prec - 1)/mp_bits_per_limb + 1;
		nlimb0 = nlimb - nlimb1;
		ptr = t->_mpfr_d;
		for (mp_size_t l = 0; l < nlimb0; ++l)
			*(ptr++) = 0;
#if SIZEOF_MP_LIMB_T == 4
		for (mp_size_t l = 0; l < nlimb1; ++l, d += 1)
			*(ptr++) = d[0];
#else
		for (mp_size_t l = 0; l < nlimb1; ++l, d += 2)
			*(ptr++) = ((mp_limb_t) d[1] << 32) | ((mp_limb_t) d[0] & 0x00000000FFFFFFFFu);
#endif
		}
		arf_set_mpfr(y + j, t);
		UNPROTECT(5);
	}
	mpfr_clear(t);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_coerce_arf_mpfr(SEXP from)
{
	R_flint_coerce_init();
	mp_limb_t j, n = R_flint_get_length(from);
	if (n > R_XLEN_T_MAX)
		Rf_error(_("length would exceed maximum %lld"),
		         (long long int) R_XLEN_T_MAX);
	arf_srcptr x = R_flint_get_pointer(from);
	slong fprec = 0;
	for (j = 0; j < n; ++j) {
		slong fprec1 = arf_bits(x + j);
		if (fprec1 > fprec)
			fprec = fprec1;
	}
	mpfr_prec_t tprec = MPFR_PREC_MIN;
	mpfr_rnd_t trnd = MPFR_RNDZ;
#define R_RMPFR_PREC_MAX MIN2(MPFR_PREC_MAX, INT_MAX)
	if (fprec > tprec)
		tprec = (fprec > R_RMPFR_PREC_MAX) ? R_RMPFR_PREC_MAX : (mpfr_prec_t) fprec;
	SEXP
		object = PROTECT(newObject("mpfr")),
		value  = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t) n)),
		to     = PROTECT(R_do_slot_assign(object, R_flint_symbol_dot_data, value));
	mpfr_t t;
	mpfr_init2(t, tprec);
	for (j = 0; j < n; ++j) {
		arf_get_mpfr(t, x + j, trnd);
		tprec = mpfr_min_prec(t);
		if (tprec < MPFR_PREC_MIN)
			tprec = MPFR_PREC_MIN;
		SEXP elt = PROTECT(newObject("mpfr1")),
			sprec = PROTECT(Rf_allocVector(INTSXP, 1)),
			ssign = PROTECT(Rf_allocVector(INTSXP, 1)),
			sexp  = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) (sizeof(mpfr_exp_t)/sizeof(int)))),
			sd    = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) (sizeof(mp_limb_t)/sizeof(int)) * ((mpfr_regular_p(t)) ? (tprec - 1)/mp_bits_per_limb + 1 : 0)));
		int
			*prec = (void *) INTEGER(sprec),
			*sign = (void *) INTEGER(ssign);
		unsigned int
			*exp  = (void *) INTEGER(sexp),
			*d    = (void *) INTEGER(sd);
		prec[0] = (int) tprec;
		sign[0] = t->_mpfr_sign;
#if SIZEOF_MPFR_EXP_T == 4
		exp[0] = (unsigned int) t->_mpfr_exp;
#else
		exp[0] = (unsigned int) ((mpfr_uexp_t) t->_mpfr_exp & 0x00000000FFFFFFFFu);
		exp[1] = (unsigned int) ((mpfr_uexp_t) t->_mpfr_exp >> 32);
#endif
		if (mpfr_regular_p(t)) {
		mp_size_t
			nlimb  = (t->_mpfr_prec - 1)/mp_bits_per_limb + 1,
			nlimb1 = (        tprec - 1)/mp_bits_per_limb + 1,
			nlimb0 = nlimb - nlimb1;
		mp_limb_t *ptr = t->_mpfr_d + nlimb0;
#if SIZEOF_MP_LIMB_T == 4
		for (mp_size_t l = 0; l < nlimb1; ++l, d += 1)
			d[0] = (unsigned int) *(ptr++);
#else
		for (mp_size_t l = 0; l < nlimb1; ++l, d += 2) {
			d[0] = (unsigned int) (*(ptr  ) & 0x00000000FFFFFFFFu);
			d[1] = (unsigned int) (*(ptr++) >> 32);
		}
#endif
		}
		R_do_slot_assign(elt, R_Rmpfr_symbol_prec, sprec);
		R_do_slot_assign(elt, R_Rmpfr_symbol_sign, ssign);
		R_do_slot_assign(elt, R_Rmpfr_symbol_exp , sexp);
		R_do_slot_assign(elt, R_Rmpfr_symbol_d   , sd);
		SET_VECTOR_ELT(to, (R_xlen_t) j, elt);
		UNPROTECT(5);
	}
	mpfr_clear(t);
	UNPROTECT(3);
	return to;
}
