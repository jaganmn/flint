#include <limits.h>
#include <mpfr.h>
#include <Rinternals.h>
#include <R_ext/Error.h>

#if MPFR_PREC_WIDTH == 1
# define FCOPY_PREC(i, j) \
	do { \
		(i) = (mpfr_prec_t) (j)[0]; \
	} while (0)
# define RCOPY_PREC(i, j) \
	do { \
		(j)[0] = (int) (i); \
	} while (0)
#elif MPFR_PREC_WIDTH == 2
# define FCOPY_PREC(i, j) \
	do { \
		mpfr_uprec_t tmp = ((mpfr_uprec_t) (j)[1]) << sizeof(int) | ((mpfr_uprec_t) (j)[0]); \
		if (tmp <= (((mpfr_uprec_t) -1) >> 1)) \
		(i) = (mpfr_prec_t) tmp; \
		else \
		(i) = -(mpfr_prec_t) ~tmp - 1; \
	} while (0)
# define RCOPY_PREC(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) (((mpfr_uprec_t) (i)) & 0x00000000FFFFFFFFu); \
		if (tmp <= INT_MAX) \
		(j)[0] = (int) tmp; \
		else \
		(j)[0] = -(int) ~tmp - 1; \
		tmp = (unsigned int) (((mpfr_uprec_t) (i)) >> sizeof(int)); \
		if (tmp <= INT_MAX) \
		(j)[1] = (int) tmp; \
		else \
		(j)[1] = -(int) ~tmp - 1; \
	} while (0)
#else
# error "invalid MPFR_PREC_WIDTH"
#endif

#if MPFR_EXP_WIDTH == 1
# define FCOPY_EXP(i, j) \
	do { \
		(i) = (mpfr_exp_t) (j)[0]; \
	} while (0)
# define RCOPY_EXP(i, j) \
	do { \
		(j)[0] = (int) (i); \
	} while (0)
#elif MPFR_EXP_WIDTH == 2
# define FCOPY_EXP(i, j) \
	do { \
		mpfr_uexp_t tmp = ((mpfr_uexp_t) (j)[1]) << sizeof(int) | ((mpfr_uexp_t) (j)[0]); \
		if (tmp <= (((mpfr_uexp_t) -1) >> 1)) \
		(i) = (mpfr_exp_t) tmp; \
		else \
		(i) = -(mpfr_exp_t) ~tmp - 1; \
	} while (0)
# define RCOPY_EXP(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) (((mpfr_uexp_t) (i)) & 0x00000000FFFFFFFFu); \
		if (tmp <= INT_MAX) \
		(j)[0] = (int) tmp; \
		else \
		(j)[0] = -(int) ~tmp - 1; \
		tmp = (unsigned int) (((mpfr_uexp_t) (i)) >> sizeof(int)); \
		if (tmp <= INT_MAX) \
		(j)[1] = (int) tmp; \
		else \
		(j)[1] = -(int) ~tmp - 1; \
	} while (0)
#else
# error "invalid MPFR_EXP_WIDTH"
#endif

#if MPFR_SIGN_WIDTH == 1
# define FCOPY_SIGN(i, j) \
	do { \
		(i) = (mp_sign_t) (j)[0]; \
	} while (0)
# define RCOPY_SIGN(i, j) \
	do { \
		(j)[0] = (int) (i); \
	} while (0)
#else
# error "invalid MPFR_SIGN_WIDTH"
#endif

#if MP_LIMB_WIDTH == 1
# define FCOPY_LIMB(i, j) \
	do { \
		(i) = (mp_limb_t) (j)[0]; \
	} while (0)
# define RCOPY_LIMB(i, j) \
	do { \
		if ((i) <= INT_MAX) \
		(j)[0] = (int) (i); \
		else \
		(j)[0] = -(int) ~(i) - 1; \
	} while (0)
#elif MP_LIMB_WIDTH == 2
# define FCOPY_LIMB(i, j) \
	do { \
		(i) = ((mp_limb_t) (j)[1]) << sizeof(int) | ((mp_limb_t) (j)[0]); \
	} while (0)
# define RCOPY_LIMB(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) ((i) & 0x00000000FFFFFFFFu); \
		if (tmp <= INT_MAX) \
		(j)[0] = (int) tmp; \
		else \
		(j)[0] = -(int) ~tmp - 1; \
		tmp = (unsigned int) ((i) >> sizeof(int)); \
		if (tmp <= INT_MAX) \
		(j)[1] = (int) tmp; \
		else \
		(j)[1] = -(int) ~tmp - 1; \
	} while (0)
#else
# error "invalid MP_LIMB_WIDTH"
#endif

extern SEXP mpfr_precSymbol, mpfr_expSymbol, mpfr_signSymbol, mpfr_dSymbol;

void sexp_as_mpfr(mpfr_t to, SEXP from)
{
	static const char *valid[] = { "mpfr1", "" };
	if (TYPEOF(from) != OBJSXP)
		Rf_error("invalid type \"%s\" in '%s'",
		         type2char((SEXPTYPE) TYPEOF(from)), __func__);
	if (R_check_class_etc(from, valid) < 0)
		Rf_error("invalid class '%s' in '%s'",
		         CHAR(STRING_ELT(getAttrib(from, R_ClassSymbol), 0)), __func__);

	mpfr_prec_t prec;
	mpfr_exp_t exp;
	mpfr_sign_t sign;

	SEXP s_prec = R_do_slot(from, mpfr_precSymbol);
	if (TYPEOF(s_prec) != INTSXP || XLENGTH(s_prec) != MPFR_PREC_WIDTH)
		Rf_error("invalid '%s' slot", "prec");
	FCOPY_PREC(prec, INTEGER(s_prec));
	if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX)
		Rf_error("invalid '%s' slot", "prec");
	
	SEXP s_exp = R_do_slot(from, mpfr_expSymbol);
	if (TYPEOF(s_exp) != INTSXP || XLENGTH(s_exp) != MPFR_EXP_WIDTH)
		Rf_error("invalid '%s' slot", "exp");
	FCOPY_EXP(exp, INTEGER(s_exp));

	SEXP s_sign = R_do_slot(from, mpfr_signSymbol);
	if (TYPEOF(s_sign) != INTSXP || XLENGTH(s_sign) != 1 ||
	    INTEGER(s_sign)[0] == NA_INTEGER)
		Rf_error("invalid '%s' slot", "sign");
	FCOPY_SIGN(sign, INTEGER(s_sign));

	mpfr_init2(to, prec);
	to->_mpfr_exp = exp;
	to->_mpfr_sign = sign;
	if (mpfr_regular_p(to)) {

	mpfr_prec_t limbs = prec / mp_bits_per_limb +
		(prec % mp_bits_per_limb != 0);

	SEXP s_d = R_do_slot(from, mpfr_dSymbol);
	if (TYPEOF(s_d) != INTSXP || XLENGTH(s_d) != limbs * MP_LIMB_WIDTH) {
		mpfr_clear(to);
		Rf_error("invalid '%s' slot", "d");
	}
	mp_limb_t *d = to->_mpfr_d;
	int *s = INTEGER(s_d);
	for (mpfr_prec_t l = 0; l < limbs; ++l) {
		FCOPY_LIMB(d[l], s);
		s += MP_LIMB_WIDTH;
	}
	if (limbs > 0 &&
	    ((d[limbs - 1] >> (mp_bits_per_limb - 1)) == 0 ||
	     (prec % mp_bits_per_limb != 0 &&
	      (d[0] << (prec % mp_bits_per_limb)) != 0))) {
		mpfr_clear(to);
		Rf_error("invalid '%s' slot", "d");
	}

	}

	return;
}

void mpfr_as_sexp(SEXP to, mpfr_t from)
{
	static const char *valid[] = { "mpfr1", "" };
	if (TYPEOF(to) != OBJSXP)
		Rf_error("invalid type \"%s\" in '%s'",
		         type2char((SEXPTYPE) TYPEOF(to)), __func__);
	if (R_check_class_etc(to, valid) < 0)
		Rf_error("invalid class '%s' in '%s'",
		         CHAR(STRING_ELT(getAttrib(to, R_ClassSymbol), 0)), __func__);

	mpfr_prec_t prec = from->_mpfr_prec;
	mpfr_exp_t exp = from->_mpfr_exp;
	mpfr_sign_t sign = from->_mpfr_sign;

	SEXP s_prec = R_do_slot(to, mpfr_precSymbol);
	RCOPY_PREC(prec, INTEGER(s_prec));

	SEXP s_exp = R_do_slot(to, mpfr_expSymbol);
	RCOPY_EXP(exp, INTEGER(s_exp));

	SEXP s_sign = R_do_slot(to, mpfr_signSymbol);
	RCOPY_SIGN(sign, INTEGER(s_sign));

	if (mpfr_regular_p(from)) {

	mpfr_prec_t limbs = prec / mp_bits_per_limb +
		(prec % mp_bits_per_limb != 0);

	SEXP s_d = PROTECT(allocVector(INTSXP, limbs * MP_LIMB_WIDTH));
	mp_limb_t *d = from->_mpfr_d;
	int *s = INTEGER(s_d);
	for (mpfr_prec_t l = 0; l < limbs; ++l) {
		RCOPY_LIMB(d[l], s);
		s += MP_LIMB_WIDTH;
	}
	R_do_slot_assign(to, mpfr_dSymbol, s_d);
	UNPROTECT(1);

	}

	return;
}
