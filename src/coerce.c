#include <mpfr.h>
#include "R_flint.h"

#define CAST_SIGNED(u, utype, stype) \
	((u) <= (((utype) -1) >> 1)) ? (stype) (u) : -(stype) ~(u) - 1

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
		mpfr_uprec_t tmp = (mpfr_uprec_t) (j)[1] << (sizeof(int) * CHAR_BIT) | (mpfr_uprec_t) (j)[0]; \
		(i) = CAST_SIGNED(tmp, mpfr_uprec_t, mpfr_prec_t); \
	} while (0)
# define RCOPY_PREC(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) ((mpfr_uprec_t) (i) & 0x00000000FFFFFFFFu); \
		(j)[0] = CAST_SIGNED(tmp, unsigned int, int); \
		tmp = (unsigned int) ((mpfr_uprec_t) (i) >> (sizeof(int) * CHAR_BIT)); \
		(j)[1] = CAST_SIGNED(tmp, unsigned int, int); \
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
		mpfr_uexp_t tmp = (mpfr_uexp_t) (j)[1] << (sizeof(int) * CHAR_BIT) | (mpfr_uexp_t) (j)[0]; \
		(i) = CAST_SIGNED(tmp, mpfr_uexp_t, mpfr_exp_t); \
	} while (0)
# define RCOPY_EXP(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) ((mpfr_uexp_t) (i) & 0x00000000FFFFFFFFu); \
		(j)[0] = CAST_SIGNED(tmp, unsigned int, int); \
		tmp = (unsigned int) ((mpfr_uexp_t) (i) >> (sizeof(int) * CHAR_BIT)); \
		(j)[1] = CAST_SIGNED(tmp, unsigned int, int); \
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
		(j)[0] = CAST_SIGNED((i), unsigned int, int); \
	} while (0)
#elif MP_LIMB_WIDTH == 2
# define FCOPY_LIMB(i, j) \
	do { \
		(i) = (mp_limb_t) (j)[1] << (sizeof(int) * CHAR_BIT) | (mp_limb_t) (j)[0]; \
	} while (0)
# define RCOPY_LIMB(i, j) \
	do { \
		unsigned int tmp; \
		tmp = (unsigned int) ((i) & 0x00000000FFFFFFFFu); \
		(j)[0] = CAST_SIGNED(tmp, unsigned int, int); \
		tmp = (unsigned int) ((i) >> (sizeof(int) * CHAR_BIT)); \
		(j)[1] = CAST_SIGNED(tmp, unsigned int, int); \
	} while (0)
#else
# error "invalid MP_LIMB_WIDTH"
#endif

void sexp_as_mpfr(SEXP from, mpfr_t to)
{
	assertClass(from, "mpfr1", __func__);

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
	if (TYPEOF(s_sign) != INTSXP || XLENGTH(s_sign) != MPFR_SIGN_WIDTH)
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

void mpfr_as_sexp(mpfr_t from, SEXP to)
{
	assertClass(to, "mpfr1", __func__);

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
