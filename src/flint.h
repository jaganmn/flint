#ifndef R_FLINT_FLINT_H
#define R_FLINT_FLINT_H

#include <ctype.h> /* isspace */
#include <float.h> /* DBL_MIN, ... */
#include <limits.h> /* CHAR_BIT, ... */
#include <math.h> /* fabs, ldexp, frexp, ... */
#include <stdarg.h> /* va_list, va_start, ... */
#include <stddef.h> /* size_t */
#include <stdio.h> /* vsnprintf */
#include <string.h> /* strlen, memset, memcpy, memmove */
#include <gmp.h> /* mp_limb_t */
#include <mpfr.h> /* mpfr_uprec_t, mpfr_uexp_t */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#include "noreturn.h"
#include <flint/flint.h>
#include <flint/longlong.h>
#include <flint/ulong_extras.h>
#include <flint/long_extras.h>
#include <flint/double_extras.h>
#include <flint/fmpz.h>
#include <flint/fmpz_mat.h>
#include <flint/fmpq.h>
#include <flint/fmpq_mat.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/arb.h>
#include <flint/arb_mat.h>
#include <flint/acb.h>
#include <flint/acb_mat.h>
#include "revertnoreturn.h"
#include "fallback.h"
#include <Rconfig.h> /* R_INLINE, ENABLE_NLS */
#include <R_ext/Arith.h> /* R_FINITE, ISNAN, ... */
#include <R_ext/Complex.h> /* Rcomplex */
#include <R_ext/Error.h> /* Rf_error, Rf_warning */
#include <R_ext/Memory.h> /* R_alloc */
#include <R_ext/Rdynload.h> /* DL_FUNC, ... */
#include <R_ext/Visibility.h> /* attribute_visible */
#include <Rinternals.h> /* SEXP, ... */
#include <Rversion.h> /* R_VERSION */

#ifdef ENABLE_NLS
# include <libintl.h> /* dgettext, dngettext */
#else
# define dgettext(Domain, String) (String)
# define dngettext(Domain, String, StringP, N) (((N) == 1) ? String : StringP)
#endif
#define _(String) dgettext("flint", String)

#if R_VERSION < R_Version(4, 4, 0)
# define OBJSXP S4SXP
#endif /* < 4.4.0 */

#define R_FLINT_SWITCH(class, template) \
do { \
	switch (class) { \
	case R_FLINT_CLASS_ULONG: \
		template(ulong, ulong, const ulong *, ulong *); \
		break; \
	case R_FLINT_CLASS_SLONG: \
		template(slong, slong, const slong *, slong *); \
		break; \
	case R_FLINT_CLASS_FMPZ: \
		template(fmpz, fmpz, const fmpz *, fmpz *); \
		break; \
	case R_FLINT_CLASS_FMPQ: \
		template(fmpq, fmpq, const fmpq *, fmpq *); \
		break; \
	case R_FLINT_CLASS_MAG: \
		template(mag, mag_t, mag_srcptr, mag_ptr); \
		break; \
	case R_FLINT_CLASS_ARF: \
		template(arf, arf_t, arf_srcptr, arf_ptr); \
		break; \
	case R_FLINT_CLASS_ACF: \
		template(acf, acf_t, acf_srcptr, acf_ptr); \
		break; \
	case R_FLINT_CLASS_ARB: \
		template(arb, arb_t, arb_srcptr, arb_ptr); \
		break; \
	case R_FLINT_CLASS_ACB: \
		template(acb, acb_t, acb_srcptr, acb_ptr); \
		break; \
	default: \
		Rf_error(_("should never happen ...")); \
	} \
} while (0)

#define MIN2(a, b) \
(((a) > (b)) ? (b)                    : (a))
#define MIN3(a, b, c) \
(((a) > (b)) ? MIN2(b, c)             : MIN2(a, c))
#define MIN4(a, b, c, d) \
(((a) > (b)) ? MIN3(b, c, d)          : MIN3(a, c, d))
#define MIN5(a, b, c, d, e) \
(((a) > (b)) ? MIN4(b, c, d, e)       : MIN4(a, c, d, e))
#define MIN6(a, b, c, d, e, f) \
(((a) > (b)) ? MIN5(b, c, d, e, f)    : MIN5(a, c, d, e, f))
#define MIN7(a, b, c, d, e, f, g) \
(((a) > (b)) ? MIN6(b, c, d, e, f, g) : MIN6(a, c, d, e, f, g))

#define MAX2(a, b) \
(((a) < (b)) ? (b)                    : (a))
#define MAX3(a, b, c) \
(((a) < (b)) ? MAX2(b, c)             : MAX2(a, c))
#define MAX4(a, b, c, d) \
(((a) < (b)) ? MAX3(b, c, d)          : MAX3(a, c, d))
#define MAX5(a, b, c, d, e) \
(((a) < (b)) ? MAX4(b, c, d, e)       : MAX4(a, c, d, e))
#define MAX6(a, b, c, d, e, f) \
(((a) < (b)) ? MAX5(b, c, d, e, f)    : MAX5(a, c, d, e, f))
#define MAX7(a, b, c, d, e, f, g) \
(((a) < (b)) ? MAX6(b, c, d, e, f, g) : MAX6(a, c, d, e, f, g))

#define RECYCLE2(a, b) \
(((a) && (b))                                    ? MAX2(a, b)                : 0)
#define RECYCLE3(a, b, c) \
(((a) && (b) && (c))                             ? MAX3(a, b, c)             : 0)
#define RECYCLE4(a, b, c, d) \
(((a) && (b) && (c) && (d))                      ? MAX4(a, b, c, d)          : 0)
#define RECYCLE5(a, b, c, d, e) \
(((a) && (b) && (c) && (d) && (e))               ? MAX5(a, b, c, d, e)       : 0)
#define RECYCLE6(a, b, c, d, e, f) \
(((a) && (b) && (c) && (d) && (e) && (f))        ? MAX6(a, b, c, d, e, f)    : 0)
#define RECYCLE7(a, b, c, d, e, f, g) \
(((a) && (b) && (c) && (d) && (e) && (f) && (g)) ? MAX7(a, b, c, d, e, f, g) : 0)

#define FOR_RECYCLE0(j, n) \
for (j = 0; \
     j < n; \
     ++j)
#define FOR_RECYCLE1(j, n, ja, na) \
for (j = 0, \
     ja = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja)
#define FOR_RECYCLE2(j, n, ja, na, jb, nb) \
for (j = 0, \
     ja = 0, \
     jb = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb)
#define FOR_RECYCLE3(j, n, ja, na, jb, nb, jc, nc) \
for (j = 0, \
     ja = 0, \
     jb = 0, \
     jc = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb, \
     jc = (++jc == nc) ? 0 : jc)
#define FOR_RECYCLE4(j, n, ja, na, jb, nb, jc, nc, jd, nd) \
for (j = 0, \
     ja = 0, \
     jb = 0, \
     jc = 0, \
     jd = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb, \
     jc = (++jc == nc) ? 0 : jc, \
     jd = (++jd == nd) ? 0 : jd)
#define FOR_RECYCLE5(j, n, ja, na, jb, nb, jc, nc, jd, nd, je, ne) \
for (j = 0, \
     ja = 0, \
     jb = 0, \
     jc = 0, \
     jd = 0, \
     je = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb, \
     jc = (++jc == nc) ? 0 : jc, \
     jd = (++jd == nd) ? 0 : jd, \
     je = (++je == ne) ? 0 : je)
#define FOR_RECYCLE6(j, n, ja, na, jb, nb, jc, nc, jd, nd, je, ne, jf, nf) \
for (j = 0, \
     ja = 0, \
     jb = 0, \
     jc = 0, \
     jd = 0, \
     je = 0, \
     jf = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb, \
     jc = (++jc == nc) ? 0 : jc, \
     jd = (++jd == nd) ? 0 : jd, \
     je = (++je == ne) ? 0 : je, \
     jf = (++jf == nf) ? 0 : jf)
#define FOR_RECYCLE7(j, n, ja, na, jb, nb, jc, nc, jd, nd, je, ne, jf, nf, jg, ng) \
for (j = 0, \
     ja = 0, \
     jb = 0, \
     jc = 0, \
     jd = 0, \
     je = 0, \
     jf = 0, \
     jg = 0; \
     j < n; \
     ++j, \
     ja = (++ja == na) ? 0 : ja, \
     jb = (++jb == nb) ? 0 : jb, \
     jc = (++jc == nc) ? 0 : jc, \
     jd = (++jd == nd) ? 0 : jd, \
     je = (++je == ne) ? 0 : je, \
     jf = (++jf == nf) ? 0 : jf, \
     jg = (++jg == ng) ? 0 : jg)

#define WARNING_OOB_INTEGER \
do { \
	Rf_warning(_("NA introduced by coercion to range of \"%s\""), \
	           "integer"); \
} while (0)

#define WARNING_OOB_DOUBLE \
do { \
	Rf_warning(_("-Inf or Inf introduced by coercion to range of \"%s\""), \
	           "double"); \
} while (0)

#define WARNING_LOST_IMAG \
do { \
	Rf_warning(_("imaginary parts discarded in conversion")); \
} while (0)

#define WARNING_LOST_RAD \
do { \
	Rf_warning(_("radii discarded in conversion")); \
} while (0)

#define ERROR_INVALID_TYPE(x, func) \
do { \
	Rf_error(_("object of invalid type \"%s\" in '%s'"), \
	         Rf_type2char((SEXPTYPE) TYPEOF(x)), func); \
} while (0)

#define ERROR_INVALID_CLASS(x, func) \
do { \
	if (Rf_isObject(x)) \
		Rf_error(_("object of invalid class \"%s\" in '%s'"), \
		         CHAR(STRING_ELT(Rf_getAttrib(x, R_ClassSymbol), 0)), func); \
	else \
		Rf_error(_("object without class attribute in '%s'"), \
		         func); \
} while (0)

#define ERROR_TOO_LONG(n, nmax) \
do { \
	if ((n) > (nmax)) \
		Rf_error(_("length would exceed maximum %llu"), \
		         (unsigned long long int) (nmax)); \
} while (0)

#define ARB_CONTAINS_NAN(x) \
	(arf_is_nan(arb_midref(x)))
#define ACB_CONTAINS_NAN(x) \
	(ARB_CONTAINS_NAN(acb_realref(x)) || \
	 ARB_CONTAINS_NAN(acb_imagref(x)))

#define ARB_CONTAINS_ZERO(x) \
	(!arf_is_nan(arb_midref(x)) && \
	 arf_cmpabs_mag(arb_midref(x), arb_radref(x)) <= 0)
#define ACB_CONTAINS_ZERO(x) \
	(ARB_CONTAINS_ZERO(acb_realref(x)) && \
	 ARB_CONTAINS_ZERO(acb_imagref(x)))

#define ARB_CONTAINS_NONZERO(x) \
	(!mag_is_zero(arb_radref(x)) || \
	 !(arf_is_nan(arb_midref(x)) || arf_is_zero(arb_midref(x))))
#define ACB_CONTAINS_NONZERO(x) \
	(ARB_CONTAINS_NONZERO(acb_realref(x)) || \
	 ARB_CONTAINS_NONZERO(acb_imagref(x)))

#define MPFR_ERANGE_SET \
mpfr_exp_t \
	__emin_old = mpfr_get_emin(), __emin_new = mpfr_get_emin_min(), \
	__emax_old = mpfr_get_emax(), __emax_new = mpfr_get_emax_max(); \
mpfr_set_emin(__emin_new); \
mpfr_set_emax(__emax_new); \

#define MPFR_ERANGE_RESET \
mpfr_set_emin(__emin_old); \
mpfr_set_emax(__emax_old); \


extern
SEXP R_flint_namespace,
	R_flint_symbol_missing,
	R_flint_symbol_dot_data,
	R_flint_symbol_dot_xdata,
	R_flint_symbol_dim,
	R_flint_symbol_dimnames,
	R_flint_symbol_names,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_off;

typedef enum {
	R_FLINT_CLASS_ULONG = 0,
	R_FLINT_CLASS_SLONG,
	R_FLINT_CLASS_FMPZ,
	R_FLINT_CLASS_FMPQ,
	R_FLINT_CLASS_MAG,
	R_FLINT_CLASS_ARF,
	R_FLINT_CLASS_ACF,
	R_FLINT_CLASS_ARB,
	R_FLINT_CLASS_ACB,
	R_FLINT_CLASS_INVALID = -1
} R_flint_class_t;

typedef enum {
	R_FLINT_OPS2_ADD = 0, /*    x  +  y    */
	R_FLINT_OPS2_SUB,     /*    x  -  y    */
	R_FLINT_OPS2_MUL,     /*    x  *  y    */
	R_FLINT_OPS2_DIV,     /*    x  /  y    */
	R_FLINT_OPS2_POW,     /*    x  ^  y    */
	R_FLINT_OPS2_FDR,     /*    x  %% y    */
	R_FLINT_OPS2_FDQ,     /*    x %/% y    */
	R_FLINT_OPS2_EQ,      /*    x  == y    */
	R_FLINT_OPS2_NEQ,     /*    x  != y    */
	R_FLINT_OPS2_L,       /*    x  <  y    */
	R_FLINT_OPS2_LEQ,     /*    x  <= y    */
	R_FLINT_OPS2_G,       /*    x  >  y    */
	R_FLINT_OPS2_GEQ,     /*    x  >= y    */
	R_FLINT_OPS2_AND,     /*    x  &  y    */
	R_FLINT_OPS2_OR,      /*    x  |  y    */
	R_FLINT_OPS2_PROD,       /*             x %*% y    */
	R_FLINT_OPS2_CROSSPROD,  /*     crossprod(x, y)    */
	R_FLINT_OPS2_TCROSSPROD, /*    tcrossprod(x, y)    */
	R_FLINT_OPS2_SOLVE,      /*         solve(x, y)    */
	R_FLINT_OPS2_BACKSOLVE,  /*     backsolve(x, y)    */
	R_FLINT_OPS2_TBACKSOLVE, /*    tbacksolve(x, y)    */
	R_FLINT_OPS2_INVALID = -1
} R_flint_ops2_t;

typedef enum {
	R_FLINT_OPS1_PLUS = 0, /*                +z    */
	R_FLINT_OPS1_MINUS,    /*                -z    */
	R_FLINT_OPS1_ISNA,     /*          is.na(z)    */
	R_FLINT_OPS1_ISNAN,    /*         is.nan(z)    */
	R_FLINT_OPS1_ISINF,    /*    is.infinite(z)    */
	R_FLINT_OPS1_ISNUM,    /*      is.finite(z)    */
	R_FLINT_OPS1_NOT,      /*                !z    */
	R_FLINT_OPS1_CONJ,     /*           Conj(z)    */
	R_FLINT_OPS1_REAL,     /*             Re(z)    */
	R_FLINT_OPS1_IMAG,     /*             Im(z)    */
	R_FLINT_OPS1_MOD,      /*            Mod(z)    */
	R_FLINT_OPS1_ARG,      /*            Arg(z)    */
	R_FLINT_OPS1_ABS,      /*            abs(z)    */
	R_FLINT_OPS1_SIGN,     /*           sign(z)    */
	R_FLINT_OPS1_SQRT,     /*           sqrt(z)    */
	R_FLINT_OPS1_FLOOR,    /*          floor(z)    */
	R_FLINT_OPS1_CEILING,  /*        ceiling(z)    */
	R_FLINT_OPS1_TRUNC,    /*          trunc(z)    */
	R_FLINT_OPS1_CUMMIN,   /*         cummin(z)    */
	R_FLINT_OPS1_CUMMAX,   /*         cummax(z)    */
	R_FLINT_OPS1_CUMSUM,   /*         cumsum(z)    */
	R_FLINT_OPS1_CUMPROD,  /*        cumprod(z)    */
	R_FLINT_OPS1_LOG,      /*            log(z)    */
	R_FLINT_OPS1_LOG2,     /*           log2(z)    */
	R_FLINT_OPS1_LOG10,    /*          log10(z)    */
	R_FLINT_OPS1_LOG1P,    /*          log1p(z)    */
	R_FLINT_OPS1_EXP,      /*            exp(z)    */
	R_FLINT_OPS1_EXPM1,    /*          expm1(z)    */
	R_FLINT_OPS1_COS,      /*            cos(z)    */
	R_FLINT_OPS1_COSPI,    /*          cospi(z)    */
	R_FLINT_OPS1_ACOS,     /*           acos(z)    */
	R_FLINT_OPS1_COSH,     /*           cosh(z)    */
	R_FLINT_OPS1_ACOSH,    /*          acosh(z)    */
	R_FLINT_OPS1_SIN,      /*            sin(z)    */
	R_FLINT_OPS1_SINPI,    /*          sinpi(z)    */
	R_FLINT_OPS1_ASIN,     /*           asin(z)    */
	R_FLINT_OPS1_SINH,     /*           sinh(z)    */
	R_FLINT_OPS1_ASINH,    /*          asinh(z)    */
	R_FLINT_OPS1_TAN,      /*            tan(z)    */
	R_FLINT_OPS1_TANPI,    /*          tanpi(z)    */
	R_FLINT_OPS1_ATAN,     /*           atan(z)    */
	R_FLINT_OPS1_TANH,     /*           tanh(z)    */
	R_FLINT_OPS1_ATANH,    /*          atanh(z)    */
	R_FLINT_OPS1_GAMMA,    /*          gamma(z)    */
	R_FLINT_OPS1_LGAMMA,   /*         lgamma(z)    */
	R_FLINT_OPS1_2GAMMA,   /*        digamma(z)    */
	R_FLINT_OPS1_3GAMMA,   /*       trigamma(z)    */
	R_FLINT_OPS1_ROUND,    /*          round(z)    */
	R_FLINT_OPS1_SIGNIF,   /*         signif(z)    */
	R_FLINT_OPS1_MIN,      /*            min(z)    */
	R_FLINT_OPS1_MAX,      /*            max(z)    */
	R_FLINT_OPS1_RANGE,    /*          range(z)    */
	R_FLINT_OPS1_SUM,      /*            sum(z)    */
	R_FLINT_OPS1_PROD,     /*           prod(z)    */
	R_FLINT_OPS1_MEAN,     /*           mean(z)    */
	R_FLINT_OPS1_ANY,      /*            any(z)    */
	R_FLINT_OPS1_ALL,      /*            all(z)    */
	R_FLINT_OPS1_ANYNA,    /*          anyNA(z)    */
	R_FLINT_OPS1_ISUNS,    /*    is.unsorted(z)    */
	R_FLINT_OPS1_ROWSUM,   /*        rowSums(z)    */
	R_FLINT_OPS1_COLSUM,   /*        colSums(z)    */
	R_FLINT_OPS1_ROWMEAN,  /*       rowMeans(z)    */
	R_FLINT_OPS1_COLMEAN,  /*       colMeans(z)    */
	R_FLINT_OPS1_CROSSPROD,  /*      crossprod(z)    */
	R_FLINT_OPS1_TCROSSPROD, /*     tcrossprod(z)    */
	R_FLINT_OPS1_SOLVE,      /*          solve(z)    */
	R_FLINT_OPS1_BACKSOLVE,  /*      backsolve(z)    */
	R_FLINT_OPS1_TBACKSOLVE, /*     tbacksolve(z)    */
	R_FLINT_OPS1_CHOL2INV,   /*       chol2inv(z)    */
	R_FLINT_OPS1_CHOL,       /*           chol(z)    */
	R_FLINT_OPS1_DET,        /*            det(z)    */
	R_FLINT_OPS1_DIFF,       /*           diff(z)    */
	R_FLINT_OPS1_DIFFINV,    /*        diffinv(z)    */
	R_FLINT_OPS1_INVALID = -1
} R_flint_ops1_t;

extern
SEXPTYPE R_flint_sexptypes[8];

extern
const char *R_flint_classes[11];

extern
const char *R_flint_ops2[22];

extern
const char *R_flint_ops1[74];

#if R_VERSION < R_Version(4, 5, 0)
void CLEAR_ATTRIB(SEXP s);
SEXP R_ClosureEnv(SEXP s);
#endif

char *R_alloc_snprintf(size_t, const char *, ...);

SEXP newObject(const char *);
SEXP newFlint(R_flint_class_t, void *, mp_limb_t);

SEXPTYPE checkType(SEXP, SEXPTYPE *, const char *);
const char *checkClass(SEXP, const char **, const char *);

SEXP copyVector(SEXP);

mp_limb_t validLength(SEXP, SEXP, mp_limb_t);
SEXP validDim(SEXP);
SEXP validDimNames(SEXP, SEXP);
SEXP validNames(SEXP, mp_limb_t);

void setDDNN(SEXP, SEXP, SEXP, SEXP);
void setDDNN2(SEXP, SEXP, SEXP, mp_limb_t, mp_limb_t, mp_limb_t, int);
void setDDNN1(SEXP, SEXP);

int checkConformable(SEXP, SEXP, mp_limb_t, mp_limb_t, int, int *);

slong asPrec(SEXP, const char *);
mpfr_prec_t mpfrPrec(slong);
arf_rnd_t asRnd(SEXP, int, const char *);
mpfr_rnd_t mpfrRnd(arf_rnd_t);
int isRndZ(arf_rnd_t);
int asBase(SEXP, const char *);
size_t asDigits(SEXP, const char *);
const char *asSep(SEXP, const char *);

void  ucopy(unsigned int *, const mp_limb_t *);
void uucopy(mp_limb_t *, const unsigned int *);

size_t strmatch(const char *, const char **);
R_flint_ops2_t ops2match(const char *);
R_flint_ops1_t ops1match(const char *);
int ops2info(R_flint_ops2_t);
int ops1info(R_flint_ops1_t);

void *R_flint_get_pointer(SEXP);
mp_limb_t R_flint_get_length(SEXP);
R_flint_class_t R_flint_get_class(SEXP);
void R_flint_set(SEXP, void *, mp_limb_t, R_CFinalizer_t);

void R_flint_ulong_finalize(SEXP);
void R_flint_slong_finalize(SEXP);
void R_flint_fmpz_finalize(SEXP);
void R_flint_fmpq_finalize(SEXP);
void R_flint_mag_finalize(SEXP);
void R_flint_arf_finalize(SEXP);
void R_flint_acf_finalize(SEXP);
void R_flint_arb_finalize(SEXP);
void R_flint_acb_finalize(SEXP);

#endif /* ! defined (R_FLINT_FLINT_H) */
