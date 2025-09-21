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

#define WARNING_OOB_INTEGER(w) \
do { \
	if (w) { \
		Rf_warning(_("NA introduced by coercion to range of \"%s\""), \
		           "integer"); \
		w = 0; \
	} \
} while (0)

#define WARNING_OOB_DOUBLE(w) \
do { \
	if (w) { \
		Rf_warning(_("-Inf or Inf introduced by coercion to range of \"%s\""), \
		           "double"); \
		w = 0; \
	} \
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
		Rf_error(_("value length would exceed maximum %llu"), \
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
SEXP R_flint_symbol_missing,
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

extern
SEXPTYPE R_flint_sexptypes[8];

extern
const char *R_flint_classes[11];

extern
const char *R_flint_ops2[22];

extern
const char *R_flint_ops1[70];

#if R_VERSION < R_Version(4, 5, 0)
void CLEAR_ATTRIB(SEXP x);
#endif

char *R_alloc_snprintf(size_t, const char *, ...);

SEXP newObject(const char *);

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

mpfr_prec_t asPrec(SEXP, const char *);
mpfr_rnd_t asRnd(SEXP, const char *);
int asBase(SEXP, const char *);
size_t asDigits(SEXP, const char *);
const char *asSep(SEXP, const char *);

void  ucopy(unsigned int *, const mp_limb_t *);
void uucopy(mp_limb_t *, const unsigned int *);

size_t strmatch(const char *, const char **);
int matrixop(size_t);

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
