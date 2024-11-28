#ifndef R_FLINT_FLINT_H
#define R_FLINT_FLINT_H

#include <float.h> /* DBL_MIN, ... */
#include <limits.h> /* CHAR_BIT, ... */
#include <math.h> /* fabs, ldexp, frexp, ... */
#include <stdarg.h> /* va_list, va_start, ... */
#include <stddef.h> /* size_t */
#include <stdio.h> /* vsnprintf */
#include <string.h> /* strlen, memset, memcpy, memmove */

#include <Rconfig.h> /* R_INLINE, ENABLE_NLS */

#ifdef ENABLE_NLS
# include <libintl.h> /* dgettext, dngettext */
#endif

#include <R_ext/Arith.h> /* R_FINITE, ISNAN, ... */
#include <R_ext/Complex.h> /* Rcomplex */
#include <R_ext/Error.h> /* Rf_error, Rf_warning */
#include <R_ext/Memory.h> /* R_alloc */
#include <Rinternals.h> /* SEXP, ... */
#include <Rversion.h> /* R_VERSION */

#ifndef ENABLE_NLS
# define dgettext(Domain, String) (String)
# define dngettext(Domain, String, StringP, N) (((N) == 1) ? String : StringP)
#endif
#define _(String) dgettext("flint", String)

#if R_VERSION < R_Version(4, 4, 0)
# define OBJSXP S4SXP
#endif /* < 4.4.0 */

#define slong_set(rop, op) *(rop) = *(op)
#define ulong_set(rop, op) *(rop) = *(op)

#define MAX2(a, b) \
(((a) < (b)) ? (b)                 : (a))
#define MAX3(a, b, c) \
(((a) < (b)) ? MAX2(b, c)          : MAX2(a, c))
#define MAX4(a, b, c, d) \
(((a) < (b)) ? MAX3(b, c, d)       : MAX3(a, c, d))
#define MAX5(a, b, c, d, e) \
(((a) < (b)) ? MAX4(b, c, d, e)    : MAX4(a, c, d, e))
#define MAX6(a, b, c, d, e, f) \
(((a) < (b)) ? MAX5(b, c, d, e, f) : MAX5(a, c, d, e, f))

#define RECYCLE2(a, b) \
(((a) && (b))                             ? MAX2(a, b)             : 0)
#define RECYCLE3(a, b, c) \
(((a) && (b) && (c))                      ? MAX3(a, b, c)          : 0)
#define RECYCLE4(a, b, c, d) \
(((a) && (b) && (c) && (d))               ? MAX4(a, b, c, d)       : 0)
#define RECYCLE5(a, b, c, d, e) \
(((a) && (b) && (c) && (d) && (e))        ? MAX5(a, b, c, d, e)    : 0)
#define RECYCLE6(a, b, c, d, e, f) \
(((a) && (b) && (c) && (d) && (e) && (f)) ? MAX6(a, b, c, d, e, f) : 0)

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

#define ERROR_TOO_LONG(n) \
do { \
	if (n > R_XLEN_T_MAX) \
		Rf_error(_("value length would exceed maximum %lld"), \
		         (long long int) R_XLEN_T_MAX); \
} while (0)

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
SEXP R_flint_symbol_dot_xdata,
	R_flint_symbol_dot_data,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_off,
	R_flint_symbol_prec,
	R_flint_symbol_exp,
	R_flint_symbol_sign,
	R_flint_symbol_d;

typedef enum {
	R_FLINT_CLASS_SLONG = 0,
	R_FLINT_CLASS_ULONG,
	R_FLINT_CLASS_FMPZ,
	R_FLINT_CLASS_FMPQ,
	R_FLINT_CLASS_ARF,
	R_FLINT_CLASS_MAG,
	R_FLINT_CLASS_ARB,
	R_FLINT_CLASS_ACB,
	R_FLINT_CLASS_INVALID = -1
} R_flint_class_t;

extern
SEXPTYPE R_flint_sexptypes[6];

extern
const char *R_flint_classes[9];

extern
const char *R_flint_ops2[16];

extern
const char *R_flint_ops1[52];

#if R_VERSION < R_Version(4, 5, 0)
void CLEAR_ATTRIB(SEXP);
#endif /* < 4.5.0 */

char *R_alloc_snprintf(size_t, const char *, ...);

SEXP newObject(const char *);
SEXP newBasic(const char *, SEXPTYPE, R_xlen_t);

SEXPTYPE checkType(SEXP, SEXPTYPE *, const char *);
const char *checkClass(SEXP, const char **, const char *);

unsigned long long int asLength(SEXP, const char *);
int asPrec(SEXP, const char *);
int asBase(SEXP, const char *);
size_t asDigits(SEXP, const char *);
const char *asSep(SEXP, const char *);
int asRnd(SEXP, int, const char *);

void  ucopy(unsigned int *, const unsigned long long int *);
void uucopy(unsigned long long int *, const unsigned int *);

size_t strmatch(const char *, const char **);

void *R_flint_get_pointer(SEXP);
unsigned long long int R_flint_get_length(SEXP);
R_flint_class_t R_flint_get_class(SEXP);
void R_flint_set(SEXP, void *, unsigned long long int, R_CFinalizer_t);

void R_flint_slong_finalize(SEXP);
void R_flint_ulong_finalize(SEXP);
void R_flint_fmpz_finalize(SEXP);
void R_flint_fmpq_finalize(SEXP);
void R_flint_arf_finalize(SEXP);
void R_flint_mag_finalize(SEXP);
void R_flint_arb_finalize(SEXP);
void R_flint_acb_finalize(SEXP);

#endif /* ! defined (R_FLINT_FLINT_H) */
