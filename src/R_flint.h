#ifndef R_FLINT_H
#define R_FLINT_H

#include <float.h> /* DBL_MIN, ... */
#include <limits.h> /* CHAR_BIT, ... */
#include <math.h> /* fabs, ldexp, frexp, ... */
#include <stdarg.h> /* va_list, va_start, ... */
#include <stddef.h> /* size_t */
#include <string.h> /* strcmp */
#include <flint/flint.h> /* slong, ulong, ... */

#include <Rconfig.h> /* ENABLE_NLS */

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

#define MAX2(a, b) \
(((a) < (b)) ? (b)              : (a))
#define MAX3(a, b, c) \
(((a) < (b)) ? MAX2(b, c)       : MAX2(a, c))
#define MAX4(a, b, c, d) \
(((a) < (b)) ? MAX3(b, c, d)    : MAX3(a, c, d))
#define MAX5(a, b, c, d, e) \
(((a) < (b)) ? MAX4(b, c, d, e) : MAX4(a, c, d, e))

#define RECYCLE2(a, b) \
(((a) && (b))                      ? MAX2(a, b)          : 0)
#define RECYCLE3(a, b, c) \
(((a) && (b) && (c))               ? MAX3(a, b, c)       : 0)
#define RECYCLE4(a, b, c, d) \
(((a) && (b) && (c) && (d))        ? MAX4(a, b, c, d)    : 0)
#define RECYCLE5(a, b, c, d, e) \
(((a) && (b) && (c) && (d) && (e)) ? MAX5(a, b, c, d, e) : 0)

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

extern
SEXP R_flint_symbol_dot_xdata,
	R_flint_symbol_dot_data,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_prec,
	R_flint_symbol_exp,
	R_flint_symbol_sign,
	R_flint_symbol_d;

extern
SEXPTYPE R_flint_sexptypes[6];

extern
const char *R_flint_classes[9];

#if R_VERSION < R_Version(4, 4, 1)
void CLEAR_ATTRIB(SEXP);
#endif /* < 4.4.1 */

char *R_alloc_snprintf(size_t, const char *, ...);

void uconv(unsigned long long int *, unsigned int *, int);

SEXP newObject(const char *);
SEXP newBasic(const char *, SEXPTYPE, R_xlen_t);

SEXPTYPE checkType(SEXP, SEXPTYPE *, const char *);
const char *checkClass(SEXP, const char **, const char *);

unsigned long long int asLength(SEXP, const char *);
int asFlags(SEXP, const char *);
int asRnd(SEXP, const char *);

void *R_flint_get_pointer(SEXP);
unsigned long long int R_flint_get_length(SEXP);
const char *R_flint_get_class(SEXP);
void R_flint_set(SEXP, void *, unsigned long long int, R_CFinalizer_t);

void R_flint_slong_finalize(SEXP);
void R_flint_ulong_finalize(SEXP);
void R_flint_fmpz_finalize(SEXP);
void R_flint_fmpq_finalize(SEXP);
void R_flint_arf_finalize(SEXP);
void R_flint_mag_finalize(SEXP);
void R_flint_arb_finalize(SEXP);
void R_flint_acb_finalize(SEXP);

#endif /* ! defined (R_FLINT_H) */
