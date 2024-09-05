#ifndef R_FLINT_H
#define R_FLINT_H

#include <float.h> /* DBL_MIN */
#include <limits.h> /* CHAR_BIT, INT_MAX, ... */
#include <math.h> /* fabs */
#include <stddef.h> /* size_t */
#include <flint/flint.h> /* ulong, slong, ... */

#define R_NO_REMAP

#include <Rconfig.h> /* R_INLINE */
#include <R_ext/Arith.h> /* R_FINITE, ISNAN, ... */
#include <R_ext/Error.h> /* Rf_error, Rf_warning */
#include <R_ext/RS.h> /* R_Calloc, R_Free */
#include <Rinternals.h> /* SEXP, ... */

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
		Rf_warning("NA introduced by coercion to range of \"%s\"", \
		           "integer"); \
		w = 0; \
	} \
} while (0)

#define WARNING_OOB_DOUBLE(w) \
do { \
	if (w) { \
		Rf_warning("-Inf or Inf introduced by coercion to range of \"%s\"", \
		           "double"); \
		w = 0; \
	} \
} while (0)

#define ERROR_INVALID_TYPE(x, func) \
do { \
	Rf_error("invalid type \"%s\" in '%s'", \
	         Rf_type2char((SEXPTYPE) TYPEOF(x)), func); \
} while (0)

#define ERROR_INVALID_CLASS(x, func) \
do { \
	Rf_error("invalid class \"%s\" in '%s'", \
	         CHAR(STRING_ELT(Rf_getAttrib(object, R_ClassSymbol), 0)), func); \
} while (0)

extern
SEXP R_flint_symbol_prec, R_flint_symbol_exp, R_flint_symbol_sign,
	R_flint_symbol_d, R_flint_symbol_length, R_flint_symbol_x;

SEXP newObject(const char *);
void assertClass(SEXP, const char *, const char *);

int asFlags(SEXP, const char *);
int asRnd(SEXP, const char *);

unsigned long long int _R_flint_length_get(SEXP);
void _R_flint_length_set(SEXP, unsigned long long int);

void *_R_flint_x_get(SEXP);
void _R_flint_x_set(SEXP, void *, R_CFinalizer_t);

void R_flint_fmpz_finalize(SEXP);
void R_flint_fmpq_finalize(SEXP);
void R_flint_mag_finalize(SEXP);
void R_flint_arf_finalize(SEXP);
void R_flint_arb_finalize(SEXP);
void R_flint_acb_finalize(SEXP);

#endif /* ! defined (R_FLINT_H) */
