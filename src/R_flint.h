#ifndef R_FLINT_H
#define R_FLINT_H

#include <float.h> /* DBL_MIN */
#include <limits.h> /* CHAR_BIT, INT_MAX, ... */
#include <math.h> /* fabs */
#include <stddef.h> /* size_t */
#include <flint.h> /* ulong, slong, ... */

#define R_NO_REMAP

#include <Rconfig.h> /* R_INLINE */
#include <R_ext/Arith.h> /* R_FINITE, ISNAN, ... */
#include <R_ext/Error.h> /* Rf_error, Rf_warning */
#include <R_ext/RS.h> /* R_Calloc, R_Free */
#include <Rinternals.h> /* SEXP, ... */

#define MAX2(a, b)       (((a) < (b)) ? (b)           : (a)          )
#define MAX3(a, b, c)    (((a) < (b)) ? MAX2(b, c)    : MAX2(a, c)   )
#define MAX4(a, b, c, d) (((a) < (b)) ? MAX3(b, c, d) : MAX3(a, c, d))

#define RECYCLE2(a, b)       (((a) && (b)              ) ? MAX2(a, b)       : 0)
#define RECYCLE3(a, b, c)    (((a) && (b) && (c)       ) ? MAX3(a, b, c)    : 0)
#define RECYCLE4(a, b, c, d) (((a) && (b) && (c) && (d)) ? MAX4(a, b, c, d) : 0)

extern
SEXP R_flint_symbol_prec, R_flint_symbol_exp, R_flint_symbol_sign,
	R_flint_symbol_d, R_flint_symbol_length, R_flint_symbol_x;

unsigned long long int _R_flint_length_get(SEXP);
void _R_flint_length_set(SEXP, unsigned long long int);

void *_R_flint_x_get(SEXP);
void _R_flint_x_set(SEXP, void *);

void R_flint_fmpz_finalize(SEXP);
void R_flint_fmpq_finalize(SEXP);
void R_flint_mag_finalize(SEXP);
void R_flint_arf_finalize(SEXP);
void R_flint_arb_finalize(SEXP);
void R_flint_acb_finalize(SEXP);

#endif /* ! defined (R_FLINT_H)
