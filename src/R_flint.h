#ifndef R_FLINT_H
#define R_FLINT_H

#define R_NO_REMAP

#include <limits.h> /* CHAR_BIT, INT_MAX, ... */
#include <Rconfig.h> /* R_INLINE */
#include <R_ext/Error.h> /* Rf_error, Rf_warning */
#include <R_ext/RS.h> /* R_Calloc, R_Free */
#include <Rinternals.h> /* SEXP, ... */

extern
SEXP R_flint_symbol_prec, R_flint_symbol_exp, R_flint_symbol_sign,
	R_flint_symbol_d, R_flint_symbol_length, R_flint_symbol_x;

#endif /* ! defined (R_FLINT_H)
