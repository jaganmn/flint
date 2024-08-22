#ifndef R_FLINT_H
#define R_FLINT_H

#define R_NO_REMAP

#include <limits.h> /* CHAR_BIT, INT_MAX, ... */
#include <Rinternals.h>
#include <R_ext/Error.h>

extern
SEXP R_flint_symbol_prec, R_flint_symbol_exp, R_flint_symbol_sign,
	R_flint_symbol_d, R_flint_symbol_length, R_flint_symbol_x;

#endif /* ! defined (R_FLINT_H)
