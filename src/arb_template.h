#ifndef R_FLINT_ARB_TEMPLATE_H
#define R_FLINT_ARB_TEMPLATE_H

#include "noreturn.h"
#include <flint/flint.h>
#include <flint/arb.h>
#include <Rinternals.h>

SEXP R_flint_arb_0ary(void (*)(arb_t, slong),
                      SEXP, SEXP);
SEXP R_flint_arb_1ary(void (*)(arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP);
SEXP R_flint_arb_2ary(void (*)(arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_3ary(void (*)(arb_t, const arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP);

#endif /* ! defined (R_FLINT_ARB_TEMPLATE_H) */
