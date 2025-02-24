#ifndef R_FLINT_ARB_TEMPLATE_H
#define R_FLINT_ARB_TEMPLATE_H

#include "flint.h"

SEXP R_flint_arb_0ary(void (*)(arb_t, slong),
                      SEXP, SEXP);
SEXP R_flint_arb_1ary(void (*)(arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP);
SEXP R_flint_arb_2ary(void (*)(arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_3ary(void (*)(arb_t, const arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_4ary(void (*)(arb_t, const arb_t, const arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_5ary(void (*)(arb_t, const arb_t, const arb_t, const arb_t, const arb_t, const arb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_0ary_flags(void (*)(arb_t, int, slong),
                            SEXP, SEXP, SEXP);
SEXP R_flint_arb_1ary_flags(void (*)(arb_t, const arb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_2ary_flags(void (*)(arb_t, const arb_t, const arb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_3ary_flags(void (*)(arb_t, const arb_t, const arb_t, const arb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_4ary_flags(void (*)(arb_t, const arb_t, const arb_t, const arb_t, const arb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_5ary_flags(void (*)(arb_t, const arb_t, const arb_t, const arb_t, const arb_t, const arb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif /* ! defined (R_FLINT_ARB_TEMPLATE_H) */
