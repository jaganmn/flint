#ifndef R_FLINT_ACB_TEMPLATE_H
#define R_FLINT_ACB_TEMPLATE_H

#include "flint.h"

SEXP R_flint_acb_0ary(void (*)(acb_t, slong),
                      SEXP, SEXP);
SEXP R_flint_acb_1ary(void (*)(acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP);
SEXP R_flint_acb_2ary(void (*)(acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_3ary(void (*)(acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_4ary(void (*)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_5ary(void (*)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_acb_0ary_flags(void (*)(acb_t, int, slong),
                            SEXP, SEXP, SEXP);
SEXP R_flint_acb_1ary_flags(void (*)(acb_t, const acb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_2ary_flags(void (*)(acb_t, const acb_t, const acb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_3ary_flags(void (*)(acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_4ary_flags(void (*)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_5ary_flags(void (*)(acb_t, const acb_t, const acb_t, const acb_t, const acb_t, const acb_t, int, slong),
                            SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif /* ! defined (R_FLINT_ACB_TEMPLATE_H) */
