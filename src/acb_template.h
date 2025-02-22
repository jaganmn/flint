#ifndef R_FLINT_ACB_TEMPLATE_H
#define R_FLINT_ACB_TEMPLATE_H

#include "flint.h"

SEXP R_flint_acb_1ary(void (*)(acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP);
SEXP R_flint_acb_2ary(void (*)(acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_3ary(void (*)(acb_t, const acb_t, const acb_t, const acb_t, slong),
                      SEXP, SEXP, SEXP, SEXP, SEXP);

#endif /* ! defined (R_FLINT_ACB_TEMPLATE_H) */
