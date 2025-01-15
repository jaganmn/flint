#include <gmp.h>
#include <flint/arb.h>
#include "flint.h"
#include "arb_template.h"

SEXP R_flint_arb_const_pi(SEXP s_res, SEXP s_prec)
{
	return R_flint_arb_0ary(&arb_const_pi, s_res, s_prec);
}

SEXP R_flint_arb_const_log2(SEXP s_res, SEXP s_prec)
{
	return R_flint_arb_0ary(&arb_const_log2, s_res, s_prec);
}

SEXP R_flint_arb_const_log10(SEXP s_res, SEXP s_prec)
{
	return R_flint_arb_0ary(&arb_const_log10, s_res, s_prec);
}

SEXP R_flint_arb_const_e(SEXP s_res, SEXP s_prec)
{
	return R_flint_arb_0ary(&arb_const_e, s_res, s_prec);
}
