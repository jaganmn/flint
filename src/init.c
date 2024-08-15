#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

SEXP mpfr_precSymbol, mpfr_expSymbol, mpfr_signSymbol, mpfr_dSymbol;
SEXP R_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
	{"R_acb_dirichlet_lerch_phi", (DL_FUNC) &R_acb_dirichlet_lerch_phi, 4},
	{NULL, NULL, 0}
};

void attribute_visible R_init_flint(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
	mpfr_precSymbol = install("prec");
	mpfr_expSymbol = install("exp");
	mpfr_signSymbol = install("sign");
	mpfr_dSymbol = install("d");	
	return;
}
