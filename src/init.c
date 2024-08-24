#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rinternals.h>

SEXP R_flint_symbol_prec, R_flint_symbol_exp, R_flint_symbol_sign,
	R_flint_symbol_d, R_flint_symbol_length, R_flint_symbol_x;

SEXP R_flint_length_get(SEXP);

SEXP R_flint_fmpz_initialize(SEXP, SEXP);

SEXP R_flint_fmpq_initialize(SEXP, SEXP, SEXP);

SEXP R_flint_mag_initialize(SEXP, SEXP);

SEXP R_flint_arf_initialize(SEXP, SEXP);

SEXP R_flint_arb_initialize(SEXP, SEXP);

SEXP R_flint_acb_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1_continuation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
#define CALL_ENTRY(name, nargs) {#name, (DL_FUNC) &name, nargs}
	CALL_ENTRY(R_flint_length_get, 1),
	CALL_ENTRY(R_flint_fmpz_initialize, 2),
	CALL_ENTRY(R_flint_fmpq_initialize, 3),
	CALL_ENTRY(R_flint_mag_initialize, 2),
	CALL_ENTRY(R_flint_arf_initialize, 2),
	CALL_ENTRY(R_flint_arb_initialize, 2),
	CALL_ENTRY(R_flint_acb_initialize, 3),
	CALL_ENTRY(R_flint_acb_dirichlet_lerch_phi, 5),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1, 7),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1_continuation, 10),
	{NULL, NULL, 0}
};

void attribute_visible R_init_flint(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
	R_flint_symbol_prec   = install("prec");
	R_flint_symbol_exp    = install("exp");
	R_flint_symbol_sign   = install("sign");
	R_flint_symbol_d      = install("d");
	R_flint_symbol_length = install("length");
	R_flint_symbol_x      = install("x");
	return;
}
