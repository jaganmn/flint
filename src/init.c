#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rinternals.h>

SEXP R_flint_symbol_length,
	R_flint_symbol_x,
	R_flint_symbol_dot_data,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_prec,
	R_flint_symbol_exp,
	R_flint_symbol_sign,
	R_flint_symbol_d;

SEXPTYPE R_flint_sexptypes[] =
{ CPLXSXP, REALSXP, INTSXP, LGLSXP, RAWSXP, NILSXP };

const char *R_flint_classes[] =
{ "slong", "ulong", "fmpz", "fmpq", "arf", "mag", "arb", "acb", "" };

SEXP R_flint_bits(void);
SEXP R_flint_class(SEXP);
SEXP R_flint_new(SEXP);
SEXP R_flint_valid(SEXP);
SEXP R_flint_length(SEXP);

SEXP R_flint_slong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_slong_nflint(SEXP);
SEXP R_flint_slong_vector(SEXP);

SEXP R_flint_ulong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_ulong_nflint(SEXP);
SEXP R_flint_ulong_vector(SEXP);

SEXP R_flint_fmpz_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_nflint(SEXP);
SEXP R_flint_fmpz_vector(SEXP);

SEXP R_flint_fmpq_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_nflint(SEXP);
SEXP R_flint_fmpq_vector(SEXP);

SEXP R_flint_arf_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_arf_nflint(SEXP, SEXP);
SEXP R_flint_arf_vector(SEXP, SEXP);

SEXP R_flint_mag_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_mag_nflint(SEXP);
SEXP R_flint_mag_vector(SEXP);

SEXP R_flint_arb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_nflint(SEXP, SEXP);
SEXP R_flint_arb_vector(SEXP, SEXP);

SEXP R_flint_acb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_nflint(SEXP, SEXP);
SEXP R_flint_acb_vector(SEXP, SEXP);

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1_continuation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
#define CALL_ENTRY(name, nargs) {#name, (DL_FUNC) &name, nargs}
	CALL_ENTRY(R_flint_bits, 0),
	CALL_ENTRY(R_flint_class, 1),
	CALL_ENTRY(R_flint_new, 1),
	CALL_ENTRY(R_flint_valid, 1),
	CALL_ENTRY(R_flint_length, 1),
	CALL_ENTRY(R_flint_slong_initialize, 3),
	CALL_ENTRY(R_flint_slong_nflint, 1),
	CALL_ENTRY(R_flint_slong_vector, 1),
	CALL_ENTRY(R_flint_ulong_initialize, 3),
	CALL_ENTRY(R_flint_ulong_nflint, 1),
	CALL_ENTRY(R_flint_ulong_vector, 1),
	CALL_ENTRY(R_flint_fmpz_initialize, 3),
	CALL_ENTRY(R_flint_fmpz_nflint, 1),
	CALL_ENTRY(R_flint_fmpz_vector, 1),
	CALL_ENTRY(R_flint_fmpq_initialize, 5),
	CALL_ENTRY(R_flint_fmpq_nflint, 1),
	CALL_ENTRY(R_flint_fmpq_vector, 1),
	CALL_ENTRY(R_flint_arf_initialize, 3),
	CALL_ENTRY(R_flint_arf_nflint, 2),
	CALL_ENTRY(R_flint_arf_vector, 2),
	CALL_ENTRY(R_flint_mag_initialize, 3),
	CALL_ENTRY(R_flint_mag_nflint, 1),
	CALL_ENTRY(R_flint_mag_vector, 1),
	CALL_ENTRY(R_flint_arb_initialize, 5),
	CALL_ENTRY(R_flint_arb_nflint, 2),
	CALL_ENTRY(R_flint_arb_vector, 2),
	CALL_ENTRY(R_flint_acb_initialize, 7),
	CALL_ENTRY(R_flint_acb_nflint, 2),
	CALL_ENTRY(R_flint_acb_vector, 2),
	CALL_ENTRY(R_flint_acb_dirichlet_lerch_phi, 5),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1, 7),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1_continuation, 10),
	{NULL, NULL, 0}
};

void attribute_visible R_init_flint(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	/* R_forceSymbols(info, TRUE); */
	R_flint_symbol_length   = Rf_install("length");
	R_flint_symbol_x        = Rf_install("x");
	R_flint_symbol_dot_data = Rf_install(".Data");
	R_flint_symbol_num      = Rf_install("num");
	R_flint_symbol_den      = Rf_install("den");
	R_flint_symbol_mid      = Rf_install("mid");
	R_flint_symbol_rad      = Rf_install("rad");
	R_flint_symbol_real     = Rf_install("real");
	R_flint_symbol_imag     = Rf_install("imag");
	R_flint_symbol_prec     = Rf_install("prec");
	R_flint_symbol_exp      = Rf_install("exp");
	R_flint_symbol_sign     = Rf_install("sign");
	R_flint_symbol_d        = Rf_install("d");
	return;
}
