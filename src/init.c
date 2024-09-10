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

const char *R_flint_classes[] =
{ "slong", "ulong", "fmpz", "fmpq", "arf", "mag", "arb", "acb", "" };

SEXP R_flint_bits(void);
SEXP R_flint_length(SEXP);
SEXP R_flint_class(SEXP);

SEXP R_flint_slong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_slong_nslong(SEXP);
SEXP R_flint_slong_double(SEXP);

SEXP R_flint_ulong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_ulong_nulong(SEXP);
SEXP R_flint_ulong_double(SEXP);

SEXP R_flint_fmpz_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_nfmpz(SEXP);
SEXP R_flint_fmpz_double(SEXP);

SEXP R_flint_fmpq_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_nfmpq(SEXP);
SEXP R_flint_fmpq_double(SEXP);

SEXP R_flint_arf_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_arf_narf(SEXP, SEXP);

SEXP R_flint_mag_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_mag_nmag(SEXP);

SEXP R_flint_arb_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_arb_narb(SEXP, SEXP);

SEXP R_flint_acb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_nacb(SEXP, SEXP);
SEXP R_flint_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1_continuation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
#define CALL_ENTRY(name, nargs) {#name, (DL_FUNC) &name, nargs}
	CALL_ENTRY(R_flint_bits, 0),
	CALL_ENTRY(R_flint_length, 1),
	CALL_ENTRY(R_flint_class, 1),
	CALL_ENTRY(R_flint_slong_initialize, 3),
	CALL_ENTRY(R_flint_slong_nslong, 1),
	CALL_ENTRY(R_flint_slong_double, 1),
	CALL_ENTRY(R_flint_ulong_initialize, 3),
	CALL_ENTRY(R_flint_ulong_nulong, 1),
	CALL_ENTRY(R_flint_ulong_double, 1),
	CALL_ENTRY(R_flint_fmpz_initialize, 3),
	CALL_ENTRY(R_flint_fmpz_nfmpz, 1),
	CALL_ENTRY(R_flint_fmpz_double, 1),
	CALL_ENTRY(R_flint_fmpq_initialize, 5),
	CALL_ENTRY(R_flint_fmpq_nfmpq, 1),
	CALL_ENTRY(R_flint_fmpq_double, 1),
	CALL_ENTRY(R_flint_arf_initialize, 3),
	CALL_ENTRY(R_flint_arf_narf, 2),
	CALL_ENTRY(R_flint_mag_initialize, 3),
	CALL_ENTRY(R_flint_mag_nmag, 1),
	CALL_ENTRY(R_flint_arb_initialize, 3),
	CALL_ENTRY(R_flint_arb_narb, 2),
	CALL_ENTRY(R_flint_acb_initialize, 5),
	CALL_ENTRY(R_flint_acb_nacb, 2),
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
	R_flint_symbol_length   = install("length");
	R_flint_symbol_x        = install("x");
	R_flint_symbol_dot_data = install(".Data");
	R_flint_symbol_num      = install("num");
	R_flint_symbol_den      = install("den");
	R_flint_symbol_mid      = install("mid");
	R_flint_symbol_rad      = install("rad");
	R_flint_symbol_real     = install("real");
	R_flint_symbol_imag     = install("imag");
	R_flint_symbol_prec     = install("prec");
	R_flint_symbol_exp      = install("exp");
	R_flint_symbol_sign     = install("sign");
	R_flint_symbol_d        = install("d");
	return;
}
