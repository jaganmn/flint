#include <flint/flint.h> /* flint_set_abort */
#include <R_ext/Rdynload.h> /* DL_FUNC, ... */
#include <R_ext/Visibility.h> /* attribute_visible */
#include <Rinternals.h> /* SEXP, ... */
#include <Rversion.h> /* R_VERSION */

#if R_VERSION < R_Version(4, 4, 0)
# define OBJSXP S4SXP
#endif /* < 4.4.0 */

SEXP R_flint_symbol_dot_xdata,
	R_flint_symbol_dot_data,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_off,
	R_flint_symbol_prec,
	R_flint_symbol_exp,
	R_flint_symbol_sign,
	R_flint_symbol_d;

SEXPTYPE R_flint_sexptypes[] =
{ OBJSXP, STRSXP, CPLXSXP, REALSXP, INTSXP, LGLSXP, RAWSXP, NILSXP };

const char *R_flint_classes[] =
{
	"slong", "ulong", "fmpz", "fmpq",
	"mag", "arf", "acf", "arb", "acb", "",
	NULL
};

const char *R_flint_ops2[] =
{
	/* Arith (7) : */
	"+", "-", "*", "%%", "%/%", "/", "^",
	/* Compare (6) : */
	"==", "!=", "<", ">", "<=", ">=",
	/* Logic (2) : */
	"&", "|",
	NULL
};

const char *R_flint_ops1[] =
{
	/* Arith (2) : */
	"+", "-",
	/* Compare (4) : */
	"is.na", "is.nan", "is.infinite", "is.finite",
	/* Logic (1) : */
	"!",
	/* Complex (5) : */
	"Conj", "Re", "Im", "Mod", "Arg",
	/* Math (35) : */
	"abs", "sign", "sqrt",
	"floor", "ceiling", "trunc",
	"cummin", "cummax", "cumsum", "cumprod",
	"log", "log10", "log2", "log1p", "exp", "expm1",
	"cos", "cospi", "acos", "cosh", "acosh",
	"sin", "sinpi", "asin", "sinh", "asinh",
	"tan", "tanpi", "atan", "tanh", "atanh",
	"gamma", "lgamma", "digamma", "trigamma",
	/* Math2 (2) : */
	"round", "signif",
	/* Summary (9) : */
	"min", "max", "range",
	"sum", "prod", "mean",
	"any", "all", "anyNA",
	NULL
};

SEXP R_flint_bind(SEXP);
SEXP R_flint_bits(void);
SEXP R_flint_class(SEXP);
SEXP R_flint_identical(SEXP, SEXP);
SEXP R_flint_length(SEXP);
SEXP R_flint_new(SEXP);
SEXP R_flint_realloc(SEXP, SEXP);
SEXP R_flint_rep_each(SEXP, SEXP);
SEXP R_flint_rep_lengthout(SEXP, SEXP);
SEXP R_flint_rep_times(SEXP, SEXP);
SEXP R_flint_size(SEXP);
SEXP R_flint_subassign(SEXP, SEXP, SEXP);
SEXP R_flint_subscript(SEXP, SEXP);
SEXP R_flint_triple(SEXP);
SEXP R_flint_valid(SEXP);
SEXP R_flint_version(void);

SEXP R_flint_slong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_slong_vector(SEXP);
SEXP R_flint_slong_format(SEXP, SEXP);

SEXP R_flint_ulong_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_ulong_vector(SEXP);
SEXP R_flint_ulong_format(SEXP, SEXP);

SEXP R_flint_fmpz_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_vector(SEXP);
SEXP R_flint_fmpz_format(SEXP, SEXP);
SEXP R_flint_fmpz_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_fmpq_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_part(SEXP, SEXP);
SEXP R_flint_fmpq_vector(SEXP);
SEXP R_flint_fmpq_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_mag_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_mag_vector(SEXP);
SEXP R_flint_mag_format(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_mag_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_mag_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arf_initialize(SEXP, SEXP, SEXP);
SEXP R_flint_arf_vector(SEXP);
SEXP R_flint_arf_format(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arf_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_arf_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_acf_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acf_part(SEXP, SEXP);
SEXP R_flint_acf_vector(SEXP);
SEXP R_flint_acf_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_acf_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_part(SEXP, SEXP);
SEXP R_flint_arb_vector(SEXP);
SEXP R_flint_arb_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_arb_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_acb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_part(SEXP, SEXP);
SEXP R_flint_acb_vector(SEXP);
SEXP R_flint_acb_ops2(SEXP, SEXP, SEXP);
SEXP R_flint_acb_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arb_lambertw(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_lambertw(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_dirichlet_zeta(SEXP, SEXP, SEXP);
SEXP R_flint_acb_dirichlet_zeta(SEXP, SEXP, SEXP);

SEXP R_flint_arb_dirichlet_hurwitz(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_dirichlet_hurwitz(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_gamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_gamma(SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_rgamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_rgamma(SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_lgamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_lgamma(SEXP, SEXP, SEXP);

SEXP R_flint_acb_hypgeom_polygamma(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
#define CALL_ENTRY(name, nargs) {#name, (DL_FUNC) &name, nargs}
	CALL_ENTRY(R_flint_bind, 1),
	CALL_ENTRY(R_flint_bits, 0),
	CALL_ENTRY(R_flint_class, 1),
	CALL_ENTRY(R_flint_identical, 2),
	CALL_ENTRY(R_flint_length, 1),
	CALL_ENTRY(R_flint_new, 1),
	CALL_ENTRY(R_flint_realloc, 2),
	CALL_ENTRY(R_flint_rep_each, 2),
	CALL_ENTRY(R_flint_rep_lengthout, 2),
	CALL_ENTRY(R_flint_rep_times, 2),
	CALL_ENTRY(R_flint_size, 1),
	CALL_ENTRY(R_flint_subassign, 3),
	CALL_ENTRY(R_flint_subscript, 2),
	CALL_ENTRY(R_flint_triple, 1),
	CALL_ENTRY(R_flint_valid, 1),
	CALL_ENTRY(R_flint_version, 0),
	CALL_ENTRY(R_flint_slong_initialize, 3),
	CALL_ENTRY(R_flint_slong_vector, 1),
	CALL_ENTRY(R_flint_slong_format, 2),
	CALL_ENTRY(R_flint_ulong_initialize, 3),
	CALL_ENTRY(R_flint_ulong_vector, 1),
	CALL_ENTRY(R_flint_ulong_format, 2),
	CALL_ENTRY(R_flint_fmpz_initialize, 3),
	CALL_ENTRY(R_flint_fmpz_vector, 1),
	CALL_ENTRY(R_flint_fmpz_format, 2),
	CALL_ENTRY(R_flint_fmpz_ops2, 3),
	CALL_ENTRY(R_flint_fmpz_ops1, 3),
	CALL_ENTRY(R_flint_fmpq_initialize, 5),
	CALL_ENTRY(R_flint_fmpq_part, 2),
	CALL_ENTRY(R_flint_fmpq_vector, 1),
	CALL_ENTRY(R_flint_fmpq_ops2, 3),
	CALL_ENTRY(R_flint_fmpq_ops1, 3),
	CALL_ENTRY(R_flint_mag_initialize, 3),
	CALL_ENTRY(R_flint_mag_vector, 1),
	CALL_ENTRY(R_flint_mag_format, 5),
	CALL_ENTRY(R_flint_mag_ops2, 3),
	CALL_ENTRY(R_flint_mag_ops1, 3),
	CALL_ENTRY(R_flint_arf_initialize, 3),
	CALL_ENTRY(R_flint_arf_vector, 1),
	CALL_ENTRY(R_flint_arf_format, 5),
	CALL_ENTRY(R_flint_arf_ops2, 3),
	CALL_ENTRY(R_flint_arf_ops1, 3),
	CALL_ENTRY(R_flint_acf_initialize, 5),
	CALL_ENTRY(R_flint_acf_part, 2),
	CALL_ENTRY(R_flint_acf_vector, 1),
	CALL_ENTRY(R_flint_acf_ops2, 3),
	CALL_ENTRY(R_flint_acf_ops1, 3),
	CALL_ENTRY(R_flint_arb_initialize, 5),
	CALL_ENTRY(R_flint_arb_part, 2),
	CALL_ENTRY(R_flint_arb_vector, 1),
	CALL_ENTRY(R_flint_arb_ops2, 3),
	CALL_ENTRY(R_flint_arb_ops1, 3),
	CALL_ENTRY(R_flint_acb_initialize, 5),
	CALL_ENTRY(R_flint_acb_part, 2),
	CALL_ENTRY(R_flint_acb_vector, 1),
	CALL_ENTRY(R_flint_acb_ops2, 3),
	CALL_ENTRY(R_flint_acb_ops1, 3),
	CALL_ENTRY(R_flint_arb_lambertw, 4),
	CALL_ENTRY(R_flint_acb_lambertw, 5),
	CALL_ENTRY(R_flint_arb_dirichlet_zeta, 3),
	CALL_ENTRY(R_flint_acb_dirichlet_zeta, 3),
	CALL_ENTRY(R_flint_arb_dirichlet_hurwitz, 4),
	CALL_ENTRY(R_flint_acb_dirichlet_hurwitz, 4),
	CALL_ENTRY(R_flint_acb_dirichlet_lerch_phi, 5),
	CALL_ENTRY(R_flint_arb_hypgeom_gamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_gamma, 3),
	CALL_ENTRY(R_flint_arb_hypgeom_rgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_rgamma, 3),
	CALL_ENTRY(R_flint_arb_hypgeom_lgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_lgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_polygamma, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_2f1, 7),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1, 7),
	{NULL, NULL, 0}
};

void attribute_visible R_init_flint(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
	R_flint_symbol_dot_xdata = Rf_install(".xData");
	R_flint_symbol_dot_data  = Rf_install(".Data");
	R_flint_symbol_num       = Rf_install("num");
	R_flint_symbol_den       = Rf_install("den");
	R_flint_symbol_mid       = Rf_install("mid");
	R_flint_symbol_rad       = Rf_install("rad");
	R_flint_symbol_real      = Rf_install("real");
	R_flint_symbol_imag      = Rf_install("imag");
	R_flint_symbol_off       = Rf_install("off");
	R_flint_symbol_prec      = Rf_install("prec");
	R_flint_symbol_exp       = Rf_install("exp");
	R_flint_symbol_sign      = Rf_install("sign");
	R_flint_symbol_d         = Rf_install("d");
	void R_flint_abort(void);
	flint_set_abort(&R_flint_abort);
	return;
}
