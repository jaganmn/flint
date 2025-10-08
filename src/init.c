#include "flint.h"

SEXP R_flint_symbol_missing,
	R_flint_symbol_dot_data,
	R_flint_symbol_dot_xdata,
	R_flint_symbol_dim,
	R_flint_symbol_dimnames,
	R_flint_symbol_names,
	R_flint_symbol_num,
	R_flint_symbol_den,
	R_flint_symbol_mid,
	R_flint_symbol_rad,
	R_flint_symbol_real,
	R_flint_symbol_imag,
	R_flint_symbol_off;

SEXPTYPE R_flint_sexptypes[] =
{ OBJSXP, STRSXP, CPLXSXP, REALSXP, INTSXP, LGLSXP, RAWSXP, NILSXP };

const char *R_flint_classes[] =
{
	"ulong", "slong", "fmpz", "fmpq",
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
	/* matrixOps (6) : */
	"%*%", "crossprod", "tcrossprod",
	"solve", "backsolve", "tbacksolve",
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
	/* Summary (14) : */
	"min", "max", "range",
	"sum", "prod", "mean",
	"any", "all", "anyNA", "is.unsorted",
	"colSums", "rowSums", "colMeans", "rowMeans",
	/* matrixOps (8) : */
	"solve", "backsolve", "tbacksolve", "chol2inv", "chol", "det",
	NULL
};

SEXP R_flint_abi(void);
SEXP R_flint_aperm(SEXP, SEXP, SEXP);
SEXP R_flint_asplit(SEXP, SEXP, SEXP);
SEXP R_flint_bind(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_bits(SEXP);
SEXP R_flint_bits_accurate(SEXP);
SEXP R_flint_bits_per_limb(void);
SEXP R_flint_class(SEXP);
SEXP R_flint_diag(SEXP, SEXP, SEXP);
SEXP R_flint_identical(SEXP, SEXP);
SEXP R_flint_find_interval(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_length(SEXP, SEXP);
SEXP R_flint_length_assign(SEXP, SEXP);
SEXP R_flint_list(SEXP, SEXP);
SEXP R_flint_long_long_limb(void);
SEXP R_flint_new(SEXP);
SEXP R_flint_rep_each(SEXP, SEXP, SEXP);
SEXP R_flint_rep_lengthout(SEXP, SEXP, SEXP);
SEXP R_flint_rep_times(SEXP, SEXP, SEXP);
SEXP R_flint_size(SEXP);
SEXP R_flint_subassign(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_subscript(SEXP, SEXP, SEXP);
SEXP R_flint_transpose(SEXP, SEXP);
SEXP R_flint_triple(SEXP);
SEXP R_flint_valid(SEXP);
SEXP R_flint_version(void);

SEXP R_flint_coerce_bigz_fmpz(SEXP);
SEXP R_flint_coerce_fmpz_bigz(SEXP);
SEXP R_flint_coerce_bigq_fmpq(SEXP);
SEXP R_flint_coerce_fmpq_bigq(SEXP);
SEXP R_flint_coerce_mpfr_arf(SEXP);
SEXP R_flint_coerce_arf_mpfr(SEXP);

SEXP R_flint_ulong_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_ulong_atomic(SEXP);
SEXP R_flint_ulong_format(SEXP, SEXP);
SEXP R_flint_ulong_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_ulong_ops1(SEXP, SEXP, SEXP);
SEXP R_flint_ulong_seq(SEXP, SEXP, SEXP);
SEXP R_flint_ulong_complement(SEXP, SEXP, SEXP);

SEXP R_flint_slong_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_slong_atomic(SEXP);
SEXP R_flint_slong_format(SEXP, SEXP);
SEXP R_flint_slong_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_slong_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_fmpz_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_atomic(SEXP);
SEXP R_flint_fmpz_format(SEXP, SEXP);
SEXP R_flint_fmpz_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpz_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_fmpq_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_part(SEXP, SEXP);
SEXP R_flint_fmpq_atomic(SEXP);
SEXP R_flint_fmpq_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_fmpq_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_mag_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_mag_atomic(SEXP);
SEXP R_flint_mag_format(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_mag_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_mag_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arf_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arf_atomic(SEXP);
SEXP R_flint_arf_format(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arf_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arf_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_acf_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acf_part(SEXP, SEXP);
SEXP R_flint_acf_atomic(SEXP);
SEXP R_flint_acf_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acf_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_part(SEXP, SEXP);
SEXP R_flint_arb_atomic(SEXP);
SEXP R_flint_arb_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_arb_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_acb_initialize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_part(SEXP, SEXP);
SEXP R_flint_acb_atomic(SEXP);
SEXP R_flint_acb_ops2(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_ops1(SEXP, SEXP, SEXP);

SEXP R_flint_arb_const_pi(SEXP, SEXP);
SEXP R_flint_arb_const_log2(SEXP, SEXP);
SEXP R_flint_arb_const_log10(SEXP, SEXP);
SEXP R_flint_arb_const_e(SEXP, SEXP);

SEXP R_flint_arb_lambertw(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_lambertw(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_dirichlet_zeta(SEXP, SEXP, SEXP);
SEXP R_flint_acb_dirichlet_zeta(SEXP, SEXP, SEXP);

SEXP R_flint_arb_dirichlet_hurwitz(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_dirichlet_hurwitz(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_acb_dirichlet_lerch_phi(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_bessel_j(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_bessel_j(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_bessel_y(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_bessel_y(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_bessel_i(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_bessel_i(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_bessel_k(SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_bessel_k(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_gamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_gamma(SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_rgamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_rgamma(SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_lgamma(SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_lgamma(SEXP, SEXP, SEXP);

SEXP R_flint_acb_hypgeom_polygamma(SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_gamma_lower(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_gamma_lower(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_gamma_upper(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_gamma_upper(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_beta_lower(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_beta_lower(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP R_flint_arb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_flint_acb_hypgeom_2f1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static R_CallMethodDef CallEntries[] =
{
#define CALL_ENTRY(name, nargs) {#name, (DL_FUNC) &name, nargs}
	CALL_ENTRY(R_flint_abi, 0),
	CALL_ENTRY(R_flint_aperm, 3),
	CALL_ENTRY(R_flint_asplit, 3),
	CALL_ENTRY(R_flint_bind, 4),
	CALL_ENTRY(R_flint_bits, 1),
	CALL_ENTRY(R_flint_bits_accurate, 1),
	CALL_ENTRY(R_flint_bits_per_limb, 0),
	CALL_ENTRY(R_flint_class, 1),
	CALL_ENTRY(R_flint_diag, 3),
	CALL_ENTRY(R_flint_find_interval, 5),
	CALL_ENTRY(R_flint_identical, 2),
	CALL_ENTRY(R_flint_length, 2),
	CALL_ENTRY(R_flint_length_assign, 2),
	CALL_ENTRY(R_flint_list, 2),
	CALL_ENTRY(R_flint_long_long_limb, 0),
	CALL_ENTRY(R_flint_new, 1),
	CALL_ENTRY(R_flint_rep_each, 3),
	CALL_ENTRY(R_flint_rep_lengthout, 3),
	CALL_ENTRY(R_flint_rep_times, 3),
	CALL_ENTRY(R_flint_size, 1),
	CALL_ENTRY(R_flint_subassign, 4),
	CALL_ENTRY(R_flint_subscript, 3),
	CALL_ENTRY(R_flint_triple, 1),
	CALL_ENTRY(R_flint_transpose, 2),
	CALL_ENTRY(R_flint_valid, 1),
	CALL_ENTRY(R_flint_version, 0),
	CALL_ENTRY(R_flint_coerce_bigz_fmpz, 1),
	CALL_ENTRY(R_flint_coerce_fmpz_bigz, 1),
	CALL_ENTRY(R_flint_coerce_bigq_fmpq, 1),
	CALL_ENTRY(R_flint_coerce_fmpq_bigq, 1),
	CALL_ENTRY(R_flint_coerce_mpfr_arf, 1),
	CALL_ENTRY(R_flint_coerce_arf_mpfr, 1),
	CALL_ENTRY(R_flint_ulong_initialize, 6),
	CALL_ENTRY(R_flint_ulong_atomic, 1),
	CALL_ENTRY(R_flint_ulong_format, 2),
	CALL_ENTRY(R_flint_ulong_ops2, 4),
	CALL_ENTRY(R_flint_ulong_ops1, 3),
	CALL_ENTRY(R_flint_ulong_seq, 3),
	CALL_ENTRY(R_flint_ulong_complement, 3),
	CALL_ENTRY(R_flint_slong_initialize, 6),
	CALL_ENTRY(R_flint_slong_atomic, 1),
	CALL_ENTRY(R_flint_slong_format, 2),
	CALL_ENTRY(R_flint_slong_ops2, 4),
	CALL_ENTRY(R_flint_slong_ops1, 3),
	CALL_ENTRY(R_flint_fmpz_initialize, 6),
	CALL_ENTRY(R_flint_fmpz_atomic, 1),
	CALL_ENTRY(R_flint_fmpz_format, 2),
	CALL_ENTRY(R_flint_fmpz_ops2, 4),
	CALL_ENTRY(R_flint_fmpz_ops1, 3),
	CALL_ENTRY(R_flint_fmpq_initialize, 8),
	CALL_ENTRY(R_flint_fmpq_part, 2),
	CALL_ENTRY(R_flint_fmpq_atomic, 1),
	CALL_ENTRY(R_flint_fmpq_ops2, 4),
	CALL_ENTRY(R_flint_fmpq_ops1, 3),
	CALL_ENTRY(R_flint_mag_initialize, 6),
	CALL_ENTRY(R_flint_mag_atomic, 1),
	CALL_ENTRY(R_flint_mag_format, 5),
	CALL_ENTRY(R_flint_mag_ops2, 4),
	CALL_ENTRY(R_flint_mag_ops1, 3),
	CALL_ENTRY(R_flint_arf_initialize, 6),
	CALL_ENTRY(R_flint_arf_atomic, 1),
	CALL_ENTRY(R_flint_arf_format, 5),
	CALL_ENTRY(R_flint_arf_ops2, 4),
	CALL_ENTRY(R_flint_arf_ops1, 3),
	CALL_ENTRY(R_flint_acf_initialize, 8),
	CALL_ENTRY(R_flint_acf_part, 2),
	CALL_ENTRY(R_flint_acf_atomic, 1),
	CALL_ENTRY(R_flint_acf_ops2, 4),
	CALL_ENTRY(R_flint_acf_ops1, 3),
	CALL_ENTRY(R_flint_arb_initialize, 8),
	CALL_ENTRY(R_flint_arb_part, 2),
	CALL_ENTRY(R_flint_arb_atomic, 1),
	CALL_ENTRY(R_flint_arb_ops2, 4),
	CALL_ENTRY(R_flint_arb_ops1, 3),
	CALL_ENTRY(R_flint_acb_initialize, 8),
	CALL_ENTRY(R_flint_acb_part, 2),
	CALL_ENTRY(R_flint_acb_atomic, 1),
	CALL_ENTRY(R_flint_acb_ops2, 4),
	CALL_ENTRY(R_flint_acb_ops1, 3),
	CALL_ENTRY(R_flint_arb_const_pi, 2),
	CALL_ENTRY(R_flint_arb_const_log2, 2),
	CALL_ENTRY(R_flint_arb_const_log10, 2),
	CALL_ENTRY(R_flint_arb_const_e, 2),
	CALL_ENTRY(R_flint_arb_lambertw, 4),
	CALL_ENTRY(R_flint_acb_lambertw, 5),
	CALL_ENTRY(R_flint_arb_dirichlet_zeta, 3),
	CALL_ENTRY(R_flint_acb_dirichlet_zeta, 3),
	CALL_ENTRY(R_flint_arb_dirichlet_hurwitz, 4),
	CALL_ENTRY(R_flint_acb_dirichlet_hurwitz, 4),
	CALL_ENTRY(R_flint_acb_dirichlet_lerch_phi, 5),
	CALL_ENTRY(R_flint_arb_hypgeom_bessel_j, 4),
	CALL_ENTRY(R_flint_acb_hypgeom_bessel_j, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_bessel_y, 4),
	CALL_ENTRY(R_flint_acb_hypgeom_bessel_y, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_bessel_i, 4),
	CALL_ENTRY(R_flint_acb_hypgeom_bessel_i, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_bessel_k, 4),
	CALL_ENTRY(R_flint_acb_hypgeom_bessel_k, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_gamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_gamma, 3),
	CALL_ENTRY(R_flint_arb_hypgeom_rgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_rgamma, 3),
	CALL_ENTRY(R_flint_arb_hypgeom_lgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_lgamma, 3),
	CALL_ENTRY(R_flint_acb_hypgeom_polygamma, 4),
	CALL_ENTRY(R_flint_arb_hypgeom_gamma_lower, 5),
	CALL_ENTRY(R_flint_acb_hypgeom_gamma_lower, 5),
	CALL_ENTRY(R_flint_arb_hypgeom_gamma_upper, 5),
	CALL_ENTRY(R_flint_acb_hypgeom_gamma_upper, 5),
	CALL_ENTRY(R_flint_arb_hypgeom_beta_lower, 6),
	CALL_ENTRY(R_flint_acb_hypgeom_beta_lower, 6),
	CALL_ENTRY(R_flint_arb_hypgeom_2f1, 7),
	CALL_ENTRY(R_flint_acb_hypgeom_2f1, 7),
	{NULL, NULL, 0}
};

void attribute_visible R_init_flint(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
	R_flint_symbol_missing   = Rf_install(".__WAS_MISSING__.");
	R_flint_symbol_dot_data  = Rf_install(".Data");
	R_flint_symbol_dot_xdata = Rf_install(".xData");
	R_flint_symbol_dim       = Rf_install("dim");
	R_flint_symbol_dimnames  = Rf_install("dimnames");
	R_flint_symbol_names     = Rf_install("names");
	R_flint_symbol_num       = Rf_install("num");
	R_flint_symbol_den       = Rf_install("den");
	R_flint_symbol_mid       = Rf_install("mid");
	R_flint_symbol_rad       = Rf_install("rad");
	R_flint_symbol_real      = Rf_install("real");
	R_flint_symbol_imag      = Rf_install("imag");
	R_flint_symbol_off       = Rf_install("off");
#if __FLINT_RELEASE < 30100
	FLINT_NORETURN
#endif
	void R_flint_abort(void);
	flint_set_abort(&R_flint_abort);
	return;
}
