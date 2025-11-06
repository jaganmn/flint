#include "flint.h"
#include <flint/arb_calc.h>
#include <flint/acb_calc.h>

static int R_flint_acb_calc_integrate_integrand(acb_ptr res, const acb_ptr z, void *param, slong order, slong prec)
{
	SEXP call = param;
	acb_set(R_flint_get_pointer(CADR(call)), z);
	INTEGER(CADDDR(call))[0] = (int) order;
	SEXP value = Rf_eval(call, R_ClosureEnv(CAR(call)));
	if (R_flint_get_class(value) != R_FLINT_CLASS_ACB)
		Rf_error(_("class of integrand value is not \"%s\""), "acb");
	if (R_flint_get_length(value) != 1)
		Rf_error(_("length of integrand value is not %d"), 1);
	acb_set(res, R_flint_get_pointer(value));
	return 0;
}

SEXP R_flint_acb_calc_integrate(SEXP s_res, SEXP s_func, SEXP s_param, SEXP s_a, SEXP s_b, SEXP s_rel_goal, SEXP s_abs_tol, SEXP s_options, SEXP s_prec)
{
	slong prec = asPrec(s_prec, __func__);
	if (R_flint_get_length(s_a) != 1)
		Rf_error(_("length of '%s' is not %d"), "a", 1);
	if (R_flint_get_length(s_b) != 1)
		Rf_error(_("length of '%s' is not %d"), "b", 1);
	if (s_rel_goal != R_NilValue &&
	    R_flint_get_length(s_rel_goal) != 1)
		Rf_error(_("length of '%s' is not %d"), "rel.goal", 1);
	if (s_abs_tol != R_NilValue &&
	    R_flint_get_length(s_abs_tol) != 1)
		Rf_error(_("length of '%s' is not %d"), "abs.tol", 1);

	/* func(z, param, order, prec) */
	SEXP s_a0 = s_func;
	SEXP s_a1 = PROTECT(newObject("acb"));
	acb_ptr a1 = flint_calloc(1, sizeof(acb_t));
	R_flint_set(s_a1, a1, 1, (R_CFinalizer_t) &R_flint_acb_finalize);
	SEXP s_a2 = PROTECT(Rf_allocVector(INTSXP, 1));
	SEXP s_a3 = s_param;
	SEXP s_a4 = PROTECT(newObject("slong"));
	slong *a4 = flint_calloc(1, sizeof(slong));
	R_flint_set(s_a4, a4, 1, (R_CFinalizer_t) &R_flint_slong_finalize);
	a4[0] = prec;

	acb_calc_func_t func = (acb_calc_func_t) &R_flint_acb_calc_integrate_integrand;
	void *param = PROTECT(Rf_lang5(s_a0, s_a1, s_a2, s_a3, s_a4));
	acb_ptr a = R_flint_get_pointer(s_a);
	acb_ptr b = R_flint_get_pointer(s_b);
	slong rel_goal;
	mag_ptr abs_tol;
	acb_calc_integrate_opt_t options;
	acb_calc_integrate_opt_init(options);

	if (s_rel_goal != R_NilValue)
		rel_goal = ((slong *) R_flint_get_pointer(s_rel_goal))[0];
	else
		rel_goal = prec;

	if (s_abs_tol != R_NilValue)
		abs_tol = R_flint_get_pointer(s_abs_tol);
	else {
		s_abs_tol = newObject("mag");
		abs_tol = flint_calloc(1, sizeof(mag_t));
		R_flint_set(s_abs_tol, abs_tol, 1, (R_CFinalizer_t) &R_flint_mag_finalize);
		mag_set_ui_2exp_si(abs_tol, 1, -prec);
	}
	PROTECT(s_abs_tol);

	if (s_options != R_NilValue) {
		const slong *t;
		t = R_flint_get_pointer(VECTOR_ELT(s_options, 0));
		options->   deg_limit = t[0];
		t = R_flint_get_pointer(VECTOR_ELT(s_options, 1));
		options->  eval_limit = t[0];
		t = R_flint_get_pointer(VECTOR_ELT(s_options, 2));
		options-> depth_limit = t[0];
	}
	if (s_options != R_NilValue) {
		const int *t;
		t = INTEGER_RO(VECTOR_ELT(s_options, 3));
		options->use_heap = t[0];
		t = INTEGER_RO(VECTOR_ELT(s_options, 4));
		options-> verbose = t[0];
	}

	acb_ptr res = flint_calloc(1, sizeof(acb_t));
	R_flint_set(s_res, res, 1, (R_CFinalizer_t) &R_flint_acb_finalize);

	int status = acb_calc_integrate(res, func, param, a, b, rel_goal, abs_tol, options, prec);
	if (status == ARB_CALC_NO_CONVERGENCE)
		Rf_warning(_("target accuracy not reached on all subintervals"));

	UNPROTECT(4);
	return R_NilValue;
}
