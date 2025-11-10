#include "flint.h"
#include <flint/arb_calc.h>
#include <flint/acb_calc.h>

static int integrate_integrand(acb_ptr res, const acb_ptr z, void *param, slong order, slong prec)
{
	SEXP call = param;
	arb_set(R_flint_get_pointer(CADR(call)), acb_realref(z));
	INTEGER(CADDDR(call))[0] = (int) order;
	SEXP value = Rf_eval(call, R_ClosureEnv(CAR(call)));
	if (R_flint_get_class(value) != R_FLINT_CLASS_ARB)
		Rf_error(_("class of value of '%s' call is not \"%s\""),
		         "func", "arb");
	if (R_flint_get_length(value) != 1)
		Rf_error(_("length of value of '%s' call is not %d"),
		         "func", 1);
	arb_set(acb_realref(res), R_flint_get_pointer(value));
	arb_zero(acb_imagref(res));
	return 0;
}

SEXP R_flint_arb_calc_integrate(SEXP s_res, SEXP s_func, SEXP s_param, SEXP s_a, SEXP s_b, SEXP s_rel_goal, SEXP s_abs_tol, SEXP s_options, SEXP s_prec)
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

	SEXP s_work = PROTECT(newFlint(R_FLINT_CLASS_ACB, 0, 3));
	acb_ptr work = R_flint_get_pointer(s_work);

	/* R: func(x, param, order, prec) */
	SEXP s_a0 = s_func;
	SEXP s_a1 = PROTECT(newFlint(R_FLINT_CLASS_ARB, 0, 1));
	SEXP s_a2 = PROTECT(Rf_allocVector(INTSXP, 1));
	SEXP s_a3 = s_param;
	SEXP s_a4 = PROTECT(newFlint(R_FLINT_CLASS_SLONG, 0, 1));
	SEXP call = PROTECT(Rf_lang5(s_a0, s_a1, s_a2, s_a3, s_a4));

	((slong *) R_flint_get_pointer(s_a4))[0] = prec;

	/* C: acb_calc_integrate(res, func, param, a, b, rel_goal, abs_tol, options, prec) */
	acb_calc_func_t func = (acb_calc_func_t) &integrate_integrand;
	void *param = call;
	acb_ptr a = work + 1;
	acb_ptr b = work + 2;
	slong rel_goal;
	mag_ptr abs_tol;
	acb_calc_integrate_opt_t options;
	acb_calc_integrate_opt_init(options);

	arb_set(acb_realref(a), R_flint_get_pointer(s_a));
	arb_set(acb_realref(b), R_flint_get_pointer(s_b));
	arb_zero(acb_imagref(a));
	arb_zero(acb_imagref(b));

	if (s_rel_goal == R_NilValue)
		rel_goal = prec;
	else
		rel_goal = ((slong *) R_flint_get_pointer(s_rel_goal))[0];

	if (s_abs_tol == R_NilValue) {
		s_abs_tol = newFlint(R_FLINT_CLASS_MAG, 0, 1);
		abs_tol = R_flint_get_pointer(s_abs_tol);
		mag_set_ui_2exp_si(abs_tol, 1, -prec);
	}
	else
		abs_tol = R_flint_get_pointer(s_abs_tol);
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

	int status = acb_calc_integrate(work, func, param, a, b, rel_goal, abs_tol, options, prec);
	if (status == ARB_CALC_NO_CONVERGENCE)
		Rf_warning(_("target accuracy not reached on all subintervals"));

	arb_ptr res = flint_calloc(1, sizeof(arb_t));
	R_flint_set(s_res, res, 1, (R_CFinalizer_t) &R_flint_arb_finalize);
	arb_set(res, acb_realref(work));

	UNPROTECT(6);
	return R_NilValue;
}
